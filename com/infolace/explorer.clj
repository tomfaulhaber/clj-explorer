(ns com.infolace.explorer
  ;; TODO - document
  ;; TODO - make sure the import set is minimal
  (:import (javax.swing JButton JFrame JLabel JList JPanel
                        JScrollPane JTabbedPane JTextField JSeparator
                        JTextArea JSpinner SpinnerNumberModel)
           (javax.swing.event ChangeListener)
           (java.util.concurrent LinkedBlockingQueue)
           (clojure.contrib.pprint PrettyWriter))
  (:use clojure.contrib.miglayout
        clojure.xml
        [clojure.contrib.pprint :only (write write-out cl-format *print-right-margin*)]
        [clojure.contrib.pprint.utilities :only (prlabel)]))

(defn make-frame [title panel-fn]
  (let [[panel text-area] (panel-fn (JPanel.))] 
    (doto (JFrame. title)
     (.add panel)
     (.pack)
     (.setVisible true))
    text-area))

(def write-opts (ref {:level 0, :length 0}))
(def current-object (ref nil))
(def block-tree (ref nil))
(def path-set (ref #()))
(def update-from-ui false)

(defmacro wrapping-fn [bindings & body]
  (let [pairs (partition 2 bindings)]
    `(do
       (clojure.lang.Var/pushThreadBindings 
        (into {}
              [~@(for [[f wrap] pairs]
                   `(let [orig# ~f] 
                      [#'~f (partial ~wrap orig#)]))]))
       (try
        ~@body
        (finally
         (clojure.lang.Var/popThreadBindings))))))

(def depth 0)
(comment
  (binding [depth 0]
    (wrapping-fn [write-out (fn [f obj] (cl-format true "*") (f obj))
                  clojure.contrib.pprint.PrettyWriter/-startBlock 
                  (fn [f this & args] 
                    (set! depth (inc depth)) 
                    (cl-format true "<~d" depth)
                    (apply f this args))
                  clojure.contrib.pprint.PrettyWriter/-endBlock 
                  (fn [f this & args] 
                    (set! depth (dec depth)) 
                    (cl-format true ">~d" depth) 
                    (apply f this args))]
      (pprint '((a b) c))))
  (nqsym #'write-out)
  )

(declare process-sublists)

(defn list-to-tree [l]
  (let [[typ start-line start-col] (first l)]
    (when (not (= typ :start))
      (throw (Exception. "List in list to tree didn't begin with a start")))
    (when (empty? (next l))
      (throw (Exception. "No next in list")))
    (let [[sublists remainder] (process-sublists (next l))]
      (when (empty? remainder)
        (throw (Exception. "No matching :end node in list")))
      (let [[typ end-line end-col] (first remainder)]
        (when (not (= typ :end))
          (throw (Exception. "Remainder didn't begin with :end")))
        [(concat [[start-line start-col] [end-line end-col]] sublists) (next remainder)]))))

(defn process-sublists [l]
  (loop [r l
         acc []]
    (if (or (nil? r) (not (= (first (first r)) :start)))
      [acc r]
      (let [[sublist r] (list-to-tree r)]
        (recur r (conj acc sublist))))))

(defn matches [node line col]
  (let [[[start-line start-col] [end-line end-col]] node]
    (and (or (> line start-line) (and (= line start-line) (>= col start-col)))
         (or (< line end-line) (and (= line end-line) (<= col end-col))))))

(defn search-tree [t line col]
  (loop [subnodes (next (next t))
         index 0]
    (cond
      (nil? subnodes) nil
      (matches (first subnodes) line col) (cons index (search-tree (first subnodes) line col))
      :else (recur (next subnodes) (inc index)))))

(defn pprint-obj [output-area]
  (let [obj @current-object
        q (LinkedBlockingQueue.)]
    (when obj 
      (with-open [sw (java.io.StringWriter.)
                  writer (PrettyWriter. sw *print-right-margin* nil)] ;; TODO rethink args to match window
        (.setLogicalBlockCallback writer #(.put q [% (.getLine writer) (.getColumn writer)]))
        (apply write obj :stream writer (into [] (mapcat identity @write-opts)))
        (.setText output-area (str sw))
        (dosync
         (ref-set block-tree (first (list-to-tree (into [] (.toArray q))))))))))

(defmacro spinner-watcher [ref-key]
  `(proxy [ChangeListener] []
    (stateChanged [evt#]
      (let [val# (.. evt# (getSource) (getModel) (getValue))]
        (dosync
         (binding [update-from-ui true]
           (alter write-opts assoc ~ref-key val#)))))))

;; TODO: clean up watches when we close the window
(defn make-panel [panel]
  (let [output-area (doto (JTextArea. 60 100)
                      (.setEditable false)
                      (.setFont (java.awt.Font. "Monospaced" java.awt.Font/PLAIN 14)))
        level-spinner (doto (JSpinner. (SpinnerNumberModel. (:level @write-opts) 1 1000 1))
                        (.addChangeListener (spinner-watcher :level)))
        options-update-key (gensym)
        length-spinner (doto (JSpinner. (SpinnerNumberModel. (:length @write-opts) 1 100000 1))
                  (.addChangeListener (spinner-watcher :length)))
        layout (miglayout 
                panel
                ;;:layout "debug"
                :row "[][fill]"
                (JLabel. "Level")
                level-spinner
                (JLabel. "Length")
                length-spinner 
                :wrap
                (JScrollPane. output-area) "spanx 4,growx,growy"
                )]
    (add-watch write-opts options-update-key
               (fn [_ _ _ new] 
                 (when (not update-from-ui)
                   (.setValue level-spinner (:level new))
                   (.setValue length-spinner (:length new)))))
    (.addMouseListener
     output-area
     (proxy [java.awt.event.MouseListener] []
       (mouseEntered [evt])
       (mouseExited [evt])
       (mousePressed [evt])
       (mouseReleased [evt])
       (mouseClicked [evt]
                     (let [pt (.getPoint evt)
                           char-num (.viewToModel output-area pt)
                           line (.getLineOfOffset output-area char-num)
                           column (- char-num (.getLineStartOffset output-area line))]
                       (cl-format *err* "Got point: (~d,~d). Translates to char ~d, line ~d, column ~d.~%~
                                             tree location = ~a~%"
                                  (.x pt) (.y pt) char-num line column
                                  (search-tree @block-tree line column))))))
    [layout output-area]))


(defn doit [obj]
  (dosync 
   (ref-set write-opts {:level (or *print-level* 3) :length (or *print-length* 5)})
   (ref-set current-object obj))
  (let [output-area (make-frame "Clojure Object Explorer" make-panel)
        do-write (fn [_ _ _ _] (pprint-obj output-area))]
    (add-watch write-opts (gensym) do-write)
    (add-watch current-object (gensym) do-write)
    (pprint-obj output-area)))

(defn xml-convert [o] 
  (into []
        (concat [(:tag o) (:attrs o)] (for [x (:content o)] (xml-convert x)))))

(comment
 (doit nil)

 (def build (parse (java.io.File. "../clojure/build.xml")))

 (binding [*print-level* 3 *print-length* 5] (doit build))
 (doit build)
 (dosync (alter write-opts assoc :level 10))
 (dosync (ref-set current-object (xml-convert build)))
 (dosync (alter write-opts assoc :dispatch xml-dispatch))
 
 
 (def pw (find-ns 'clojure.contrib.pprint.PrettyWriter))
 (get-var ('-startBlock (ns-interns pw)))
)
