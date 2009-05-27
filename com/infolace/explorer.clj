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
        [clojure.contrib.pprint :only (write cl-format *print-right-margin*)]
        [clojure.contrib.pprint.utilities :only (prlabel)]))

(defn make-frame [title panel-fn]
  (let [[panel text-area] (panel-fn (JPanel.))] 
    (doto (JFrame. title)
     (.add panel)
     (.pack)
     (.setVisible true))
    text-area))

(def current-level (ref 0))
(def current-length (ref 0))
(def current-object (ref nil))
(def block-tree (ref nil))

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
        (binding [*print-level* @current-level
                  *print-length* @current-length]
          (write obj :stream writer)
          (.setText output-area (str sw))
          (dosync
           (ref-set block-tree (first (list-to-tree (into [] (.toArray q)))))))))))

(defmacro spinner-watcher [ref-name output-area]
  `(proxy [ChangeListener] []
     (stateChanged [evt#]
       (let [val# (.. evt# (getSource) (getModel) (getValue))]
         (dosync
          (ref-set ~ref-name val#)
          (pprint-obj ~output-area))))))


(defn make-panel [panel]
  (let [output-area (doto (JTextArea. 60 100)
                      (.setEditable false)
                      (.setFont (java.awt.Font. "Monospaced" java.awt.Font/PLAIN 14)))
        layout (miglayout 
                panel
                ;;:layout "debug"
                :row "[][fill]"
                (JLabel. "Level")
                (doto (JSpinner. (SpinnerNumberModel. @current-level 1 1000 1))
                  (.addChangeListener (spinner-watcher current-level output-area)))
                (JLabel. "Length")
                (doto (JSpinner. (SpinnerNumberModel. @current-length 1 100000 1))
                  (.addChangeListener (spinner-watcher current-length output-area))) 
                :wrap
                (JScrollPane. output-area) "spanx 4,growx,growy"
                )]
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
   (ref-set current-level (or *print-level* 3))
   (ref-set current-length (or *print-length* 5))
   (ref-set current-object obj))
  (let [output-area (make-frame "Clojure Object Explorer" make-panel)]
    (pprint-obj output-area)))


(comment
 (doit nil)

 (def build (parse (java.io.File. "../clojure/build.xml")))

 (binding [*print-level* 3 *print-length* 5] (doit build))
 (doit build)
 ; (with-pprint-dispatch *simple-dispatch* (doit build))
 
 (def pw (find-ns 'clojure.contrib.pprint.PrettyWriter))
 (get-var ('-startBlock (ns-interns pw)))
)
