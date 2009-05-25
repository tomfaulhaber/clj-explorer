(ns com.infolace.explorer
  ;; TODO - document
  ;; TODO - make sure the import set is minimal
  (:import (javax.swing JButton JFrame JLabel JList JPanel
                        JScrollPane JTabbedPane JTextField JSeparator
                        JTextArea JSpinner SpinnerNumberModel)
           (javax.swing.event ChangeListener ))
  (:use clojure.contrib.miglayout
        clojure.xml
        [clojure.contrib.pprint :only (write)]))

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

(defn pprint-obj [output-area]
  (let [obj @current-object]
    (when obj 
      (binding [*print-level* @current-level
                *print-length* @current-length]
        (.setText output-area (write obj :stream nil))))))

(defmacro spinner-watcher [ref-name output-area]
  `(proxy [ChangeListener] []
     (stateChanged [evt#]
       (let [val# (.. evt# (getSource) (getModel) (getValue))]
         (dosync
          (ref-set ~ref-name val#)
          (pprint-obj ~output-area))))))

(defn make-panel [panel]
  (let [output-area (doto (JTextArea. 25 80)
                      (.setEditable false)
                      (.setFont (java.awt.Font. "Monospaced" java.awt.Font/PLAIN 12)))
        layout (miglayout 
                panel
                ;; :layout :debug
                (JLabel. "Level")
                (doto (JSpinner. (SpinnerNumberModel. @current-level 1 1000 1))
                  (.addChangeListener (spinner-watcher current-level output-area)))
                (JLabel. "Length")
                (doto (JSpinner. (SpinnerNumberModel. @current-length 1 100000 1))
                  (.addChangeListener (spinner-watcher current-length output-area))) :wrap
                (JScrollPane. output-area) "spanx 4,growx,growy")]
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
