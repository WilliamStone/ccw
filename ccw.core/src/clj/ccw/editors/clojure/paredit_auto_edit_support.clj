(ns ccw.editors.clojure.paredit-auto-edit-support
  (:use [clojure.core.incubator :only [-?>]])  
  (:import [org.eclipse.jface.text DocumentCommand]
           [ccw.editors.clojure IClojureEditor]))

(defn apply-modif! 
  "Apply modification in result to command for editor."
  [^IClojureEditor editor ^DocumentCommand command result]
  (when (and result (not= :ko (-> result :parser-state)))
    (if-let [modif (-?> result :modifs first)]
      (do
        (set! (.offset command) (-> result :modifs first :offset))
        (set! (.length command) (-> result :modifs first :length))
        (set! (.text command) (-> result :modifs first :text))
        (doseq [{:keys [offset length text]} (rest (-> result :modifs))]
          (.addCommand command offset length text nil)))
      (do
        (set! (.offset command) (:offset result))
        (set! (.length command) 0)
        (set! (.text command) "")
        (set! (.doit command) false)))
    (set! (.shiftsCaret command) false)
    (set! (.caretOffset command) (:offset result))
    (when-not (zero? (:length result)) ;;; WHY when-not zero? 
      (.selectAndReveal editor (:offset result) (:length result)))))
