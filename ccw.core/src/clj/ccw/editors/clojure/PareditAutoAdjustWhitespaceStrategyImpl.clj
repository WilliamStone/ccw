(ns ccw.editors.clojure.PareditAutoAdjustWhitespaceStrategyImpl
  (:require [paredit [core :refer [paredit]]])
  (:require [clojure.core.incubator :refer [-?>]])
  (:require [ccw.editors.clojure.paredit-auto-edit-support :as support])
  (:require [paredit.loc-utils :as lu])
  (:require [paredit.text-utils :as tu])
  (:require [paredit.parser :as p])
  (:require [clojure.zip :as zip])
  (:import
    [org.eclipse.jface.text IAutoEditStrategy
                            IDocument
                            DocumentCommand]
    [org.eclipse.jface.preference IPreferenceStore]
    [ccw.editors.clojure IClojureEditor PareditAutoAdjustWhitespaceStrategy]))
   
#_(set! *warn-on-reflection* true)

(defn init
  [editor preference-store] (ref {:editor editor :prefs-store preference-store}))

#_(defn- right-or-up [loc]
  (when loc (or (zip/right loc) (recur (zip/up loc)))))
(defn next-code
  "Return loc if it's code, or the next on the right which is code, or nil"
  [loc]
  (when loc
    (if-not (= :whitespace (:tag (zip/node loc)))
      loc
      (recur (zip/right loc)))))

(defn find-node
  "Return loc if it's code, or the next on the right which is code, or nil"
  [loc]
  (let [line-stop (tu/line-stop (lu/node-text (zip/root loc)) (lu/start-offset loc))]
    ;(println "line-stop:" line-stop)
    (loop [loc (next-code loc)]
      (when (and loc (< (lu/start-offset loc) line-stop))
        (if (.contains (lu/loc-text loc) "\n")
          loc
          (recur (next-code (zip/right loc))))))))

; TODO l'offset fonctionne pas si plusieurs niveaux d'indentation
; TODO backspace ne fonctionne pas devant une form top level

;- l'offset offset a été décalé de delta caractères
;- soit loc la loc pour offset
;- soit loc-p la loc parente de loc
;- appliquer le décalage delta à tous les débuts de ligne inclus dans loc-p, à partir d'offset
;- s'arrêter après épuisement des lignes contenues dans loc-p, ou si une des lignes a du contenu plus à gauche que delta
;- si au moins une ligne plus à gauche que delta : c'est fini
;- sinon, on recommence avec le père de loc-p, en partant de l'offset de fin (initial) de loc-p, et avec delta

;(defn tree-at [loc] (-> loc zip/node lu/parsed-root-loc))

(defn path-count [loc] (count (zip/path loc)))

(defn propagate-delta [loc col delta]
  (println "propagate-delta[loc-text" (str "'" (lu/loc-text loc) "'") ",col: " col ", delta:" delta)
  (let [loc-p (zip/up loc)
        _ (println "loc-p" loc-p)
        loc-p-path (cond
                     (nil? loc-p) [(zip/node loc)]
                     (zip/up loc-p) (zip/path loc-p)
                     :else [(zip/node loc-p)])
        loc-p-path-cnt (count loc-p-path)
        ;loc-p-parents (count loc-p-path)
        [loc st] (loop [l loc]
                   (if-let [next-nl-loc (when-not (zip/end? l)
                                          (lu/next-newline-loc l))]
                     (if ;(> (path-count next-nl-loc) loc-p-parents)
                         (= (take loc-p-path-cnt (zip/path next-nl-loc))
                            loc-p-path)
                       (if (<= col (tu/col (lu/loc-text next-nl-loc) 
                                           (count (lu/loc-text next-nl-loc) ; using count because a space can span many lines
                                                                            ))) 
                         (recur (zip/next (lu/shift-nl-whitespace next-nl-loc delta)))
                         [l :stop])
                       [l :ok])
                     [l :ok]))]
    (if (and (= :ok st) #_(zip/up loc-p))
      (if-let [loc (zip/right (first (filter #(= loc-p-path-cnt (path-count %)) 
                                             (iterate zip/up loc))))]
        (recur loc (- (lu/loc-col loc) delta) delta)
        loc)
      loc)))
  
(defn noop-diff? [diff]
  (and (zero? (:length diff))
       (zero? (count (:text diff)))))

(defn customizeDocumentCommand 
  "Work only if no command has been added via (.addCommand)"
  [^PareditAutoAdjustWhitespaceStrategy this, #^IDocument document, #^DocumentCommand command]
  (let [^IClojureEditor editor (-> this .state deref :editor)
        {:keys [parse-tree buffer]} (.getParseState editor)
        text-before (lu/node-text parse-tree)
        parse-tree (-> buffer
                     (p/edit-buffer (.offset command) (.length command) (.text command))
                     (p/buffer-parse-tree 0))
        text (lu/node-text parse-tree)
        offset (+ (.offset command) (count (.text command)))
        offset-before (+ (.offset command) (.length command))
        col (tu/col text offset)
        delta (- col
                 (tu/col text-before offset-before))
        rloc (lu/parsed-root-loc parse-tree)
        loc (lu/loc-for-offset rloc offset)]
    (when loc
      (let [shifted-loc (propagate-delta loc col delta)
            shifted-text (lu/node-text (zip/root shifted-loc))
            loc-diff (tu/text-diff text shifted-text)
            diff (update-in 
                   loc-diff
                   [:offset] - delta)]
        (when-not (noop-diff? diff)
          (.addCommand command 
            (:offset diff)
            (:length diff)
            (:text diff)
            nil)
          (set! (.shiftsCaret command) false)
          (set! (.caretOffset command) offset))))))

