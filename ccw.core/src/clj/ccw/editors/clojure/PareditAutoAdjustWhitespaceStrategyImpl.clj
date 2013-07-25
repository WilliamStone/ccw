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

; TODO l'offset fonctionne pas si plusieurs niveaux d'indentation
; TODO backspace ne fonctionne pas devant une form top level

;- l'offset offset a été décalé de delta caractères
;- soit loc la loc pour offset
;- soit loc-p la loc parente de loc
;- appliquer le décalage delta à tous les débuts de ligne inclus dans loc-p, à partir d'offset
;- s'arrêter après épuisement des lignes contenues dans loc-p, ou si une des lignes a du contenu plus à gauche que delta
;- si au moins une ligne plus à gauche que delta : c'est fini
;- sinon, on recommence avec le père de loc-p, en partant de l'offset de fin (initial) de loc-p, et avec delta

; HYPOTHESE : l'offset qui se decale marque le debut d'un noeud (= offset-start)
;             SI PAS VRAI => ON FAIT RIEN
;             mais non, pas toujours possible, par ex. si on ajoute
;             un espace au milieu d'espaces, ou une lettre au milieu d'un symbole
;             => la regle c'est: si (not= offset-start) => on prend le frere suivant
;
; ATTENTION, dans les freres il y aura des :comment qui vont poser probleme
;            il faut les traiter specialement, je le crains
;
; Plutôt que de faire des duplications, plutôt implementer un decalage
; rapide par offset (pas optimise pour l'instant) pour se retrouver sur le
; bon noeud. Ou implémenter un zipper intelligent qui calcule ce qu'il faut
; pour les offsets (au moment de la construction du noeud, rajouter
; les meta donnees :cumulative-count)
;
; =>SIMPLIFIER l'algorithme en utilisant shift-whitespace sur les freres de l'offset,
;              puis éventuellement les freres du pere si la fin du père a été poussée à gauche, etc.
;              et toujours garder le fait que que dès qu'on tombe sur un élément plus à "gauche" qu'offset,
;              on arrête la propagation dans les freres (et ça bloque aussi la progagation dans les peres, du coup)
;              (si on fait pas ça, alors ouille, on parcourra systématiquement tout le fichier)


; BUGS:
; - a comment "stops" the propagation of the shift if after the comment we're
;     at the first column
; - inserting parens or double quotes or chars shifts the whole content: baad
; TEST CASES:
; - after a comment
 

(defn propagate-delta [loc col delta]
  (let [depth (count (zip/path loc))
        ;_ (println "depth" depth)
        [loc st] (loop [l loc]
         ;          (println "l node:" (with-out-str (pr (zip/node l))))
         ;          (println "(count (zip/path l))" (count (zip/path l)))
         ;           (println "(lu/newline? l)" (lu/newline? l))
                   (if (> depth (count (zip/path l)))
                     [l :continue]
                     (let [[l st] (cond 
                                    (lu/newline? l)
                                    (if (lu/whitespace? l)
                                      (let [blanks (let [text (lu/loc-text l)]
                                                     (if (.contains text "\n")
                                                       (- (count text)
                                                          (inc (.lastIndexOf text "\n")))
                                                       (count text)))]
                                        (if (<= col blanks)
                                          [(lu/shift-nl-whitespace l delta) :continue]
                                          [l :stop]))
                                      ; l is not whitespace
                                      (if (and (zero? col) (pos? delta))
                                        [(lu/shift-nl-whitespace l delta) :continue]
                                        [l :stop]))
                                    :else
                                    [l :continue]
                                    ; TODO recursive call
                                    )]
                       (cond
                         (= :stop st)             [l :stop]
                         (nil? (zip/next l))     [l :continue]
                         (zip/end? (zip/next l)) [l :stop]
                         :else                    (recur (zip/next l))))))]
    (if (= :stop st)
      [loc :stop]
      (if-let [next-loc (if-let [p (zip/up loc)] (zip/right p))]
        (recur next-loc
               (lu/loc-col next-loc) ; FIXME may not work if :cumulated-count is not correct 
               ; OR MAY RETURN old col
               delta)
        [loc :stop]))))
  
(defn noop-diff? [diff]
  (and (zero? (:length diff))
       (zero? (count (:text diff)))))

(defn customizeDocumentCommand 
  "Work only if no command has been added via (.addCommand)"
  [^PareditAutoAdjustWhitespaceStrategy this, #^IDocument document, #^DocumentCommand command]
  (let [^IClojureEditor editor (-> this .state deref :editor)
        {:keys [parse-tree buffer]} (.getParseState editor)
        text-before (lu/node-text parse-tree)
        ;_ (println "text-before:" (str "'" text-before "'"))
        parse-tree (-> buffer
                     (p/edit-buffer (.offset command) (.length command) (.text command))
                     (p/buffer-parse-tree 0))
        text (lu/node-text parse-tree)
        ;_ (println "text:" (str "'" text "'"))
        offset (+ (.offset command) (count (.text command)))
        offset-before (+ (.offset command) (.length command))
        col (tu/col text offset)
        delta (- col
                 (tu/col text-before offset-before)
                 )
        ;_ (println "delta:" delta)
        rloc (lu/parsed-root-loc parse-tree)
        loc (lu/loc-for-offset rloc offset)
        loc (if (< (lu/start-offset loc) offset) (zip/right loc) loc)
        col (- (lu/loc-col loc) delta)
        ;_ (println "col" col)
        ;_ (println "loc-node:" (zip/node loc))
        ]
    (when loc
      (let [[shifted-loc _] (propagate-delta loc col delta)
            shifted-text (lu/node-text (zip/root shifted-loc))
            ;_ (println "shifted-text:" (with-out-str (pr shifted-text)))
            ;_ (println "text        :" (with-out-str (pr text)))
            loc-diff (tu/text-diff text shifted-text)
            ;_ (println "loc-diff" (with-out-str (pr loc-diff)))
            diff (update-in 
                   loc-diff
                   [:offset] + (.length command) (- (count (.text command))))
            ;_ (println "diff" (with-out-str (pr diff)))
            ]
        (when-not (noop-diff? loc-diff)
          (.addCommand command 
            (:offset diff)
            (:length diff)
            (:text diff)
            nil)
          (set! (.shiftsCaret command) false)
          (set! (.caretOffset command) offset))))))

