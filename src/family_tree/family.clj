(ns family-tree.core)

(def fam-db (ref {}))

(defmacro dbg
  [x]
  `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

;;; read the file

(defn extract-children [to-extract]
  (let [child (re-find to-extract)]
    (if child
      (conj (extract-children to-extract) (last child))
      (vector))))

(defn read-parent-of [families line]
  (let [parent-children (re-find #"<(\w+)> parent_of (.+)" line)]
    (if parent-children
      (let [child-matcher (re-matcher #"<(\w+)>" (last parent-children))
            children (extract-children child-matcher)
            parent (second parent-children)]
        (assoc families parent children))
      families)))

(defn store-couple [couples the-one the-other]
  (assoc (assoc couples the-one the-other) the-other the-one))

(defn read-married-to[couples line]
  (let [married-to (re-find #"<(\w+)> married_to <(\w+)>" line)]
    (if married-to
      (store-couple couples (second married-to) (last married-to))
      couples)))

(defn read-a-family [[families couples] line]
  [(read-parent-of families line) (read-married-to couples line)])

(defn read-families []
  (with-open [rdr (clojure.java.io/reader "/Users/vschepik/src/clojure/family-tree/people.data")]
    (dosync
      (ref-set fam-db (reduce read-a-family [(hash-map) (hash-map)] (line-seq rdr))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; query the family-tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn children-of [parent]
  ((first @fam-db) parent))

(defn root [fam-db]
  (let [parents (set (keys (first fam-db)))
        children (flatten (vals (first fam-db)))]
    (first (apply (partial disj parents) children))))

(defn assoc-parent [first-matched-parent current-parent anc-level]
  (if (and (not (:parent first-matched-parent)) (= anc-level (+ 1 (:hop-back first-matched-parent))))
    (assoc first-matched-parent :parent current-parent)
    (assoc first-matched-parent :hop-back (+ 1 (:hop-back first-matched-parent)))))

(defn search-deeper [{fam-db :fam-db        
                      current-parent :current-parent 
                      offspring :offspring  
                      anc-level :ancestry-level
                      children :next-children}]

  (let [max-children (count children)]
    (loop [n 0
           found-parent nil]
      (do
        (println "$$$ begin search-deeper:" children found-parent)
        (if (< n max-children)
          (if (not found-parent)
            ; search deeper
            (let [next-child (nth children n)]
              (do
                (println "$$$ search deeper")
                (let [deeper-parent (find-parent {:fam-db fam-db 
                                                  :current-parent next-child 
                                                  :offspring offspring 
                                                  :ancestry-level anc-level})]
                  (if deeper-parent 
                    (assoc-parent deeper-parent current-parent anc-level)
                    (do
                      (println "$$$ children before IndexOutOfBounds " children)
                      (println "$$$ n " n)
                      ; continue looping
                      (recur (+ n 1)                    
                             deeper-parent))))))))))))

(defn find-parent [{fam-db :fam-db        
                    current-parent :current-parent 
                    offspring :offspring  
                    anc-level :ancestry-level}]
  (do
    (println "$$$ offspring in find-parent " offspring)
    (let [direct-children ((first fam-db) current-parent)
          couples (second fam-db)
          couples-of-children (remove nil? (map #(get couples %) direct-children))
          children (flatten (conj direct-children couples-of-children))]
      (do
        (dbg children)
        ; search the offspring
        (cond 
          ; no children left
          (empty? children) (do 
                              (println "no children left")
                              nil)

        ; offspring is a child
        (some (partial = offspring) children) (if (= 1 anc-level)
                                                (do
                                                  (println "$$$ offspring found: " offspring anc-level current-parent children)
                                                  {:parent current-parent :hop-back 1})
                                                  {:hop-back 1})
        ; search deeper in the tree
        :default (do 
                   (println "$$$ search-deeper " current-parent offspring anc-level children)
                   (search-deeper {:fam-db fam-db 
                                   :current-parent current-parent
                                   :offspring offspring 
                                   :ancestry-level anc-level
                                   :next-children children})))))))

(defn parent-of [offspring]
  (:parent (find-parent {:fam-db @fam-db 
                         :current-parent (root @fam-db) 
                         :offspring offspring
                         :ancestry-level 1})))

(defn grandparent-of [offspring]
  (do
    (println "$$$ offspring in grandparent-of " offspring)
    (:parent (find-parent {:fam-db @fam-db 
                           :current-parent (root @fam-db) 
                           :offspring offspring
                           :ancestry-level 2}))))

