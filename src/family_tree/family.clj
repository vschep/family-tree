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

(defn ^:dynamic find-parent [{fam-db           :fam-db 
                              current-parent   :current-parent 
                              offspring        :offspring
                              anc-level        :ancestry-level}]
  (do
    (let [children ((first fam-db) current-parent)]
      (cond 
        ; there is nothing left
        (not children) nil

        ; succeeded
        (some (partial = offspring) children) (if (= 1 anc-level)
                                                {:parent current-parent :hop-back 1}
                                                {:hop-back 1})

        ; continue searching
        :default (let [max-children (count children)]
                   (loop [n 0
                          next-child (get children n)
                          found-parent nil]
                     (do
                       (if (< n max-children)
                         (if (not found-parent)
                           ; search deeper
                           (let [deeper-parent (find-parent {:fam-db fam-db 
                                                             :current-parent next-child 
                                                             :offspring offspring 
                                                             :ancestry-level anc-level})]
                             (if deeper-parent 
                               (if (and (not (:parent deeper-parent)) (= anc-level (+ 1 (:hop-back deeper-parent))))
                                 (assoc deeper-parent :parent current-parent)
                                 (assoc deeper-parent :hop-back (+ 1 (:hop-back deeper-parent))))
                               (recur (+ n 1) 
                                      (get children (+ n 1))
                                      deeper-parent))))))))))))

(defn parent-of [offspring]
  (:parent (find-parent {:fam-db @fam-db 
                         :current-parent (root @fam-db) 
                         :offspring offspring
                         :ancestry-level 1})))

(defn grandparent-of [offspring]
    (:parent (find-parent {:fam-db @fam-db 
                           :current-parent (root @fam-db) 
                           :offspring offspring
                           :ancestry-level 2})))

