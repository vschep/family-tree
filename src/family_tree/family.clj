(defn extract-children [to-extract]
  (let [child (re-find to-extract)]
    (if child
      (conj (extract-children to-extract) (last child))
      (vector))))

(defn read-a-family [families line]
  (let [parent-children (re-find #"<(\w+)> parent_of (.+)" line)]
    (if parent-children
      (let [child-matcher (re-matcher #"<(\w+)>" (last parent-children))
            children (extract-children child-matcher)
            parent (second parent-children)]
        (assoc families parent children))
      families)))

(defn read-families []
  (with-open [rdr (clojure.java.io/reader "/Users/vschepik/src/clojure/family-tree/people.data")]
    (reduce read-a-family (hash-map) (line-seq rdr))))
