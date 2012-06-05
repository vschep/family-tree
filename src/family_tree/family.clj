(defn read-families []
  (let [line "<Bregor> parent_of <Hirwen>, <Gilwen>, <Bregil>, <Bregolas>, <Barahir>, <Gwindor>."
        parent-children (re-find #"(<\w+>) parent_of (.+)" line)
        child-matcher (re-matcher #"<(\w+)>" (last parent-children))
        children (extract-children child-matcher)]
    (println "+++ children:")
    (println children)))

(defn extract-children [to-extract]
  (let [child (re-find to-extract)]
    (if child
      (do
        (print "child:")
        (println child)
        (conj (extract-children to-extract) (last child)))
      (vector))))
