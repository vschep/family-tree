(defn read-families []
  (let [line "<Bregor> parent_of <Hirwen>, <Gilwen>, <Bregil>, <Bregolas>, <Barahir>, <Gwindor>."
        parent-children (re-find #"(<\w+>) parent_of (.+)" line)
        child-matcher (re-matcher #"<(\w+)>" (last parent-children))
        children (extract-children child-matcher nil)]
    (println "+++ children:")
    (println children)))

(defn extract-children [to-extract extracted]
  (let [child (re-find to-extract)]
    (if child
      (do
        (print "child:")
        (println child)
        (print "extracted:")
        (println extracted)
        (recur to-extract (conj extracted (last child))))
      (do
        (print "**extracted:")
        (println extracted)
        (extracted)))))
