(ns family-tree.core)

(str "dog" "s")

(defn pluralize [word] 
  (str word "s"))

(let [animals ["dog" "cat"]]
  (map pluralize animals))
