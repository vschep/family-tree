(ns family-tree.test.core
  (:use [family-tree.core])
  (:use [clojure.test]))

;;; test helpers

(defn check-couple [couple spouses]
  (every? true? (map #(some (partial = %) couple) spouses)))


;;; read the file

(deftest test-read-parent-of-empty-hash-1
  (is (= (read-parent-of {} "<Bregor> parent_of <Hirwen>.") {"Bregor" ["Hirwen"]})))

(deftest test-read-parent-of-empty-hash
  (let [result (read-parent-of {} "<Bregor> parent_of <Hirwen>, <Gilwen>, <Bregil>, <Bregolas>, <Barahir>, <Gwindor>.")
        children (result "Bregor")]
    (is (and 
          (contains? result "Bregor")
          (some (partial = "Hirwen") children)))))

(deftest test-read-parent-of-existing-families
  (is (read-parent-of {"Gilwen" ["Hiril" "Hareth"]} "<Bregor> parent_of <Hirwen>, <Gilwen>, <Bregil>, <Bregolas>, <Barahir>, <Gwindor>.") 
      {"Bregor" ["Hirwen" "Gilwen" "Bregil" "Bregolas" "Barahir" "Gwindor"]
       "Gilwen" ["Hiril" "Hareth"]}))

(deftest test-read-parent-of-no-match
  (is (read-parent-of {} "<Gilwen> married_to <Alatar>.") {}))


;;; query the family-tree

(deftest test-children-of
         (dosync (ref-set fam-db [{"Bregor" ["Hirwen" "Gilwen"]
                                   "Gilwen" ["Hiril" "Hareth"]}]))
         (let [children (children-of "Gilwen")]
           (is (and 
                 (some (partial = "Hiril") children)
                 (some (partial = "Hareth") children)))))

(deftest test-grandchildren-of-1
         (dosync (ref-set fam-db [{"Bregor" ["Gilwen" "Bregil" "Gwindor"]
                                   "Bregil" ["Elured" "Elurin"]
                                   "Gwindor" ["Vardamir" "Figwit"]
                                   "Vardamir" ["Baragund" "Vardilme"]}]))
         (let [grandchildren (grandchildren-of "Gwindor")]
           (is (and
                 (some (partial = "Baragund") grandchildren)
                 (some (partial = "Vardilme") grandchildren)))))

(deftest test-grandchildren-of-2
         (dosync (ref-set fam-db [{"Bregil" ["Elured" "Elurin"]
                                   "Idril"  ["Elwin" "Earendil"]
                                   "Elurin" ["Aerandir" "Erellont"]}
                                  {"Elured" "Idril", "Idril" "Elured", "Elurin" "Este", "Este" "Elurin"}]))
         (let [grandchildren (grandchildren-of "Bregil")]
           (is (and
                 (some (partial = "Elwin") grandchildren)
                 (some (partial = "Earendil") grandchildren)
                 (some (partial = "Aerandir") grandchildren)
                 (some (partial = "Erellont") grandchildren)))))


(deftest test-parents-of-without-marriage
         (dosync (ref-set fam-db [{"Bregor" ["Gilwen" "Bregil" "Gwindor"]
                                   "Bregil" ["Elured" "Elurin"]}]))
         (is (check-couple ["Bregor"] (parent-of "Bregil"))))

(deftest test-parents-with-marriage
         (dosync (ref-set fam-db [{"Bregor" ["Gilwen" "Bregil" "Gwindor"]}
                                  {"Bregor" "Celebrian", "Celebrian" "Bregor", "Bregil" "Aegnor", "Aegnor" "Bregil"}]))
         (is (check-couple ["Bregor" "Celebrian"](parent-of "Aegnor"))))

(deftest test-grandparents-of-without-marriage-1
         (dosync (ref-set fam-db [{"Bregor" ["Gilwen" "Bregil" "Gwindor"]
                                   "Bregil" ["Elured" "Elurin"]}]))
         (is (check-couple ["Bregor"] (grandparent-of "Elured"))))

(deftest test-grandparents-of-without-marriage-2
         (dosync (ref-set fam-db [{"Bregor" ["Gilwen" "Bregil" "Gwindor"]
                                   "Bregil" ["Elured" "Elurin"]}]))
         (is (check-couple ["Bregor"] (grandparent-of "Elurin"))))

(deftest test-grandparents-of-without-marriage-3
         (dosync (ref-set fam-db [{"Bregor" ["Gilwen" "Bregil" "Gwindor"]
                                   "Bregil" ["Elured"]}]))
         (is (check-couple ["Bregor"] (grandparent-of "Elured"))))

(deftest test-grandparents-of-without-marriage-4
         (dosync (ref-set fam-db [{"Bregor" ["Gilwen" "Bregil" "Gwindor"]
                                   "Bregil" ["Elured" "Elurin"]
                                   "Gwindor" ["Vardamir" "Figwit"]
                                   "Vardamir" ["Baragund" "Vardilme"]}]))
         (is (check-couple ["Gwindor"] (grandparent-of "Baragund"))))

(deftest test-root-detection
         (dosync (ref-set fam-db [{"Bregor" ["Gilwen" "Bregil" "Gwindor"]
                                   "Bregil" ["Elured" "Elurin"]
                                   "Gwindor" ["Vardamir" "Figwit"]
                                   "Vardamir" ["Baragund" "Vardilme"]}]))
         (is (= "Bregor" (root @fam-db))))

(deftest test-get-couple
         (dosync (ref-set fam-db [{}
                                  {"Bregor" "Celebrian", "Celebrian" "Bregor", "Bregil" "Aegnor", "Aegnor" "Bregil"}]))
         (is (check-couple (get-couple "Bregor") ["Bregor" "Celebrian"])))

