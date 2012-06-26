(ns family-tree.test.core
  (:use [family-tree.core])
  (:use [clojure.test]))

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

(deftest test-parents-of-without-marriage
  (dosync (ref-set fam-db [{"Bregor" ["Gilwen" "Bregil" "Gwindor"]
                            "Bregil" ["Elured" "Elurin"]}]))
  (is (= "Bregor" (parent-of "Bregil"))))

(deftest test-grandparents-of-without-marriage-1
  (dosync (ref-set fam-db [{"Bregor" ["Gilwen" "Bregil" "Gwindor"]
                            "Bregil" ["Elured" "Elurin"]}]))
  (is (= "Bregor" (grandparent-of "Elured"))))

(deftest test-grandparents-of-without-marriage-2
  (dosync (ref-set fam-db [{"Bregor" ["Gilwen" "Bregil" "Gwindor"]
                            "Bregil" ["Elured" "Elurin"]}]))
  (is (= "Bregor" (grandparent-of "Elurin"))))

(deftest test-grandparents-of-without-marriage-3
  (dosync (ref-set fam-db [{"Bregor" ["Gilwen" "Bregil" "Gwindor"]
                            "Bregil" ["Elured"]}]))
  (is (= "Bregor" (grandparent-of "Elured"))))

(deftest test-grandparents-of-without-marriage-4
         (dosync (ref-set fam-db [{"Bregor" ["Gilwen" "Bregil" "Gwindor"]
                                   "Bregil" ["Elured" "Elurin"]
                                   "Gwindor" ["Vardamir" "Figwit"]
                                   "Vardamir" ["Baragund" "Vardilme"]}]))
         (is (= "Gwindor" (grandparent-of "Baragund"))))
