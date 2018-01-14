(defn median3 [x y z]
  (cond
    [(< x y)
      (cond
        [(< y z) y]
        [(< z x) x]
        [True z])]
    [True
      (cond
        [(< z y) y]
        [(< x z) x]
        [True z])]))


(defn qsort [ls]
  (assert (>= (len ls) 0) "ls.len >= 0 is required")
  (cond
    [(= (len ls) 0) []]
    [(= (len ls) 1) ls]
    [(= (len ls) 2)
      (if (< (first ls) (last ls))
        ls
        list [(last ls) (first ls)])]
    [True
      (setv center (if (-> ls (len) (odd?))
        (-> ls (len) (// 2))
        (-> ls (len) (// 2) (- 1))))
      (setv pivot (median3 (first ls) (nth ls center) (last ls)))
      (setv new-ls (cond
              [(= pivot (first ls))
                (cut ls 1)]
              [(= pivot (nth ls center))
                (flatten (list [(->> ls (take center) (list))
                                (-> ls (cut (+ center 1)))]))]
              [True
                (-> ls (butlast) (list))]))
      (setv smaller-or-equal (list-comp x [x new-ls] (<= x pivot)))
      (setv bigger (list-comp x [x new-ls] (> x pivot)))
      (flatten (list [(qsort smaller-or-equal)
                      pivot
                      (qsort bigger)]))]))


(defn main []
  (setv ls [6 8 2 9 1 3 4 7 5 10 3 5])
  (print "Before:" ls)
  (print "After: " (qsort ls)))  ; [1, 2, 3, 3, 4, 5, 5, 6, 7, 8, 9, 10]


(if (= --name-- "__main__")
  (main))
