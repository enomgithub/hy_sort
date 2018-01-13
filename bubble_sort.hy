(defn bswap [ls]
  (assert (>= (len ls) 1) "ls.len >= 1 is required")
  (cond
    [(= (len ls) 1)
      ls]
    [True
      (cond
        [(> (first ls) (second ls))
          (flatten
            (list [(second ls)
                   (-> (list [(first ls) (cut ls 2)])
                       (flatten)
                       (bswap))]))]
        [True
          (flatten
            (list [(first ls)
                   (-> (list [(second ls) (cut ls 2)])
                       (flatten)
                       (bswap))]))])]))


(defn bsort [ls]
  "Bubble sort"
  (assert (>= (len ls) 1) "ls.len >= 1 is required")
  (cond
    [(= (len ls) 1)
      ls]
    [True
      (setv ls-swap (bswap ls))
      (flatten
        (list [(-> ls-swap (butlast) (list) (bsort))
               (-> ls-swap (last))]))]))


(defn main []
  (print (bsort [4 2 1 3 7 9 2 5 6 8])))  ; [1, 2, 2, 3, 4, 5, 6, 7, 8, 9]


(if (= --name-- "__main__")
  (main))
