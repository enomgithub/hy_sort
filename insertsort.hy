(import copy)


(defn swap [ls i j]
  (setv tmp (get ls i))
  (assoc ls i (get ls j))
  (assoc ls j tmp))


(defn isorted [ls]
  (assert (>= (len ls) 0) "len(ls) >= 0")
  (cond
    [(<= (len ls) 1)]
    [(= (len ls) 2)
      (if (> (first ls) (last ls))
        (swap ls 0 1))]
    [True
      (for [i (->> ls len (range 1))]
        (setv k i)
        (for [j (range (- i 1) -1 -1)]
          (if (< (get ls i) (get ls j))
            (setv k j)
            (break)))
        (if (not (= i k))
          (do
            (.insert ls k (get ls i))
            (del (get ls (+ i 1))))))]))


(defn isort [ls]
  (setv ls-sorted (copy.deepcopy ls))
  (isorted ls-sorted)
  ls-sorted)


(defn main []
  (setv ls [6 8 2 9 1 3 4 7 5 10 3 5])
  (print "Before:" ls)
  (print "After:" (isort ls)))  ; [1, 2, 3, 3, 4, 5, 5, 6, 7, 8, 9, 10]


(if (= --name-- "__main__")
  (main))
