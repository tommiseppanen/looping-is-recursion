(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
   (let [helper (fn [acc b-seq]
                 (if (empty? b-seq)
                   acc
                   (recur (first b-seq) (rest b-seq))))]
    (helper nil a-seq)))

(defn seq= [a-seq b-seq]
  (let [helper (fn [acc seq1 seq2]
                 (cond
                   (and (empty? seq1) (empty? seq2)) acc
                   (or (empty? seq1) (empty? seq2)) false
                   :else (recur (and acc (= (first seq1) (first seq2)) )
                          (rest seq1) (rest seq2))))]
    (helper true a-seq b-seq)))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         loop-seq a-seq]
    (cond
      (empty? loop-seq) nil
      (= true (pred (first loop-seq))) acc
      :else (recur (inc acc) (rest loop-seq)))))

(defn avg [a-seq]
  (loop [total 0
         number-count 0
         numbers a-seq]
    (if (empty? numbers)
      (/ total number-count)
      (recur (+ total (first numbers))
             (inc number-count)
             (rest numbers)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [a-set #{}
         loop-seq a-seq]
    (if (empty? loop-seq)
      a-set
      (recur (toggle a-set (first loop-seq))
             (rest loop-seq)))))

(defn fast-fibo [n]
  (if (< n 1)
    0
    (loop [f-n-1     0
           f-n       1
           current-n 1]
      (if (= current-n n)
        f-n
        (recur f-n
               (+ f-n-1 f-n)
               (inc current-n))))))

(defn cut-at-repetition [a-seq]
  (loop [a-set   #{}
         items    []
         loop-seq  a-seq]
    (let [first-item (first loop-seq)]
      (if (or (empty? loop-seq)
            (contains? a-set first-item))
        items
        (recur (conj a-set first-item)
               (conj items first-item)
               (rest loop-seq))))))

