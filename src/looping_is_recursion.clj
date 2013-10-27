(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (cond
                  (= exp 0) 1
                  (= exp 1) (* acc base)
                  (> exp 1) (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (or (empty? seq1) (empty? seq2)) false
   (not (= (first seq1) (first seq2))) false
   :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [index 0
         my-seq a-seq]
    (cond
     (empty? my-seq) nil
     (pred (first my-seq)) index
     :else (recur (inc index) (rest my-seq)))))

(defn avg [a-seq]
  (if (empty? a-seq)
    0
    (loop [sum 0
           index 0
           my-seq a-seq]
      (if (empty? my-seq)
        (/ sum index)
        (recur (+ sum (first my-seq)) (inc index) (rest my-seq))))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem] (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))]
  (loop [odds #{}
         my-seq a-seq]
    (if (empty? my-seq)
      odds
      (recur (toggle odds (first my-seq)) (rest my-seq))))))

(defn fast-fibo [n]
  (loop [i n
         Fn 0
         Fn+1 1]
    (if (< i 1)
      Fn
      (recur (dec i) Fn+1 (+ Fn Fn+1)))))

(defn cut-at-repetition [a-seq]
  (loop [unread a-seq
         result []
         found #{}]
    (cond
     (empty? unread) result
     (contains? found (first unread)) result
     :else (recur (rest unread) (conj result (first unread)) (conj found (first unread))))))

