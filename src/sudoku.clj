(ns sudoku
  (:require [clojure.set :as set]))


(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board coord]
  (let [[row _] coord]
    (loop [col 0
           s #{}]
      (if (= col 9) s
        (recur (inc col) (conj s (value-at board [row col])))))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (loop [row 0
           s #{}]
      (if (= row 9) s
        (recur (inc row) (conj s (value-at board [row col])))))))

(defn coord-pairs [coords]
  (for [i coords
        j coords] [i j]))

(defn block-values [board coord]
  (let [[row col] coord
        lr (* 3 (int (/ row 3)))
        lc (* 3 (int (/ col 3)))]
    (set (for [[r c] (coord-pairs [0 1 2 ])]
      (value-at board [(+ r lr) (+ c lc)])))))

(defn valid-values-for [board coord]
  (if (not= (value-at board coord) 0) #{}
  (set/difference #{1 2 3 4 5 6 7 8 9} (set/union (block-values board coord) (row-values board coord) (col-values board coord)) )))


(defn filled? [board]
  (reduce (fn [b1 v1] (and b1 (reduce (fn [b x] (and b (not= 0 x))) true v1))) true board))

(defn rows [board]
  (for [i (range 9)]
    (row-values board [i 0])))


(defn valid-rows? [board]
  (reduce (fn [b s](and b (= all-values s))) true (rows board)))

(defn cols [board]
  (for [i (range 9)]
    (col-values board [0 i])))

(defn valid-cols? [board]
    (reduce (fn [b s](and b (= all-values s))) true (cols board)))

(defn blocks [board]
  (for [i (range 0 9 3) j (range 0 9 3)] (block-values board [i j])))


(defn valid-blocks? [board]
    (reduce (fn [b s](and b (= all-values s))) true (blocks board)))

(defn valid-solution? [board]
  (and (valid-cols? board) (valid-rows? board) (valid-blocks? board)))


(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))


(defn find-empty-point [board]
  (first (take 1 (filter (fn [co] (= 0 (value-at board co))) (for [r (range 9) c (range 9)] [r c])))))


(defn solve-all [board]
  (if (filled? board)
    (if (valid-solution? board) [board] [])
    (let [co (find-empty-point board)]
      (apply concat (for [v (valid-values-for board co)]
                      (solve-all (set-value-at board co v)))))))

(defn solve [board]
  (first (solve-all board)))
