(ns walk-visualizations.util)

(defn square [n]
  (* n n))

(defn squared-diff [a b]
  (let [diff (- a b)]
    (square diff)))

(defn squared-distance [[x1 y1] [x2 y2]]
  (let [x-dist (squared-diff x1 x2)
        y-dist (squared-diff y1 y2)
        squared-distance (+ x-dist y-dist)]
    squared-distance))

(defn are-points-close? [[x1 y1] [x2 y2] scale]
  (let [x-dist (squared-diff x1 x2)
        y-dist (squared-diff y1 y2)
        squared-distance (+ x-dist y-dist)
        max-dist (/ 20 scale)]
    (< squared-distance max-dist)))

(defn- consecutive-pairs-helper [current remaining accumulator]
  (if (empty? remaining)
    accumulator
    (let [x (first remaining)
          xs (rest remaining)
          p [current x]
          new-acc (conj accumulator p)]
      (recur x xs new-acc))))

(defn consecutive-pairs [list]
  ;; form pairs of consecutive elements, includes last and first
  (if (empty? list)
    []
    (consecutive-pairs-helper (last list) list [])))

