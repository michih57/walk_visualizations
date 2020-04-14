(ns walk-visualizations.shapedependencies)

(defonce shape-dependencies
  (atom {}))

(defn merge-dependencies [to-merge]
  (swap! shape-dependencies merge to-merge))

(defn remove-dependency [dependencies master slave-to-remove]
  (let [current-slaves (master dependencies)
        new-slaves (disj current-slaves slave-to-remove)]
    (assoc dependencies master new-slaves)))

(defn remove-shape-from-dependencies [dependencies shape-id masters]
  (reduce (fn [deps master] (remove-dependency deps master shape-id)) dependencies masters))

(defn remove-dependencies-for-shape [shape-id]
  (let [masters-with-slaves (filter (fn [[master slaves]] (contains? slaves shape-id)) @shape-dependencies)
        masters (map first masters-with-slaves)]
    (swap! shape-dependencies remove-shape-from-dependencies shape-id masters)))

(defn remove-shape [id]
  (remove-dependencies-for-shape id)
  (swap! shape-dependencies dissoc id))

(defn add-dependency [dependencies master slave]
  (let [current-slaves (master dependencies)
        new-slaves (conj current-slaves slave)]
    (assoc dependencies master new-slaves)))

(defn add-shape-to-dependencies [dependencies shape-id masters]
  (let [dependencies-with-shape (assoc dependencies shape-id #{})
        add-single-master (fn [deps master] (add-dependency deps master shape-id))]
    (reduce add-single-master dependencies-with-shape masters)))
    
(defn add-dependencies [shape-id masters]
  (swap! shape-dependencies add-shape-to-dependencies shape-id masters))

(defn directly-dependent-shapes [shape-id]
  (into [] (shape-id @shape-dependencies)))

(defn dependent-shapes [shape-id]
  (let [dependencies @shape-dependencies
        direct-dependencies (directly-dependent-shapes (keyword shape-id))]
    (if (empty? direct-dependencies)
      []
      (let [recursive-deps (map (fn [s] (dependent-shapes s)) direct-dependencies)]
        (reduce into direct-dependencies recursive-deps)))))

