(ns walk-visualizations.id-generation)

(defonce id-generator-states
  (atom {}))

(defn add-key [key initial-value]
  (swap! id-generator-states assoc key initial-value))

(defn next-id [key]
  (let [id_num (key @id-generator-states)
        id (str (name key) "_" id_num)]
    (swap! id-generator-states assoc key (inc id_num))
    id))

(defn set-states [states]
  (reset! id-generator-states states))
