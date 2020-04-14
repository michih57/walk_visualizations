(ns walk-visualizations.computation
  (:require [ajax.core :refer [POST]]))

(defn mk-comp-req [command data]
  {:command command
   :data data})

(defn computation-call [command data handler]
  (POST (str "http://localhost:3449/computation/" command)
        {:handler handler
         :format :json
         :params (mk-comp-req
                  command
                  data)}))

(defn compute-skeleton [nodes handler]
  (computation-call "straight_skeleton"
                    {:nodes nodes :nr_nodes (count nodes)}
                    handler))

(defn compute-motorcycle-graph [nodes handler]
  (computation-call "motorcycle_graph"
                         {:nodes nodes :nr_nodes (count nodes)}
                         handler))

(defn compute-polygon-offset [nodes offset handler]
  (let [data {:polygon {:nodes nodes :nr_nodes (count nodes)}
              :offset offset }]
    (computation-call "polygon_offset" data handler)))

(defn compute-midpoint [p1 p2 handler]
  (let [data {:start p1 :end p2}]
    (computation-call "midpoint" data handler)))

(defn compute-max-walkables [nodes start handler]
  (let [data {:polygon {:nodes nodes :nr_nodes (count nodes)}
              :start start}]
    (computation-call "max_walkables" data handler)))

(defn compute-intersection-ray-segments [ray segments handler]
  (let [data {:ray ray :segments segments}]
    (computation-call "intersect_ray_segments" data handler)))

(defn list-files [directory handler]
  (let [data {:dir directory}]
    (computation-call "list_files" data handler)))

(defn save-scene [scene directory filename handler]
  (let [data {
              :scene scene
              :directory directory
              :filename filename
              }]
    (computation-call "save_scene" data handler)))

(defn load-scene [directory filename handler]
  (let [data {
              :directory directory
              :filename filename
              }]
    (computation-call "load_scene" data handler)))
