(ns walk-visualizations.core
  (:require [cljs.pprint]
            [cljs.reader]
            [dommy.core :as dom]
            [walk-visualizations.util :as u]
            [walk-visualizations.pixi :as pixi]
            [walk-visualizations.computation :as comp]
            [walk-visualizations.id-generation :as id-gen]
            [walk-visualizations.shapedependencies :as deps]))

(defn mk-data-dump []
  (let [doc js/document
        data-dump (.createElement doc "textarea")]
    (.appendChild (.-body js/document) data-dump)
    data-dump))

(defonce shapes 
  (atom {}))

(defonce data
  (atom {:dump (mk-data-dump) }))

(defonce update-handlers
  (atom {}))

(defonce commands
  (atom {}))

(defn kw-lookup [map kw]
  (let [from-str-key (map (name kw))
        from-kw-key ((keyword kw) map)]
    (some identity [from-str-key from-kw-key])))

(defn write-to-dump [dump-data]
  (let [data-str (with-out-str (cljs.pprint/pprint dump-data))]
    (set! (.-value (:dump @data)) data-str)))

(defn read-from-dump []
  (let [data-str (.-value (:dump @data))]
    (cljs.reader/read-string data-str)))
  
(defn pixi-point-to-array [pixi-point]
  (let [x (.-x pixi-point)
        y (.-y pixi-point)]
    [x y]))

(defn add-shape! [shape]
  (let [id (:id shape)]
    (pixi/add-graphics! id)
    (swap! shapes assoc id shape)))

(defn remove-shape! [raw-id]
  (let [id (keyword raw-id)]
    (swap! shapes dissoc id)
    (let [slaves (deps/directly-dependent-shapes id)]
      (doseq [slave slaves]
        (remove-shape! slave)))
    (deps/remove-shape id)
    (pixi/remove-graphics! id)))

(defn remove-shapes! [ids]
  (doseq [id ids]
    (remove-shape! (keyword id))))

(defn remove-all-shapes! []
  (doseq [id (keys @shapes)]
    (remove-shape! (keyword id))))

(defn remove-polygon! [poly-id]
  (let [vertices (-> @shapes poly-id :data)]
    (remove-shapes! vertices)))

(defn render-shape! [shape-data]
  (let [id (keyword (:id shape-data))
        color (:color shape-data)
        renderer (keyword (:renderer shape-data))
        visible (:visible shape-data)
        data (:data shape-data)]
    (if visible 
      (pixi/render-shape id data color renderer)
      (pixi/hide-shape id))))

(defn render-shapes! []
  (doseq [[_ shape-data] @shapes]
    (render-shape! shape-data)))

(defn render-shape-by-id [shape-id]
  (render-shape! (-> @shapes shape-id)))

(defn render-shapes-by-id [shape-ids]
  (doseq [id shape-ids]
    (render-shape-by-id id)))

(defn translate-node [node translation]
  (map + translation node))

(defn translate-nodes [nodes translation]
  (map #(translate-node % translation) nodes))

(defn translate-edge [edge translation]
  (let [{:keys [start end]} edge]
    {:start (translate-node start translation)
     :end (translate-node end translation)}))

(defn translate-edges [edges translation]
  (map #(translate-edge % translation) edges))

(defn update-shape-property [shape-id property-key property-value]
  (swap! shapes assoc-in [shape-id property-key] property-value))

(defn update-shape-property-fn [shape-id property-key update-fn]
  (let [old-property-value (get-in @shapes [shape-id property-key])
        new-property-value (update-fn old-property-value)]
    (update-shape-property shape-id property-key new-property-value)))

(defn change-color [shape-id new-color]
  (update-shape-property shape-id :color new-color))

(defn hide-shape [id]
  (swap! shapes assoc-in [id :visible] false))

(defn show-shape [id]
  (swap! shapes assoc-in [id :visible] true))

(defn refresh-canvas []
  (render-shapes!)
  (pixi/draw))

(defn update-dependent-shapes [master-shape-id update]
  (let [deps (into [master-shape-id] (deps/dependent-shapes master-shape-id))]
    (doseq [raw-dep deps]
      (let [dep (keyword raw-dep)]
        (when-not (nil? (@shapes dep))
          (let [update-handler-key (keyword (-> @shapes dep :update-handler))
                update-handler (update-handler-key @update-handlers)]
            (update-handler dep update)))))
    (pixi/draw)))

(defn handle-canvas-bbox-change []
  (update-dependent-shapes :canvas-bbox {:type :canvas-bbox-change
                                         :data {}}))

(defn pan! [shift]
  (pixi/pan! shift)
  (handle-canvas-bbox-change)
  (refresh-canvas))

(defn reset-pan! []
  (pixi/reset-pan!)
  (handle-canvas-bbox-change)
  (refresh-canvas))

(defn zoom! [factor]
  (pixi/zoom! factor)
  (handle-canvas-bbox-change)
  (refresh-canvas))

(defn zoom-with-center-reference! [scale]
  (pixi/zoom-with-center-reference! scale)
  (handle-canvas-bbox-change)
  (refresh-canvas))

(defn zoom-in! []
  (zoom-with-center-reference! 2))

(defn zoom-out! []
  (zoom-with-center-reference! 0.5))

(defn reset-zoom! []
  (pixi/reset-zoom!)
  (handle-canvas-bbox-change)
  (refresh-canvas))

(defn zoom-with-pan-reference! [scale]
  (let [old-pan-position (pixi/pan-position)]
    (reset-pan!)
    (zoom! scale)
    (pan! old-pan-position)))

(defn fit-view-to-box [box]
  (let [{root :root width :width height :height} box
        pan-target (map - root)
        x-scale (/ pixi/canvas-height height)
        y-scale (/ pixi/canvas-width width)
        scale (min x-scale y-scale)]
    (reset-pan!)
    (reset-zoom!)
    (zoom! scale)
    (pan! pan-target)))

(defn polygon-bbox 
  ([nodes]
   (if (empty? nodes)
     nil
     (let [[x y] (first nodes)]
       (polygon-bbox (rest nodes) {:x-min x :x-max x :y-min y :y-max y}))))
  ([nodes current-extrema]
   (if (empty? nodes)
     (let [{:keys [x-min x-max y-min y-max]} current-extrema
           root [x-min y-max]
           width (- x-max x-min)
           height (- y-max y-min)]
       {:root root :width width :height height})
     (let [{:keys [x-min x-max y-min y-max]} current-extrema
           [x y] (first nodes)
           x-min-new (min x x-min)
           x-max-new (max x x-max)
           y-min-new (min y y-min)
           y-max-new (max y y-max)]
       (polygon-bbox (rest nodes) {:x-min x-min-new :x-max x-max-new :y-min y-min-new :y-max y-max-new})))))
  
(defn update-shape [shape-id property-key value]
  (swap! shapes assoc-in [shape-id property-key] value))

(defn create-point-shape [id coordinates]
  (let [shape-data {:id id
                    :renderer :point
                    :color 0x00ffff
                    :visible true
                    :data coordinates
                    :update-handler :point}]
    (add-shape! shape-data)
    (deps/add-dependencies id #{})))

(defn create-segment-shape [id start-point-id end-point-id]
  (let [start (-> @shapes start-point-id :data)
        end (-> @shapes end-point-id :data)
        shape-data {:id id
                    :renderer :segment
                    :color 0xff00ff
                    :visible true
                    :data {:start start :end end}
                    :update-handler :rule
                    :update-rule {:command :compute-segment-points
                                  :args {:start start-point-id
                                         :end end-point-id
                                         :id id}}}]
    (add-shape! shape-data)
    (deps/add-dependencies id #{start-point-id end-point-id})))

(defn create-polygon-shape [id vertex-ids]
  (let [shape-data {:id id
                    :renderer :polygon
                    :color 0xffff00
                    :visible true
                    :data vertex-ids
                    :update-handler :polygon}]
    (add-shape! shape-data)
    (deps/add-dependencies id (into #{} vertex-ids))))

(defn close-points [coords]
  (let [point-ids (filter (fn [shape-id] (.startsWith (name shape-id) "point_")) (keys @shapes))]
    (filter (fn [point-id] 
              (u/are-points-close? coords (-> @shapes point-id :data)  (pixi/zoom-scale-avg)))
            point-ids)))

(defn find-matching-point [coords]
  (let [candidates (close-points coords)]
    (first candidates)))

(defn make-point [coordinates]
  (let [id (keyword (id-gen/next-id :point))]
    (create-point-shape id coordinates)
    (render-shape! (id @shapes))
    id))

(defn make-point-or-get-existing [coordinates]
  (let [existing (find-matching-point coordinates)]
    (if (nil? existing)
      (make-point coordinates)
      existing)))

(defn make-segment [start-point-id end-point-id]
  (let [id (keyword (id-gen/next-id :segment))]
    (create-segment-shape id start-point-id end-point-id)
    (render-shape! (id @shapes))
    id))

(defn scene-serialization []
  (let [shapes @shapes
        dependencies @deps/shape-dependencies
        id-gens @id-gen/id-generator-states
        scene {
               :shape-dependencies dependencies
               :shapes shapes
               :id-generator-states id-gens
               }]
    scene))

(defn shape-to-dump [id]
  (let [shape-data (:data (id @shapes))]
    (write-to-dump shape-data)))

(defn restore-shape [id data]
  (pixi/add-graphics! id)
  (swap! shapes assoc id data))

(defn restore-shapes [shapes]
  (doseq [[shape-id shape-data] shapes]
      (restore-shape shape-id shape-data)))

(defn scene-from-dump []
  (remove-all-shapes!)
  (let [scene-data (read-from-dump)]
    (restore-shapes scene-data)))

(defn restore-scene [{shapes :shapes 
                      dependencies :shape-dependencies
                      id-gens :id-generator-states}]
  (restore-shapes shapes)
  (deps/merge-dependencies dependencies)
  (id-gen/set-states id-gens)
  (refresh-canvas))

(defn polygon-from-dump [id]
  (let [nodes (read-from-dump)
        vertices (map (fn [n] (make-point n)) nodes)
        vertex-coords (map (fn [v] (-> @shapes v :data)) vertices)
        vertex-pairs (u/consecutive-pairs vertices)]
    (doseq [[x y] vertex-pairs]
      (make-segment x y))
    (create-polygon-shape id vertices)
    (render-shape-by-id id)
    (fit-view-to-box (polygon-bbox vertex-coords))
    (pixi/draw)))

(defn polygon-to-dump [id]
  (let [node-ids (:data (id @shapes))
        nodes (map (fn [n-id] (:data (n-id @shapes))) node-ids)]
    (write-to-dump nodes)))

(defn add-update-handler! [key update-handler]
  (swap! update-handlers assoc key update-handler))

(defn point-update-handler [handled-point-id update]
  (when (= :point-move (:type update))
    (let [data (:data update)
          updated-point-id (:id data)
          new-coords (:new-coords data)]
      (when (= handled-point-id updated-point-id)
        (update-shape-property updated-point-id :data new-coords)
        (render-shape! (-> @shapes handled-point-id))))))

(defn polygon-update-handler [handled-polygon-id update]
  (println "updating polygon: " handled-polygon-id))

(defn execute-update-rule [shape-id update]
  (let [rule (-> @shapes shape-id :update-rule)
        command-key (keyword (:command rule))
        args (:args rule)
        command-handler (command-key @commands)]
    (command-handler args)))

(defn rule-update-handler [handled-shape-id update]
  (execute-update-rule handled-shape-id update))

(defn update-segment [id start-point-id end-point-id]
  (let [start (-> @shapes start-point-id :data)
        end (-> @shapes end-point-id :data)]
    (update-shape-property id :data {:start start :end end})
    (render-shape-by-id id)))

(defn- polygon-handler [response]
  (let [json (js/JSON.parse response)
        polygon (:polygon (js->clj json :keywordize-keys true))
        shape-data {:id :polygon
                    :renderer :polygon
                    :color 0x10f708
                    :visible true
                    :data polygon}]
    (add-shape! shape-data)))

(defn skeleton-handler [polygon-id skeleton-id]
  (fn [response]
    (let [json (js/JSON.parse response)
          edges (:skeleton (js->clj json :keywordize-keys true))
          shape-data {:id skeleton-id
                      :renderer :edges
                      :color 0x64dd17
                      :visible true
                      :data edges
                      :update-handler :rule
                      :update-rule {:command :compute-skeleton
                                    :args {:polygon polygon-id
                                           :skeleton skeleton-id}}}]
      (deps/add-dependencies (:id shape-data) #{polygon-id})
      (add-shape! shape-data)
      (render-shape-by-id (:id shape-data))
      (pixi/draw))))

(defn motorcycle-graph-handler [polygon-id mc-graph-id]
  (fn [response]
    (let [json (js/JSON.parse response)
          edges (:motorcycle_graph (js->clj json :keywordize-keys true))
          shape-data {:id mc-graph-id
                      :renderer :edges
                      :color 0xf57f17
                      :visible true
                      :data edges
                      :update-handler :rule
                      :update-rule {:command :compute-motorcycle-graph
                                    :args {:polygon polygon-id
                                           :motorcycle-graph mc-graph-id}}}]
      (deps/add-dependencies (:id shape-data) #{polygon-id})
      (add-shape! shape-data)
      (render-shape-by-id (:id shape-data))
      (pixi/draw))))
        
(defn polygon-offset-handler [polygon-id offset offset-polygon-id]
  (fn [response]
    (let [json (js/JSON.parse response)
          edges (:offset_polygon (js->clj json :keywordize-keys true))
          shape-data {:id offset-polygon-id
                      :renderer :edges
                      :color 0xf5da0a
                      :visible true
                      :data edges
                      :update-handler :rule
                      :update-rule {:command :compute-polygon-offset
                                    :args {:polygon polygon-id 
                                           :offset offset
                                           :offset-polygon offset-polygon-id}}}]
      (deps/add-dependencies (:id shape-data) #{polygon-id})
      (add-shape! shape-data)
      (render-shape-by-id (:id shape-data))
      (pixi/draw))))

(defn midpoint-handler [point-ids midpoint-id]
  (let [start (:start point-ids)
        end (:end point-ids)]
    (fn [response]
      (let [json (js/JSON.parse response)
            mp (js->clj json :keywordize-keys true)
            shape-data {:id midpoint-id
                        :renderer :point
                        :color 0xff4203
                        :visible true
                        :data mp
                        :update-handler :rule
                        :update-rule {:command :compute-midpoint
                                      :args {:points point-ids
                                             :midpoint midpoint-id}}}]
        (add-shape! shape-data)
        (deps/add-dependencies (:id shape-data) [start end])
        (render-shape-by-id (:id shape-data))
        (pixi/draw)))))

(defn ray-drawing-edge-handler [ray-spec ray-id]
  (let [origin (:origin ray-spec)
        ray-direction (:direction ray-spec)
        from (:from ray-direction)
        to (:to ray-direction)]
    (fn [response]
      (let [json (js/JSON.parse response)
            intersections-with-canvas-bbox (js->clj json :keywordize-keys true)
            ray-data (case (count intersections-with-canvas-bbox)
                       0 []
                       1 [{:start (-> @shapes origin :data)
                           :end (first intersections-with-canvas-bbox)}]
                       2 intersections-with-canvas-bbox)
            renderer (if (empty? intersections-with-canvas-bbox)
                       :noop
                       :edges)
            shape-data {:id ray-id
                        :renderer renderer
                        :color 0x234521
                        :visible true
                        :data ray-data
                        :update-on #{:canvas-change}
                        :update-handler :rule
                        :update-rule {:command :compute-ray-drawing-edge
                                      :args {:ray-spec ray-spec
                                             :ray ray-id}}}]
        (add-shape! shape-data)
        (deps/add-dependencies (:id shape-data) [origin from to :canvas-bbox])
        (render-shape-by-id (:id shape-data))
        (pixi/draw)))))            
        

(defn max-walkables-handler [polygon-id start-vertex-id max-walkables-id]
  (fn [response]
    (let [json (js/JSON.parse response)
          max-walkables (js->clj json :keywordize-keys true)
          shape-data {:id max-walkables-id
                      :renderer :max-walkables
                      :color 0x334423
                      :visible true
                      :data max-walkables
                      :update-handler :rule
                      :update-rule {:command :compute-max-walkables
                                    :args {:polygon polygon-id 
                                           :start start-vertex-id
                                           :max-walkables max-walkables-id}}}]
          (deps/add-dependencies (:id shape-data) #{polygon-id})
          (add-shape! shape-data)
          (render-shape-by-id (:id shape-data))
          (pixi/draw))))

(defn poly->nodes [polygon-id]
  (let [raw-node-ids (-> @shapes polygon-id :data)
        node-ids (map keyword raw-node-ids)
        nodes (map #(-> @shapes % :data) node-ids)]
    nodes))

(defn compute-skeleton [polygon-id skeleton-id]
  (let [nodes (poly->nodes polygon-id)
        handler (skeleton-handler polygon-id skeleton-id)]
    (comp/compute-skeleton nodes handler)))

(defn compute-motorcycle-graph [polygon-id mc-graph-id]
  (let [nodes (poly->nodes polygon-id)
        handler (motorcycle-graph-handler polygon-id mc-graph-id)]
    (comp/compute-motorcycle-graph nodes handler)))

(defn compute-polygon-offset [polygon-id offset offset-polygon-id]
  (let [nodes (poly->nodes polygon-id)
        handler (polygon-offset-handler polygon-id offset offset-polygon-id)]
    (comp/compute-polygon-offset nodes offset handler)))

(defn compute-midpoint [p1 p2 midpoint-id]
  (let [p1-coords (-> @shapes p1 :data)
        p2-coords (-> @shapes p2 :data)
        handler (midpoint-handler {:start p1 :end p2} midpoint-id)]
    (comp/compute-midpoint p1-coords p2-coords handler)))

(defn compute-max-walkables [polygon-id start-vertex-id max-walkables-id]
  (let [nodes (poly->nodes polygon-id)
        start (-> @shapes start-vertex-id :data)
        handler (max-walkables-handler polygon-id start-vertex-id max-walkables-id)]
    (comp/compute-max-walkables nodes start handler)))

(defn list-files [directory]
  (let [handler (fn [response] 
                  (let [json (js/JSON.parse response)]
                    (println "response: " json)))]
    (comp/list-files directory handler)))

(defn save-scene [directory filename]
  (let [handler (fn [response]
                  (let [json (js/JSON.parse response)]
                    (println "response: " json)))
        scene (scene-serialization)]
    (comp/save-scene scene directory filename handler)))

(defn load-scene [directory filename]
  (let [handler (fn [response]
                  (let [json (js/JSON.parse response)
                        scene (js->clj json :keywordize-keys true)]
                    (restore-scene scene)))]
    (comp/load-scene directory filename handler)))

(defn bbox->poly [bbox]
  (let [[root-x root-y] (:root bbox)
        width (:width bbox)
        height (:height bbox)
        min-y (- root-y height)
        max-x (+ root-x width)]
    [
     [root-x root-y]
     [root-x min-y]
     [max-x min-y]
     [max-x root-y]
     ]))

(defn compute-intersection-ray-bbox [ray-spec ray-id bbox]
  (let [origin (:data ((keyword (:origin ray-spec)) @shapes))
        ray-direction (:direction ray-spec)
        direction-from (:data ((keyword (:from ray-direction)) @shapes))
        direction-to (:data ((keyword (:to ray-direction)) @shapes))
        direction {:from direction-from :to direction-to}
        ray {:origin origin :direction direction}
        poly (bbox->poly bbox)
        segments (u/consecutive-pairs poly)
        handler (ray-drawing-edge-handler ray-spec ray-id)]
    (comp/compute-intersection-ray-segments ray segments handler)))

(defn compute-ray-drawing-edge [ray-spec ray-id]
  (compute-intersection-ray-bbox ray-spec ray-id (pixi/canvas-bounding-box)))
        

(defn add-to-polygon-nodes [poly-id node]
  (let [nodes (:data (poly-id @shapes))
        new-nodes (conj nodes node)]
    (update-shape poly-id :data new-nodes)))

(defn replace-node [old new shape]
  (let [data (-> @shapes shape :data)
        new-data (replace {old new} data)]
    (update-shape-property shape :data new-data)))

(defn move-point [id new-coords]
  (let [old-coords (-> @shapes id :data)
        update {:type :point-move
                :data {:id id
                       :old-coords old-coords
                       :new-coords new-coords
                       }
                }
        deps (into [id] (deps/dependent-shapes id))]
    (update-dependent-shapes id update)))

; event handling

(defn noop [event])

(defn mouse-to-canvas-coordinates [mouse-event]
  (let [canvas (.-view (:renderer @pixi/pixi-state))
        canvas-bound (.getBoundingClientRect canvas)
        canvas-x (.-left canvas-bound)
        canvas-y (.-top canvas-bound)
        event-x (.-clientX mouse-event)
        event-y (.-clientY mouse-event)
        [scale-x scale-y] (pixi/zoom-scale)
        [pan-x pan-y] (pixi/pan-position)
        in-canvas-x (- event-x canvas-x)
        in-canvas-y (- event-y canvas-y)
        x (/ (- in-canvas-x (* pan-x scale-x)) scale-x)
        y (- (/ (- in-canvas-y (- (* pan-y scale-y))) scale-y))]
    [x y]))

; commands
(defn compute-segment-points-command [args]
  (let [start (keyword (:start args))
        end (keyword (:end args))]
    (update-segment (keyword (:id args)) start end)))

(defn compute-midpoint-command [args]
  (let [points (:points args)]
    (compute-midpoint (keyword (:start points)) (keyword (:end points)) (keyword (:midpoint args)))))

(defn compute-ray-drawing-edge-command [args]
  (compute-ray-drawing-edge (:ray-spec args) (:ray args)))

(defn compute-skeleton-command [args]
  (compute-skeleton (keyword (:polygon args)) (keyword (:skeleton args))))

(defn compute-motorcycle-graph-command [args]
  (compute-motorcycle-graph (keyword (:polygon args)) (keyword (:motorcycle-graph args))))

(defn compute-polygon-offset-command [args]
  (compute-polygon-offset (keyword (:polygon args)) (:offset args) (keyword (:offset-polygon args))))

(defn compute-max-walkables-command [args]
  (compute-max-walkables (keyword (:polygon args)) (keyword (:start args)) (keyword (:max-walkables args))))

(defn add-command! [key command-handler]
  (swap! commands assoc key command-handler))

; event handling
(defonce event-handlers
  (atom {:click {}
         :mousemove {}}))

(defn add-event-handler [event-key handler-key handler]
  (swap! event-handlers assoc-in [event-key handler-key] handler))

(defn remove-event-handler [event-key handler-key]
  (swap! event-handlers update-in [event-key] dissoc handler-key))

(defn ray-drawing-receiver [state]
  (let [[origin direction-from direction-to] (:points state)
        ray-spec {:origin origin 
                  :direction {:from direction-from
                              :to direction-to}}
        ray-id (keyword (id-gen/next-id :ray))]
    (compute-ray-drawing-edge ray-spec ray-id)
    (pixi/draw)))

(defn box-zooming-receiver [state]
  (let [[[x1 y1] [x2 y2]] (:points state)
        width (- x2 x1)
        height (- y1 y2)]
    (fit-view-to-box {:root [x1 y1] :width width :height height})))

(defn segment-drawing-receiver [state]
  (let [[start end] (:points state)]
    (make-segment start end)
    (pixi/draw)))

(defn generic-points-collect-handler [config state]
  (fn [event-data]
    (let [coordinates (mouse-to-canvas-coordinates event-data)
          new-state ((:coordinates-processor config) state coordinates)
          done? ((:done? config) new-state)]
      (if done?
        (do
          (remove-event-handler :click (:handler-key config))
          ((:final-state-processor config) new-state))
        (add-event-handler :click (:handler-key config) (generic-points-collect-handler config new-state))))))

(defn nr-points-done [nr-points]
  (fn [state]
    (let [points (:points state)]
      (= (count points) nr-points))))

(defn coords-to-point-ids [state coordinates]
  (let [point (make-point-or-get-existing coordinates)
        new-points (conj (:points state) point)]
    (pixi/draw)
    (assoc state :points new-points)))

(defn collect-points-handler [nr-points-to-collect points-receiver handler-key]
  (let [config {
                :handler-key handler-key
                :coordinates-processor coords-to-point-ids
                :done? (nr-points-done nr-points-to-collect)
                :final-state-processor points-receiver}
        state {:points []}]
    (generic-points-collect-handler config state)))

(defn mouse-coordinates-handler-for-dom-element [selector]
  (let [elem (dom/sel1 selector)]
    (fn [event-data]
      (let [[x y] (mouse-to-canvas-coordinates event-data)]
        (dom/set-text! elem (str "(" x "," y ")"))))))

(defn show-mouse-coordinates-handler []
  (mouse-coordinates-handler-for-dom-element :#coords))

(defn close-points-handler-for-dom-element [selector]
  (let [elem (dom/sel1 selector)]
    (fn [event-data]
      (let [coords (mouse-to-canvas-coordinates event-data)
            cps (close-points coords)]
        (if (empty? cps)
          (dom/set-text! elem "no close points")
          (let [points-with-coords (map (fn [p] [p (-> @shapes p :data)]) cps)
                _ (println "points-with-coords: " points-with-coords)
                _ (doall points-with-coords)]
            (dom/set-text! elem points-with-coords)))))))

(defn show-close-points-handler []
  (close-points-handler-for-dom-element :#close-points))

(defn event-dispatcher [event-key]
  (fn [event]
    (let [handlers (event-key @event-handlers)]
      (doseq [[_ handler] handlers]
        (handler event)))))

(defn click-dispatcher []
  (event-dispatcher :click))

(defn mousemove-dispatcher []
  (event-dispatcher :mousemove))

(defn enable-clicks []
  (let [renderer (:renderer @pixi/pixi-state)
        canvas (.-view renderer)]
  (.addEventListener canvas "click" (click-dispatcher))))

(defn enable-mousemove []
  (let [renderer (:renderer @pixi/pixi-state)
        canvas (.-view renderer)]
    (.addEventListener canvas "mousemove" (mousemove-dispatcher))))

; drawing
(defn stop-drawing! []
  (remove-event-handler :click :draw))

(defn stop-drawing-points! []
  (remove-event-handler :click :draw-points))

(defn start-drawing-points! []
  (let [config {
                :handler-key :draw-points
                :coordinates-processor coords-to-point-ids
                :done? (fn [] false)
                :final-state-processor #()}
        state {:points []}]
  (add-event-handler :click (:handler-key config) (generic-points-collect-handler config state))))

(defn start-drawing-segment! []
  (let [handler-key :draw-segment
        handler (collect-points-handler 2 segment-drawing-receiver handler-key)]
    (add-event-handler :click handler-key handler)))

(defn start-drawing-ray! []
  (let [handler-key :draw-ray
        handler (collect-points-handler 3 ray-drawing-receiver handler-key)]
  (add-event-handler :click handler-key handler)))

(defn start-point-move! []
  (let [config {
                :handler-key :point-move
                :coordinates-processor (fn [state coords]
                                         (if (nil? (:point state))
                                           (assoc state :point (find-matching-point coords))
                                           (assoc state :target-coords coords)))
                :done? (fn [state] (not-any? nil? [(:point state) (:target-coords state)]))
                :final-state-processor (fn [state] (move-point (:point state) (:target-coords state)))}
        state {}]
  (add-event-handler :click (:handler-key config) (generic-points-collect-handler config state))))

(defn start-drawing-polygon! [id]
  (let [coords-processor (fn [state coords]
                           (let [point (make-point-or-get-existing coords)]
                             (if (nil? (:start state))
                               (let [new-state (assoc state 
                                                      :start point
                                                      :previous point
                                                      :vertices [point])]
                                 (pixi/draw)
                                 new-state)
                               (let [state-with-completed (if (= point (:start state))
                                                            (assoc state :completed true)
                                                            state)
                                     state-with-previous (assoc state-with-completed :previous point)
                                     new-vertices (conj (:vertices state) point)]
                                 (make-segment (:previous state) point)
                                 (pixi/draw)
                                 (if (:completed state-with-previous)
                                   state-with-previous
                                   (assoc state-with-previous :vertices new-vertices))))))
        finalizer (fn [state] (create-polygon-shape (:polygon state) (:vertices state)))
        config {
                :handler-key :draw-polygon
                :coordinates-processor coords-processor
                :done? #(:completed %)
                :final-state-processor finalizer
                }
        state {:polygon id}]
  (add-event-handler :click (:handler-key config) (generic-points-collect-handler config state))))

(defn start-box-zoom! []
  (let [config {
                :handler-key :box-zoom
                :coordinates-processor (fn [state coords] 
                                         (let [new-points (conj (:points state) coords)]
                                           (assoc state :points new-points)))
                :done? (nr-points-done 2)
                :final-state-processor box-zooming-receiver}
        state {:points []}
        handler (generic-points-collect-handler config state)]
  (add-event-handler :click (:handler-key config) handler)))

(defn start-showing-mouse-coordinates []
  (add-event-handler :mousemove :show-mouse-coords (show-mouse-coordinates-handler)))

(defn stop-showing-mouse-coordinates []
  (remove-event-handler :mousemove :show-mouse-coords))

(defn start-showing-close-points []
  (add-event-handler :mousemove :show-close-points (show-close-points-handler)))

(defn stop-showing-close-points []
  (remove-event-handler :mousemove :show-close-points))

; initializations

(defonce is-initialized
  (atom false))

(defn add-canvas-bbox-dependency []
  (deps/add-dependencies :canvas-bbox #{}))

(defn init-update-handlers []
  (add-update-handler! :point point-update-handler)
  (add-update-handler! :polygon polygon-update-handler)
  (add-update-handler! :rule rule-update-handler))

(defn init-commands []
  (add-command! :compute-segment-points compute-segment-points-command)
  (add-command! :compute-midpoint compute-midpoint-command)
  (add-command! :compute-ray-drawing-edge compute-ray-drawing-edge-command)
  (add-command! :compute-skeleton compute-skeleton-command)
  (add-command! :compute-motorcycle-graph compute-motorcycle-graph-command)
  (add-command! :compute-polygon-offset compute-polygon-offset-command)
  (add-command! :compute-max-walkables compute-max-walkables-command))

(defn init-id-generator-states []
  (id-gen/add-key :point 1)
  (id-gen/add-key :segment 1)
  (id-gen/add-key :ray 1))

(defn init []
  (if-not @is-initialized
    (do
      (swap! is-initialized (fn [_] true))
      (enable-console-print!)
      (enable-clicks)
      (enable-mousemove)
      (init-id-generator-states)
      (pixi/init-renderers)
      (init-update-handlers)
      (init-commands)
      (add-canvas-bbox-dependency)
      (start-showing-mouse-coordinates)
      (start-showing-close-points))))

(init)

(defn sel1 [selector]
  (dom/sel1 selector))
