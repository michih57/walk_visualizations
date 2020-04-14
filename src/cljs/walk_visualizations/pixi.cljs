(ns walk-visualizations.pixi
  (:require [cljsjs.pixi]))

(def canvas-width 800)
(def canvas-height 600)

(defn- mk-renderer [] 
  (let [renderer (js/PIXI.autoDetectRenderer 800 600)]
    (.appendChild (.-body js/document) (.-view renderer))
    renderer))

(defn- mk-stage [] (js/PIXI.Container.))

(defn- mk-graphics [] (js/PIXI.Graphics.))

(defonce pixi-state (atom {:renderer (mk-renderer)
                           :stage (mk-stage)
                           :graphics {}}))

(defonce renderers
  (atom {}))

(defn- add-renderer! [key renderer]
  (swap! renderers assoc key renderer))

(defn- animate[]
  (let [renderer (:renderer @pixi-state)
        stage (:stage @pixi-state)]
    (.render renderer stage)))

(defn draw []
  (do
    (js/requestAnimationFrame animate)
    (animate)))

(defn add-graphics! [id]
  (if-let [existing-graphics (get-in @pixi-state [:graphics id])]
    (.removeChild (:stage @pixi-state) existing-graphics))
  (let [graphics (mk-graphics)]
    (.addChild (:stage @pixi-state) graphics)
    (swap! pixi-state assoc-in [:graphics id] graphics)))

(defn remove-graphics! [id]
  (let [graphics (get-in @pixi-state [:graphics id])]
    (swap! pixi-state update-in [:graphics] dissoc id)
    (.removeChild (:stage @pixi-state) graphics)))

(defn- invert-y-coordinate [[x y]]
  [x (- y)])

;; zoom/pan ;;

(defn zoom-scale []
  (let [scale (.-scale (:stage @pixi-state))
        x (.-x scale)
        y (.-y scale)]
    [x y]))

(defn zoom-scale-avg []
  (let [[x y] (zoom-scale)]
    (/ (+ x y) 2)))

(defn pan! [shift]
  (let [scale (zoom-scale-avg)
        [x-shift y-shift] (invert-y-coordinate shift)
        position (.-position (:stage @pixi-state))]
    (set! (.-x position) (+ (.-x position) (* scale x-shift)))
    (set! (.-y position) (+ (.-y position) (* scale y-shift)))
    (draw)))

(defn reset-pan! []
  (let [position (.-position (:stage @pixi-state))]
    (set! (.-x position) 0)
    (set! (.-y position) 0)
    (draw)))

(defn pan-position []
  (let [position (.-position (:stage @pixi-state))
        [scale-x scale-y] (zoom-scale)
        x (/ (.-x position) scale-x)
        y (/ (.-y position) scale-y)]
    (invert-y-coordinate [x y])))

(defn zoom! [factor]
  (let [scale-point (.-scale (:stage @pixi-state))]
    (set! (.-x scale-point) (* (.-x scale-point) factor))
    (set! (.-y scale-point) (* (.-y scale-point) factor))
    (draw)))

(defn zoom-with-center-reference! [scale]
  (let [[scale-x scale-y] (zoom-scale)
        [pan-x pan-y] (pan-position)
        old-center-x-offset (/ (/ canvas-width 2) scale-x)
        old-center-y-offset (/ (/ canvas-height 2) scale-y)
        center-x (+ (- pan-x) (/ (/ canvas-width 2) scale-x))
        center-y (- (- pan-y) (/ (/ canvas-height 2) scale-y))
        new-center-x-offset (/ (/ canvas-width 2) (* scale scale-x))
        new-center-y-offset (/ (/ canvas-height 2) (* scale scale-y))
        new-pan-x (- (- center-x new-center-x-offset))
        new-pan-y (- (+ center-y new-center-y-offset))]
    (reset-pan!)
    (zoom! scale)
    (pan! [new-pan-x new-pan-y])))

(defn reset-zoom! []
  (let [scale-point (.-scale (:stage @pixi-state))]
    (set! (.-x scale-point) 1)
    (set! (.-y scale-point) 1)
    (draw)))

(defn canvas-bounding-box []
  (let [[leftx topy] (map - (pan-position))
        [scalex scaley] (zoom-scale)
        rightx (/ (+ (* leftx scalex) canvas-width) scalex)
        bottomy (/ (+ (* topy scaley) canvas-height) scaley)
        width (- rightx leftx)
        height (- bottomy topy)
        ]
    {:root [leftx topy] :width width :height height}))

;; rendering ;;

(defn clear-shape [id]
  (let [graphics (get-in @pixi-state [:graphics id])]
    (.removeChildren graphics)
    (.clear graphics)))

(defn render-shape [id data color renderer]
  (let [renderer (renderer @renderers)
        graphics (get-in @pixi-state [:graphics id])]
    (clear-shape id)
    (renderer data color graphics)))

(defn hide-shape [id]
  (clear-shape id))

(defn draw-circle! [data color graphics]
  (let [scale (first (zoom-scale))
        radius (/ (:radius data) scale)
        [x y] (invert-y-coordinate (:center data))]
    (.beginFill graphics color 1)
    (.drawCircle graphics x y radius)
    (.endFill graphics)))

(defn setup-linestyle! [graphics width color]
  (let [actual-width (/ width (first (zoom-scale)))]
    (.lineStyle graphics actual-width color 1)))

(defn draw-text [text position graphics]
  (let [fontsize (/ 20 (first (zoom-scale)))
        font (str fontsize "px" " " "Arial")
        pixi-text (js/PIXI.Text. text (clj->js {:stroke "#fcff00" :fill "#00FF00" :font font}))
        [x y] (invert-y-coordinate position)]
    (set! (.-x pixi-text) x)
    (set! (.-y pixi-text) y)
    (.addChild graphics pixi-text)))

(defn- draw-edge [graphics edge]
  (let [{start :start end :end} edge
        [startx starty] (invert-y-coordinate start)
        [endx endy] (invert-y-coordinate end)]
    (.moveTo graphics startx starty)
    (.lineTo graphics endx endy)))

(defn- render-semi-wedge! [wedge graphics]
  (draw-edge graphics {:start (:p wedge) :end (:q wedge)})
  (draw-circle! {:radius 4 :center (:p wedge)} 0xff7f00 graphics)
  (draw-circle! {:radius 4 :center (:q wedge)} 0xff7f00 graphics))

(defn- render-backward-deadlock! [deadlock graphics]
  (draw-circle! {:radius 4 :center (:p deadlock)} 0xdc143c graphics)
  (draw-circle! {:radius 4 :center (:q deadlock)} 0xdc143c graphics))

(defn- render-forward-deadlock! [deadlock index graphics]
  (draw-text (str "f_" index) (:p deadlock) graphics)
  (draw-text (str "f_" index) (:q deadlock) graphics)
  (draw-circle! {:radius 4 :center (:p deadlock)} 0xff33cc graphics)
  (draw-circle! {:radius 4 :center (:q deadlock)} 0xff33cc graphics)
  (draw-edge graphics {:start (:p deadlock) :end (:q deadlock)}))

(defn- render-point! [data color graphics]
  (let [nodesize 3]
    (.clear graphics)
    (draw-circle! {:radius nodesize :center data} color graphics)))

(defn- render-segment! [data color graphics]
  (.clear graphics)
  (setup-linestyle! graphics 2 color)
  (draw-edge graphics data))

(defn- render-max-walkables! [data color graphics]
  (let [s (:s data)
        max-walkables (:max_walkables data)
        constraints (:constraints data)
        semi-wedges (:semi_wedges constraints)]
    (.clear graphics)
    (setup-linestyle! graphics 2 color)
    (draw-circle! {:radius 5 :center s} color graphics)
    (draw-text "s" s graphics)
    (doseq [[idx max-walkable] (map-indexed vector max-walkables)]
      (let [l-max (:l_max max-walkable)
            r-max (:r_max max-walkable)]
        (draw-edge graphics {:start l-max :end r-max})
        (draw-circle! {:radius 4 :center l-max} color graphics)
        (draw-text (str "l_max_" idx) l-max graphics)
        (draw-circle! {:radius 4 :center r-max} color graphics)
        (draw-text (str "r_max_" idx) r-max graphics)))
    (if (:cw_semi_wedge semi-wedges)
      (render-semi-wedge! (:cw_semi_wedge semi-wedges) graphics))
    (if (:ccw_semi_wedge semi-wedges)
      (render-semi-wedge! (:ccw_semi_wedge semi-wedges) graphics))
    (if (:backward_deadlock constraints)
      (render-backward-deadlock! (:backward_deadlock constraints) graphics))
    (if (:forward_deadlocks constraints)
      (doseq [[idx forward-deadlock] (map-indexed vector (:forward_deadlocks constraints))]
        (render-forward-deadlock! forward-deadlock idx graphics)))
    ))

(defn- render-noop [data color graphics]
  ; do nothing
  )

(defn- render-edges! [data color graphics]
  (.clear graphics)
  (setup-linestyle! graphics 2 color)
  (doseq [edge data]
    (draw-edge graphics edge)))

(defn init-renderers []
  (add-renderer! :point render-point!)
  (add-renderer! :segment render-segment!)
  (add-renderer! :max-walkables render-max-walkables!)
  (add-renderer! :polygon render-noop)
  (add-renderer! :edges render-edges!)
  (add-renderer! :noop render-noop))
