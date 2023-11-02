(ns gpu.dom.canvas)

(defn maximize-canvas [canvas & {:keys [max-pixel-ratio aspect-ratio]}]
  (let [raw-width js/window.innerWidth
        raw-height js/window.innerHeight
        pixel-ratio (if max-pixel-ratio
                      (min js/window.devicePixelRatio max-pixel-ratio)
                      js/window.devicePixelRatio)
        style canvas.style]
    (if aspect-ratio
      (let [height (Math/floor (min (/ raw-width aspect-ratio) raw-height))
            width (Math/floor (* height aspect-ratio))]
        (set! (.-left style) (* (- raw-width width) 0.5))
        (set! (.-top style) (* (- raw-height height) 0.5))
        (set! (.-width style) (str width "px"))
        (set! (.-height style) (str height "px"))
        (set! (.-width canvas) (* width pixel-ratio))
        (set! (.-height canvas) (* height pixel-ratio)))
      (let [[width height] (mapv (partial * pixel-ratio)
                                 [raw-width raw-height])]
        (set! (.-left style) 0)
        (set! (.-top style) 0)
        (set! (.-width style) (str raw-width "px"))
        (set! (.-height style) (str raw-height "px"))
        (set! (.-width canvas) width)
        (set! (.-height canvas) height)))))

(defn resize-canvas [canvas pixel-dimensions & {:keys [max-pixel-ratio]}]
  (let [pixel-ratio (if max-pixel-ratio
                      (min js/window.devicePixelRatio max-pixel-ratio)
                      js/window.devicePixelRatio)
        style canvas.style
        [width height] (if (number? pixel-dimensions)
                         [pixel-dimensions pixel-dimensions]
                         pixel-dimensions)
        raw-width js/window.innerWidth
        raw-height js/window.innerHeight
        [style-width style-height] (mapv #(/ % pixel-ratio) [width height])]
    (set! (.-left style) (max 0
                              (* 0.5 (- raw-width style-width))))
    (set! (.-top style) (max 0
                             (* 0.5 (- raw-height style-height))))
    (set! (.-width style) (str (/ width pixel-ratio) "px"))
    (set! (.-height style) (str (/ height pixel-ratio) "px"))
    (set! (.-width canvas) width)
    (set! (.-height canvas) height)))

(defn canvas-resolution [canvas]
  [canvas.width canvas.height])

(defn context-resolution [context]
  (canvas-resolution context.canvas))