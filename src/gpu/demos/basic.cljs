(ns gpu.demos.basic
  (:require [gpu.util :as u]
            [gpu.webgpu.core :refer [get-device
                                     create-module
                                     create-render-pipeline
                                     create-context-canvas
                                     create-buffer
                                     create-bind-group
                                     pipeline-layout
                                     set-pass-pipeline
                                     set-pass-bind-group
                                     queue-render-pass
                                     current-ctx-texture
                                     tex-view
                                     write-buffer]]
            [gpu.dom.canvas :refer [maximize-canvas
                                    ctx-resolution]]
            [gpu.wort.core :refer [wort->wgsl]]))

(def shader-code
  (u/log
   (wort->wgsl
    '{:uniforms [[resolution vec2f]]
      :functions
      {vertex (vertex
               [vertex-index {:type u32
                              :builtin vertex-index}]
               {:builtin position
                :type vec4f}
               (let pos
                 (array (vec2f -1 -1)
                        (vec2f 1 -1)
                        (vec2f -1 1)
                        (vec2f 1 1)
                        (vec2f 1 -1)
                        (vec2f -1 1)))
               (vec4f [pos vertex-index] 0 1))
       sdf ([pos vec3f]
            f32
            (- (distance (+ pos
                            (vec3f 0 0 (* 1 (sin (* 5 pos.x)))))
                         (vec3f 0))
               4))
       fragment
       (fragment
        [pixel-position {:type vec4f
                         :builtin position}]
        {:location 0
         :type vec4f}
        (let resolution-min (min resolution.x resolution.y))
        (let pos (- (* 2 (/ (- pixel-position.xy
                               (* 0.5 (- resolution resolution-min)))
                            resolution-min))
                    1))
        (const light-pos (vec3f -4 -4 -20))
        (let mag (distance pos (vec2f 0)))
        (var ray-pos (vec3f 0 0 -10))
        (let ray-dir (normalize (vec3 pos 1)))
        (var hit-surface false)
        (for (var i 0) (< i 100) (++ i)
             (let dist (sdf ray-pos))
             (if (< dist 0.0025)
               (:block (= hit-surface true)
                       break)
               (+= ray-pos (* 0.75 dist ray-dir))))
        (when hit-surface
          (return
           (vec4f
            (vec3f
             (pow (mix 0.25
                       1
                       (clamp
                        (dot (normalize
                              (vec3f (- (sdf (+ ray-pos (vec3f 0.001 0 0)))
                                        (sdf (- ray-pos (vec3f 0.001 0 0))))
                                     (- (sdf (+ ray-pos (vec3f 0 0.001 0)))
                                        (sdf (- ray-pos (vec3f 0 0.001 0))))
                                     (- (sdf (+ ray-pos (vec3f 0 0 0.001)))
                                        (sdf (- ray-pos (vec3f 0 0 0.001))))))
                             (normalize (- light-pos ray-pos)))
                        0
                        1))
                  2.2))
            1)))
        (vec4f 0 0 0 1))}})))

(defn sketch-loop [{:keys [ctx resolution-buffer device pipeline bind-group]
                    :as state}]
  (maximize-canvas ctx.canvas)
  (write-buffer device
                resolution-buffer
                (js/Float32Array.
                 (ctx-resolution ctx)))
  (queue-render-pass device
                     [(tex-view (current-ctx-texture ctx))]
                     #(-> %
                          (set-pass-pipeline pipeline)
                          (set-pass-bind-group 0 bind-group))
                     6)
  (js/requestAnimationFrame (partial sketch-loop state)))

(defn sketch-start [device]
  (let [ctx (create-context-canvas device)
        module (create-module device shader-code)
        pipeline (create-render-pipeline device {:module module})
        resolution-buffer (create-buffer device
                                         #{:uniform :copy-dst}
                                         {:size 8})
        bind-group (create-bind-group device
                                      (pipeline-layout pipeline)
                                      [resolution-buffer])]
    (queue-render-pass device
                       [(tex-view (current-ctx-texture ctx))]
                       #(-> %
                            (set-pass-pipeline pipeline)
                            (set-pass-bind-group 0 bind-group))
                       6)
    (sketch-loop {:ctx ctx
                  :device device
                  :module module
                  :pipeline pipeline
                  :resolution-buffer resolution-buffer
                  :bind-group bind-group})))

(defn init []
  (.then (get-device)
         sketch-start))
