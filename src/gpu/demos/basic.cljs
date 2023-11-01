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
    '{:uniforms [[resolution vec2f
                  time f32]]
      :structs {Ray [pos vec3f
                     dir vec3f]}
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
                            (vec3f (* 0.65 (sin (* 1 (+ time pos.y))))
                                   (* 0.1 (sin (* 7 (+ time pos.z))))
                                   (* 0.4 (sin (* 3 (+ time pos.x))))))
                         (vec3f 0))
               6))
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
        (const light-pos (vec3f -8 -6 -20))
        (let mag (distance pos (vec2f 0)))
        (var ray (Ray (vec3f 0 0 -10)
                      (normalize (vec3 pos 1))))
        (var hit-surface false)
        (for (var i 0) (< i 64) (++ i)
             (let dist (sdf ray.pos))
             (if (< (abs dist) 0.01)
               (:block (= hit-surface true)
                       break)
               (+= ray.pos (* 0.95 dist ray.dir))))
        (when hit-surface
          (let surface-dot
            (dot (normalize
                  (vec3f (- (sdf (+ ray.pos (vec3f 0.001 0 0)))
                            (sdf (- ray.pos (vec3f 0.001 0 0))))
                         (- (sdf (+ ray.pos (vec3f 0 0.001 0)))
                            (sdf (- ray.pos (vec3f 0 0.001 0))))
                         (- (sdf (+ ray.pos (vec3f 0 0 0.001)))
                            (sdf (- ray.pos (vec3f 0 0 0.001))))))
                 (normalize (- light-pos ray.pos))))
          (return
           (vec4f
            (vec3f
             (pow (+ (mix 0.15
                          0.9
                          (clamp surface-dot 0 1))
                     (* 0.1 (smoothstep 0.95 0.99 surface-dot)))
                  2.2))
            1)))
        (vec4f 0 0 0 1))}})))

(defn sketch-loop [{:keys [ctx
                           resolution-buffer
                           time-buffer
                           device
                           pipeline
                           bind-group]
                    :as state}]
  (maximize-canvas ctx.canvas)
  (write-buffer device
                resolution-buffer
                (js/Float32Array.
                 (ctx-resolution ctx)))
  (write-buffer device
                time-buffer
                (js/Float32Array. [(u/seconds-since-startup)]))
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
        time-buffer (create-buffer device
                                   #{:uniform :copy-dst}
                                   {:size 4})
        bind-group (create-bind-group device
                                      (pipeline-layout pipeline)
                                      [resolution-buffer
                                       time-buffer])]
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
                  :time-buffer time-buffer
                  :bind-group bind-group})))

(defn init []
  (.then (get-device)
         sketch-start))
