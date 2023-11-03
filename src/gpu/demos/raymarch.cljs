(ns gpu.demos.raymarch
  (:require [gpu.util :as u]
            [gpu.webgpu.core :refer [start-monocanvas-sketch!
                                     purefrag-shader
                                     create-render-pipeline
                                     create-buffer
                                     create-bind-group
                                     pipeline-layout
                                     set-pass-pipeline
                                     set-pass-bind-group
                                     purefrag-render-pass
                                     create-command-encoder
                                     finish-command-encoder
                                     write-buffer]]
            [gpu.dom.canvas :refer [maximize-canvas
                                    context-resolution]]
            [gpu.wort.core :refer [wort->wgsl]]))

(def render-shader
  (purefrag-shader
   (wort->wgsl
    '{:bindings [[uniform resolution vec2f
                  uniform time f32]]
      :structs {Ray [pos vec3f
                     dir vec3f]}
      :functions {sdf ([pos vec3f]
                       f32
                       (- (distance
                           (+ pos
                              (vec3f (* 0.65 (sin (* 1 (+ time pos.y))))
                                     (* 0.1 (sin (* 7 (+ time pos.z))))
                                     (* 0.4 (sin (* 3 (+ time pos.x))))))
                           (vec3f 0))
                          6))}
      :fragment ([pixel-position {:type vec4f
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
                 (vec4f 0 0 0 1))})))

(defn update-sketch [device
                     context
                     {:keys [resolution-buffer
                             time-buffer
                             pipeline
                             bind-group]
                      :as state}]
  (maximize-canvas context.canvas)
  (write-buffer device
                resolution-buffer
                (js/Float32Array.
                 (context-resolution context)))
  (write-buffer device
                time-buffer
                (js/Float32Array. [(u/seconds-since-startup)]))
  (let [encoder (create-command-encoder device)]
    (purefrag-render-pass encoder
                          context
                          #(-> %
                               (set-pass-pipeline pipeline)
                               (set-pass-bind-group 0 bind-group)))
    (finish-command-encoder encoder device))
  state)

(defn init-sketch [device context]
  (let [pipeline (create-render-pipeline device {:shader render-shader})
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
    {:pipeline pipeline
     :resolution-buffer resolution-buffer
     :time-buffer time-buffer
     :bind-group bind-group}))

(defn init []
  (start-monocanvas-sketch! init-sketch update-sketch))
