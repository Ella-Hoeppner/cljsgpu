(ns gpu.demos.clifford
  (:require [gpu.util :as u]
            [gpu.webgpu.core :refer [start-monocanvas-sketch!
                                     create-render-pipeline
                                     create-compute-pipeline
                                     create-buffer
                                     auto-bind-group
                                     simple-compute-pass
                                     simple-render-pass
                                     create-command-encoder
                                     finish-command-encoder
                                     write-buffer]]
            [gpu.dom.canvas :refer [maximize-canvas
                                    context-resolution]]
            [gpu.wort.core :refer [wort->wgsl]]
            [gpu.wort.tools :refer-macros [unquotable]]))

(def point-grid-size [1000 1000])
(def point-count (apply * point-grid-size))
(def workgroup-size 10)

(def compute-shader-wgsl
  (unquotable
   (wort->wgsl
    '{:bindings [[(storage read) current [array vec2f]
                  (storage read-write) next [array vec2f]]]
      :functions {clifford
                  ([x vec2f
                    a f32
                    b f32
                    c f32
                    d f32]
                   vec2f
                   (vec2f (+ (sin (* a x.y))
                             (* c (cos (* a x.x))))
                          (+ (sin (* b x.x))
                             (* d (cos (* b x.y))))))}
      :compute ([~workgroup-size ~workgroup-size]
                [id {:type vec3u
                     :builtin global-invocation-id}]
                nil
                (let index (+ id.x (* id.y ~(first point-grid-size))))
                (= [next index]
                   (clifford [current index]
                             -1.4
                             1.6
                             1
                             0.7)))})))

(def render-shader-wgsl
  (unquotable
   (wort->wgsl
    '{:bindings [[uniform resolution vec2f
                  (storage read) grid [array vec2f]]]
      :vertex ([vertex-index {:type u32
                              :builtin vertex-index}]
               {:builtin position
                :type vec4f}
               (let corner-index (% vertex-index 6))
               (let point-index (/ vertex-index 6))
               (vec4f (* (+ (* 0.5
                               [grid point-index])
                            (* 0.0005
                               [(array (vec2f -1 -1)
                                       (vec2f 1 -1)
                                       (vec2f -1 1)
                                       (vec2f 1 1)
                                       (vec2f 1 -1)
                                       (vec2f -1 1))
                                corner-index]))
                         (/ (min resolution.x resolution.y)
                            resolution))
                      0
                      1))
      :fragment ([pixel-position {:type vec4f
                                  :builtin position}]
                 {:location 0
                  :type vec4f}
                 (let resolution-min (min resolution.x resolution.y))
                 (let pos (- (* 2
                                (/ (- pixel-position.xy
                                      (* 0.5 (- resolution resolution-min)))
                                   resolution-min))
                             1))
                 (vec4f (vec3f (pow (- 1 (/ (length pos))) 0.5)) 1))})))

(defn update-sketch [device
                     context
                     {:keys [resolution-buffer
                             render-pipeline
                             compute-pipeline
                             point-buffers]
                      :as state}]
  (maximize-canvas context.canvas)
  (write-buffer device
                resolution-buffer
                (js/Float32Array. (context-resolution context)))
  (-> (create-command-encoder device)
      (simple-compute-pass compute-pipeline
                           (auto-bind-group device
                                            compute-pipeline
                                            (vec point-buffers))
                           (mapv #(/ % workgroup-size) point-grid-size))
      (simple-render-pass context
                          render-pipeline
                          (auto-bind-group device
                                           render-pipeline
                                           [resolution-buffer
                                            (first point-buffers)])
                          (* point-count 6))
      (finish-command-encoder device))
  (-> state
      (update :point-buffers reverse)))

(defn init-sketch [device context]
  {:render-pipeline (create-render-pipeline device
                                            {:shader render-shader-wgsl})
   :compute-pipeline (create-compute-pipeline device
                                              {:shader compute-shader-wgsl})
   :resolution-buffer (create-buffer device
                                     #{:uniform :copy-dst}
                                     {:size 8})
   :point-buffers (u/gen 2 (create-buffer device
                                          #{:storage :copy-dst}
                                          {:data
                                           (js/Float32Array.
                                            (repeatedly (* 2 point-count)
                                                        rand))}))})

(defn init []
  (start-monocanvas-sketch! init-sketch update-sketch))
