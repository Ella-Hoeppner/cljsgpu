(ns gpu.demos.conway
  (:require [gpu.util :as u]
            [gpu.webgpu.core :refer [purefrag-shader
                                     start-monocanvas-sketch!
                                     create-render-pipeline
                                     create-compute-pipeline
                                     create-buffer
                                     create-bind-group
                                     pipeline-layout
                                     set-pass-pipeline
                                     set-pass-bind-group
                                     purefrag-render-pass
                                     compute-pass
                                     create-command-encoder
                                     finish-command-encoder
                                     current-context-texture
                                     tex-view
                                     write-buffer]]
            [gpu.dom.canvas :refer [maximize-canvas
                                    context-resolution]]
            [gpu.wort.core :refer [wort->wgsl]]
            [gpu.wort.tools :refer-macros [unquotable]]))

(def grid-size 200)
(def workgroup-size 8)

(def compute-shader-wgsl
  (unquotable
   (wort->wgsl
    '{:bindings [[(storage read) current [array u32]
                  (storage read-write) next [array u32]]]
      :functions {get-index ([x u32
                              y u32]
                             u32
                             (+ (* (% y ~grid-size)
                                   ~grid-size)
                                (% x ~grid-size)))
                  get-cell ([x u32
                             y u32]
                            u32
                            [current (get-index x y)])
                  count-neighbors ([x u32
                                    y u32]
                                   u32
                                   (+ (get-cell (- x 1) (- y 1))
                                      (get-cell x (- y 1))
                                      (get-cell (+ x 1) (- y 1))
                                      (get-cell (+ x 1) y)
                                      (get-cell (+ x 1) (+ y 1))
                                      (get-cell x (+ y 1))
                                      (get-cell (- x 1) (+ y 1))
                                      (get-cell (- x 1) y)))}
      :compute ([8 8]
                [grid {:type vec3u
                       :builtin global-invocation-id}]
                nil
                (let neighbors (count-neighbors grid.x grid.y))
                (= [next (get-index grid.x grid.y)]
                   (? (== (get-cell grid.x grid.y) "1u")
                      (u32 (|| (== neighbors "2u")
                               (== neighbors "3u")))
                      (u32 (== neighbors "3u")))))})))

(def render-shader-wgsl
  (unquotable
   (purefrag-shader
    (wort->wgsl
     '{:bindings [[uniform resolution vec2f
                   (storage read) grid [array u32]]]
       :fragment ([pixel-position {:type vec4f
                                   :builtin position}]
                  {:location 0
                   :type vec4f}
                  (let resolution-min (min resolution.x resolution.y))
                  (let pos (/ (- pixel-position.xy
                                 (* 0.5 (- resolution resolution-min)))
                              resolution-min))
                  (? (&& (>= pos.x 0)
                         (< pos.x 1)
                         (>= pos.y 0)
                         (< pos.y 1))
                     (vec4f (vec3f (f32
                                    [grid (+ (i32 (* pos.x ~grid-size))
                                             (* ~grid-size
                                                (i32 (* pos.y ~grid-size))))]))
                            1)
                     (vec4f 0 0 0 1)))}))))

(defn update-sketch [device
                     context
                     {:keys [resolution-buffer
                             render-pipeline
                             render-bind-groups
                             compute-pipeline
                             compute-bind-groups]
                      :as state}]
  (maximize-canvas context.canvas)

  (write-buffer device
                resolution-buffer
                (js/Float32Array.
                 (context-resolution context)))

  (let [encoder (create-command-encoder device)]
    (compute-pass encoder
                  #(-> %
                       (set-pass-pipeline compute-pipeline)
                       (set-pass-bind-group 0 (first compute-bind-groups)))
                  [(/ grid-size workgroup-size)
                   (/ grid-size workgroup-size)])
    (purefrag-render-pass
     encoder
     [(tex-view (current-context-texture context))]
     #(-> %
          (set-pass-pipeline render-pipeline)
          (set-pass-bind-group 0 (first render-bind-groups))))
    (finish-command-encoder encoder device))
  (-> state
      (update :compute-bind-groups reverse)
      (update :render-bind-groups reverse)))

(defn init-sketch [device context]
  (let [render-pipeline (create-render-pipeline device
                                                {:shader render-shader-wgsl})
        compute-pipeline (create-compute-pipeline device
                                                  {:shader compute-shader-wgsl})
        resolution-buffer (create-buffer device
                                         #{:uniform :copy-dst}
                                         {:size 8})
        initial-cells (js/Uint32Array.
                       (repeatedly (* grid-size grid-size)
                                   #(if (> (rand) 0.5) 1 0)))
        grid-buffers (u/gen 2 (create-buffer device
                                             #{:storage :copy-dst}
                                             {:size initial-cells.byteLength}))

        render-bind-groups (map #(create-bind-group device
                                                    (pipeline-layout
                                                     render-pipeline)
                                                    [resolution-buffer
                                                     %])
                                grid-buffers)
        compute-bind-groups (map #(create-bind-group
                                   device
                                   (pipeline-layout compute-pipeline)
                                   (vec %))
                                 [grid-buffers (reverse grid-buffers)])]
    (write-buffer device
                  (first grid-buffers)
                  initial-cells)
    {:render-pipeline render-pipeline
     :compute-pipeline compute-pipeline
     :resolution-buffer resolution-buffer
     :render-bind-groups render-bind-groups
     :compute-bind-groups compute-bind-groups}))

(defn init []
  (start-monocanvas-sketch! init-sketch update-sketch))
