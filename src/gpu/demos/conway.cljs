(ns gpu.demos.conway
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
                                     render-pass
                                     create-command-encoder
                                     finish-command-encoder
                                     current-ctx-texture
                                     tex-view
                                     write-buffer]]
            [gpu.dom.canvas :refer [maximize-canvas
                                    ctx-resolution]]
            [gpu.wort.core :refer [wort->wgsl]]))

(def grid-size 80)
(def workgroup-size 8)

(def compute-shader-wgsl
  (wort->wgsl
   '{:bindings [[(storage read) current [array u32]
                 (storage read-write) next [array u32]]]
     :functions
     {get-index ([x u32
                  y u32]
                 u32
                 (+ (* (% y 80)
                       80)
                    (% x 80)))
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
                          (get-cell (- x 1) y)))
      compute (compute
               [8 8]
               [grid {:type vec3u
                      :builtin global-invocation-id}]
               nil
               (let neighbors (count-neighbors grid.x grid.y))
               (= [next (get-index grid.x grid.y)]
                  (? (== (get-cell grid.x grid.y) "1u")
                     (u32 (|| (== neighbors "2u")
                              (== neighbors "3u")))
                     (u32 (== neighbors "3u")))))}}))

(def render-shader-wgsl
  (wort->wgsl
   '{:bindings [[uniform resolution vec2f
                 uniform time f32
                 (storage read) grid [array u32]]]
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
      fragment
      (fragment
       [pixel-position {:type vec4f
                        :builtin position}]
       {:location 0
        :type vec4f}
       (= _ time)
       (let resolution-min (min resolution.x resolution.y))
       (let pos (/ (- pixel-position.xy
                      (* 0.5 (- resolution resolution-min)))
                   resolution-min))
       (? (&& (>= pos.x 0)
              (< pos.x 1)
              (>= pos.y 0)
              (< pos.y 1))
          (vec4f (vec3f (f32 [grid (+ (i32 (* pos.x 80))
                                      (* 80 (i32 (* pos.y 80))))]))
                 1)
          (vec4f 0 0 0 1)))}}))

(defn sketch-loop [{:keys [ctx
                           resolution-buffer
                           time-buffer
                           device
                           render-pipeline
                           render-bind-groups
                           compute-pipeline
                           compute-bind-groups]
                    :as state}]
  (maximize-canvas ctx.canvas)
  
  (write-buffer device
                resolution-buffer
                (js/Float32Array.
                 (ctx-resolution ctx)))
  (write-buffer device
                time-buffer
                (js/Float32Array. [(u/seconds-since-startup)]))

  (let [encoder (create-command-encoder device)
        compute-pass-encoder ^js (.beginComputePass encoder)]
    (.setPipeline compute-pass-encoder compute-pipeline)
    (.setBindGroup compute-pass-encoder 0 (first compute-bind-groups))
    (.dispatchWorkgroups compute-pass-encoder
                         (/ grid-size workgroup-size)
                         (/ grid-size workgroup-size))
    (.end compute-pass-encoder)
    (render-pass encoder
                 [(tex-view (current-ctx-texture ctx))]
                 #(-> %
                      (set-pass-pipeline render-pipeline)
                      (set-pass-bind-group 0 (first render-bind-groups)))
                 6)
    (finish-command-encoder encoder device))
  (js/requestAnimationFrame
   (partial sketch-loop
            (-> state
                (update :compute-bind-groups reverse)
                (update :render-bind-groups reverse)))))

(defn sketch-start [device]
  (let [ctx (create-context-canvas device)
        render-module (create-module device render-shader-wgsl)
        render-pipeline (create-render-pipeline device {:module render-module})
        compute-module (create-module device compute-shader-wgsl)
        compute-pipeline ^js (.createComputePipeline
                              device
                              (clj->js {:layout "auto"
                                        :compute {:module compute-module
                                                  :entryPoint "compute"}}))
        resolution-buffer (create-buffer device
                                         #{:uniform :copy-dst}
                                         {:size 8})
        time-buffer (create-buffer device
                                   #{:uniform :copy-dst}
                                   {:size 4})
        initial-cells (js/Uint32Array.
                       (repeatedly (* grid-size grid-size)
                                   #(if (> (rand) 0.5) 1 0)))
        grid-buffers (u/gen 2 (create-buffer device
                                             #{:storage :copy-dst}
                                             {:size initial-cells.byteLength}))

        render-bind-groups (map #(create-bind-group device
                                                    (pipeline-layout render-pipeline)
                                                    [resolution-buffer
                                                     time-buffer
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
    (sketch-loop {:ctx ctx
                  :device device
                  :render-pipeline render-pipeline
                  :compute-pipeline compute-pipeline
                  :resolution-buffer resolution-buffer
                  :time-buffer time-buffer
                  :render-bind-groups render-bind-groups
                  :compute-bind-groups compute-bind-groups})))

(defn init []
  (.then (get-device)
         sketch-start))
