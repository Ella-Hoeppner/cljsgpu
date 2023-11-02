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
                                     queue-render-pass
                                     current-ctx-texture
                                     tex-view
                                     write-buffer]]
            [gpu.dom.canvas :refer [maximize-canvas
                                    ctx-resolution]]
            [gpu.wort.core :refer [wort->wgsl]]))

(def grid-size 100)

(def compute-shader-wgsl
  (wort->wgsl
   '{:bindings [[(storage read) size vec2u
                 (storage read) current [array u32]
                 (storage read-write) next [array u32]]]
     :functions
     {get-index ([x u32
                  y u32]
                 u32
                 (+ (* (% y size.y)
                       size.x)
                    (% x size.x)))
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
                     (u32 (== neighbors "3u"))
                     (u32 (|| (== neighbors "2u")
                              (== neighbors "3u"))))))}}))

(def render-shader-wgsl
  (wort->wgsl
   '{:bindings [[uniform resolution vec2f
                 uniform time f32]]
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
       (vec4f pos 0 1))}}))

(defn sketch-loop [{:keys [ctx
                           resolution-buffer
                           time-buffer
                           device
                           render-pipeline
                           render-bind-group
                           compute-pipeline]
                    :as state}]
  (maximize-canvas ctx.canvas)

  (let [command-encoder ^js (.createCommandEncoder device)
        pass-encoder ^js (.beginComputePass command-encoder)]
    (.setPipeline pass-encoder compute-pipeline)
    )
  
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
                          (set-pass-pipeline render-pipeline)
                          (set-pass-bind-group 0 render-bind-group))
                     6)
  (js/requestAnimationFrame (partial sketch-loop state)))

(defn sketch-start [device]
  (let [ctx (create-context-canvas device)
        render-module (create-module device render-shader-wgsl)
        render-pipeline (create-render-pipeline device {:module render-module})
        resolution-buffer (create-buffer device
                                         #{:uniform :copy-dst}
                                         {:size 8})
        time-buffer (create-buffer device
                                   #{:uniform :copy-dst}
                                   {:size 4})
        render-bind-group (create-bind-group device
                                             (pipeline-layout render-pipeline)
                                             [resolution-buffer
                                              time-buffer])

        initial-cells (js/Uint32Array.
                       (repeatedly (* grid-size grid-size)
                                   #(if (> (rand) 0.5) 1 0)))
        grid-buffers (u/genv 2 (create-buffer device
                                              #{:storage}
                                              {:size initial-cells.byteLength}))
        size-buffer (create-buffer device
                                   #{:uniform :storage :copy-dst}
                                   {:size 8})
        compute-module (create-module device compute-shader-wgsl)
        compute-pipeline ^js (.createComputePipeline
                              device
                              (clj->js {:layout "auto"
                                        :compute {:module compute-module
                                                  :entryPoint "compute"}}))]
    (sketch-loop {:ctx ctx
                  :device device
                  :render-pipeline render-pipeline
                  :compute-pipeline compute-pipeline
                  :resolution-buffer resolution-buffer
                  :time-buffer time-buffer
                  :render-bind-group render-bind-group})))

(defn init []
  (.then (get-device)
         sketch-start))
