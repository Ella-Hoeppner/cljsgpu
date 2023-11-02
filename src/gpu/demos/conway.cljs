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

(def shader-code
  (u/log
   (wort->wgsl
    '{:bindings [[uniform resolution vec2f
                  uniform time f32]
                 [(storage read) size vec2f
                  (storage read) current [array u32]
                  (storage read-write) next [array u32]]]
      :functions
      {conway (compute
               [8 8]
               [index {:type u32
                       :builtin local-invocation-index}]
               nil
               (for (var i u32 0) (< i 10) (++ i)
                    (when (> (+ index i) 7)
                      break)))
       
       vertex (vertex
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
        (vec4f pos 0 1))}})))

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
