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
       fragment (fragment
                 [pixel-position {:type vec4f
                                  :builtin position}]
                 {:location 0
                  :type vec4f}
                 (let pos (- (* 2 (/ pixel-position.xy resolution)) 1))
                 (let mag (distance pos (vec2f 0)))
                 (vec4f (select (vec3f 0)
                                (vec3f 1)
                                (< mag 0.5))
                        1))}})))

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
