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
                                     queue-device-render-pass
                                     current-ctx-texture
                                     tex-view]]
            [gpu.dom.canvas :refer [maximize-canvas
                                    ctx-resolution]]))

(def shader-code
  "@group(0) @binding(0) var<uniform> resolution: vec2f;
   @vertex fn vertex(
     @builtin(vertex_index) vertexIndex : u32
   ) -> @builtin(position) vec4f {
     let pos = array(
       vec2f( -1.0, -1.0),
       vec2f( 1.0, -1.0),
       vec2f( -1.0, 1.0),
       vec2f( 1.0, 1.0),
       vec2f( 1.0, -1.0),
       vec2f( -1.0, 1.0)
     );
     return vec4f(pos[vertexIndex], 0.0, 1.0);
   }
   @fragment fn fragment(
     @builtin(position) pixelPosition : vec4f
   ) -> @location(0) vec4f {
     return vec4f(pixelPosition.xy/resolution, 0, 1);
   }"
  #_'{:uniforms [[resolution vec2f]]
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
                 {:builtin position
                  :type vec4f}
                 (vec4f (/ pixel-position.xy resolution)
                        0
                        1))}})

(defn start-sketch [device]
  (let [ctx (create-context-canvas device)]
    (maximize-canvas ctx.canvas)
    (let [module (create-module device shader-code)
          pipeline (create-render-pipeline device {:module module})
          resolution-buffer (create-buffer device
                                           #{:uniform :copy-dst}
                                           {:data (js/Float32Array.
                                                   (ctx-resolution ctx))})
          bind-group (create-bind-group device
                                        (pipeline-layout pipeline)
                                        [resolution-buffer])]
      (queue-device-render-pass device
                                [(tex-view (current-ctx-texture ctx))]
                                #(-> %
                                     (set-pass-pipeline pipeline)
                                     (set-pass-bind-group 0 bind-group))
                                6))))

(defn init []
  (.then (get-device)
         start-sketch))
