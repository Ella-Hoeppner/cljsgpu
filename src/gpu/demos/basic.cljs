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
   }")

(defn start-sketch [device]
  (let [ctx (create-context-canvas device)]
    (maximize-canvas ctx.canvas
                     {:max-pixel-ratio 1})
    (let [module (create-module device shader-code)
          pipeline (create-render-pipeline device {:module module})
          resolution-buffer (create-buffer device
                                           8
                                           #{:uniform :copy-dst}
                                           {:data (js/Float32Array.
                                                   (ctx-resolution ctx))})
          bind-group (create-bind-group device
                                        (pipeline-layout pipeline)
                                        [resolution-buffer])]
      (js/console.log (.createView
                       ^js (.getCurrentTexture ctx)))
      (queue-device-render-pass device
                                [(tex-view (current-ctx-texture ctx))]
                                #(-> %
                                     (set-pass-pipeline pipeline)
                                     (set-pass-bind-group 0 bind-group))
                                6))))

(defn init []
  (.then (get-device)
         start-sketch))
