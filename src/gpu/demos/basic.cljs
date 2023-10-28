(ns gpu.demos.basic
  (:require [gpu.util :as u]
            [gpu.webgpu.core :refer [get-device
                                     create-context-canvas
                                     preferred-presentation-format]]
            [shadow.cljs.modern :refer [js-await]]
            [gpu.dom.canvas :refer [maximize-canvas
                                    canvas-resolution]]))

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
    (let [module ^js (.createShaderModule device
                                          (clj->js
                                           {:code shader-code}))
          pipeline
          ^js (.createRenderPipeline
               device
               (clj->js
                {:layout "auto"
                 :vertex {:module module
                          :entryPoint "vertex"}
                 :fragment {:module module
                            :entryPoint "fragment"
                            :targets [{:format
                                       (preferred-presentation-format)}]}}))
          resolution-buffer
          ^js (.createBuffer device
                             (clj->js
                              {:size 8
                               :usage (bit-or js/GPUBufferUsage.UNIFORM
                                              js/GPUBufferUsage.COPY_DST)}))]
      ^js (.writeBuffer device.queue
                        resolution-buffer
                        0
                        (js/Float32Array. (canvas-resolution ctx.canvas)))
      (let [bind-group
            ^js (.createBindGroup device
                                  (clj->js
                                   {:layout (.getBindGroupLayout pipeline 0)
                                    :entries
                                    [{:binding 0
                                      :resource {:buffer resolution-buffer}}]}))
            render-pass-descriptor (clj->js
                                    {:colorAttachments
                                     [{:clearValue [0.3 0.3 0.3 1]
                                       :loadOp "clear"
                                       :storeOp "store"}]})]
        (set! (.-view (aget render-pass-descriptor.colorAttachments 0))
              (.createView ^js (.getCurrentTexture ctx)))
        (let [encoder ^js (.createCommandEncoder device)
              pass (.beginRenderPass encoder render-pass-descriptor)]
          (.setPipeline pass pipeline)
          (.setBindGroup pass 0 bind-group)
          (.draw pass 6)
          (.end pass)
          (.submit device.queue (clj->js [(.finish encoder)])))))))

(defn init []
  (.then (get-device)
         start-sketch))
