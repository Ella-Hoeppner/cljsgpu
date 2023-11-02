(ns gpu.webgpu.core
  (:require [shadow.cljs.modern :refer [js-await]]))

(defn default [m key default-value]
  (update m key #(or % default-value)))

(defn get-adapter []
  (when-not js/navigator.gpu
    (throw (js/Error. "CLJSGPU: this browser doesn't support WebGPU! :(")))
  (js/navigator.gpu.requestAdapter))

(defn get-device
  ([adapter]
   ^js (.requestDevice adapter))
  ([]
   (.then (get-adapter)
          #(get-device %))))

(def preferred-canvas-format
  (memoize
   (fn []
     (js/navigator.gpu.getPreferredCanvasFormat))))

(defn create-context [device canvas]
  (let [context (.getContext canvas "webgpu")]
    ^js (.configure
         context
         (clj->js
          {:device device
           :format (preferred-canvas-format)}))
    context))

(defn create-context-canvas [device
                             & [{:keys [id
                                        append-to-body?]
                                 :or {append-to-body? true}}]]
  (let [canvas (js/document.createElement "canvas")]
    (when id
      (set! (.-id canvas) (str id)))
    (when append-to-body?
      (set! (.-position canvas.style) "absolute")
      (.appendChild js/document.body canvas))
    (create-context device canvas)))

(defn create-module [device code & [options]]
  ^js (.createShaderModule device
                           (clj->js
                            (assoc options :code code))))

(defn create-render-pipeline [device & [{:keys [module wgsl]
                                         :as options}]]
  ^js
  (.createRenderPipeline
   device
   (clj->js
    (let [default-module (or module
                             (when wgsl
                               (create-module device wgsl)))]
      (-> options
          (dissoc :module :wgsl)
          (update :layout
                  #(or % "auto"))
          (update :fragment
                  (fn [fragment]
                    (-> fragment
                        (default :entryPoint "fragment")
                        (default :module default-module)
                        (default :targets [{:format
                                            (preferred-canvas-format)}]))))
          (update :vertex
                  (fn [vertex]
                    (-> vertex
                        (default :entryPoint "vertex")
                        (default :module default-module)))))))))

(defn create-compute-pipeline [device & [{:keys [module wgsl]
                                          :as options}]]
  ^js
  (.createComputePipeline
   device
   (clj->js
    (let [default-module (or module
                             (when wgsl
                               (create-module device wgsl)))]
      (-> options
          (dissoc :module :wgsl)
          (update :layout
                  #(or % "auto"))
          (update :compute
                  (fn [compute]
                    (-> compute
                        (default :entryPoint "compute")
                        (default :module default-module)
                        (default :targets [{:format
                                            (preferred-canvas-format)}])))))))))

(def buffer-usage-map
  {:uniform js/GPUBufferUsage.UNIFORM
   :copy-dst js/GPUBufferUsage.COPY_DST
   :copy-src js/GPUBufferUsage.COPY_SRC
   :index js/GPUBufferUsage.INDEX
   :indirect js/GPUBufferUsage.INDIRECT
   :map-read js/GPUBufferUsage.MAP_READ
   :map-write js/GPUBufferUsage.MAP_WRITE
   :query-resolve js/GPUBufferUsage.QUERY_RESOLVE
   :storage js/GPUBufferUsage.STORAGE
   :vertex js/GPUBufferUsage.VERTEX})

(defn write-buffer [device buffer data
                    & [{:keys [buffer-offset data-offset size]}]]
  ^js (.writeBuffer device.queue
                    buffer
                    (or buffer-offset 0)
                    data
                    data-offset
                    (or size js/undefined)))

(defn create-buffer [device usage & [{:keys [size data] :as options}]]
  (let [buffer
        ^js (.createBuffer
             device
             (clj->js
              (merge
               (dissoc options :data)
               {:size (or size
                          (when data
                            (.-byteLength data)))
                :usage (cond
                         (keyword? usage) (buffer-usage-map usage)
                         (set? usage) (reduce #(bit-or %1 (buffer-usage-map %2))
                                              0
                                              usage)
                         :else usage)})))]
    (when data
      (write-buffer device buffer data))
    buffer))

(defn create-command-encoder [device & [label]]
  ^js (.createCommandEncoder device (clj->js {:label label})))

(defn create-bind-group [device layout resources & [label]]
  ^js (.createBindGroup
       device
       (clj->js
        {:label label
         :layout layout
         :entries (mapv (fn [index resource]
                          {:binding index
                           :resource (if (= (type resource) js/GPUBuffer)
                                       {:buffer resource}
                                       resource)})
                        (range)
                        resources)})))

(defn pipeline-layout [pipeline & [index]]
  ^js (.getBindGroupLayout pipeline (or index 0)))

(defn device-default-encoder [device]
  (or device.defaultEncoder
      (set! device.defaultEncoder (create-command-encoder device))))

(defn compute-pass [command-encoder callback workgroup-count]
  (let [pass ^js (.beginComputePass command-encoder)]
    (callback pass)
    (if (number? workgroup-count)
      (.dispatchWorkgroups pass workgroup-count)
      (let [[x y z] (concat workgroup-count (repeat 1))]
        (.dispatchWorkgroups pass x y z)))
    (.end pass)))

(defn render-pass [command-encoder color-attachements callback vertices
                   & [options]]
  (let [pass
        ^js (.beginRenderPass command-encoder
                              (clj->js
                               (assoc options
                                      :colorAttachments
                                      (mapv #(-> (if (= (type %)
                                                        js/GPUTextureView)
                                                   {:view %}
                                                   %)
                                                 (default :loadOp :clear)
                                                 (default :storeOp :store))
                                            color-attachements))))]
    (callback pass)
    (.draw pass vertices)
    (.end pass)))

(defn finish-command-encoder [encoder & [queue-or-device]]
  (let [completion (.finish encoder)]
    (if queue-or-device
      (let [queue (if (= (type queue-or-device) js/GPUDevice)
                    queue-or-device.queue
                    queue-or-device)]
        (.submit queue (clj->js [completion])))
      completion)))

(defn set-pass-pipeline [pass pipeline]
  ^js (.setPipeline pass pipeline)
  pass)

(defn set-pass-bind-group [pass index bind-group]
  ^js (.setBindGroup pass index bind-group)
  pass)

(defn current-context-texture [context]
  ^js (.getCurrentTexture context))

(defn tex-view [tex & [options]]
  ^js (.createView tex (clj->js options)))

(defn start-sketch! [init-fn update-fn & [{:keys [device]}]]
  (let [start-from-device (fn [device]
                            (let [initial-state (when init-fn
                                                  (init-fn device))]
                              (when update-fn
                                ((fn loop-fn [state]
                                   (js/requestAnimationFrame
                                    (partial loop-fn
                                             (update-fn device
                                                        state))))
                                 initial-state))))]
    (if device
      (start-from-device device)
      (.then (get-device) start-from-device))))

(defn start-monocanvas-sketch! [init-fn update-fn & [{:keys [context
                                                             device]
                                                      :as options}]]
  (let [start-from-device
        (fn [device]
          (let [canvas-context (or context
                                   (create-context-canvas device options))
                initial-state (when init-fn
                                (init-fn device
                                         canvas-context))]
            (when update-fn
              ((fn loop-fn [state]
                 (js/requestAnimationFrame
                  (partial loop-fn
                           (update-fn device
                                      canvas-context
                                      state))))
               initial-state))))]
    (if device
      (start-from-device device)
      (.then (get-device) start-from-device))))
