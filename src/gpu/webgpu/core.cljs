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

(defn create-render-pipeline [device & [{:keys [module]
                                         :as options}]]
  ^js
  (.createRenderPipeline
   device
   (clj->js
    (-> options
        (dissoc module)
        (update :layout
                #(or % "auto"))
        (update :fragment
                (fn [fragment]
                  (-> fragment
                      (default :entryPoint "fragment")
                      (default :module module)
                      (default :targets [{:format
                                          (preferred-canvas-format)}]))))
        (update :vertex
                (fn [vertex]
                  (-> vertex
                      (default :entryPoint "vertex")
                      (default :module module))))))))

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

(defn queue-render-pass [device color-attachements callback vertices
                         & [{:keys [queue]
                             :as options}]]
  (let [encoder (create-command-encoder device)
        pass
        ^js (.beginRenderPass encoder
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
    (.end pass)
    (.submit (or queue device.queue) (clj->js [(.finish encoder)]))))

(defn set-pass-pipeline [pass pipeline]
  ^js (.setPipeline pass pipeline)
  pass)

(defn set-pass-bind-group [pass index bind-group]
  ^js (.setBindGroup pass index bind-group)
  pass)

(defn current-ctx-texture [ctx]
  ^js (.getCurrentTexture ctx))

(defn tex-view [tex & [options]]
  ^js (.createView tex (clj->js options)))