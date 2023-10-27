(ns gpu.webgpu.core
  (:require [shadow.cljs.modern :refer [js-await]]))

(defn get-adapter []
  (when-not js/navigator.gpu
    (throw "CLJSGPU: this browser doesn't support WebGPU! :("))
  (js/navigator.gpu.requestAdapter))

(defn get-device
  ([adapter]
   ^js (.requestDevice adapter))
  ([]
   (.then (get-adapter)
          #(get-device %))))

(def preferred-presentation-format
  (memoize
   (fn []
     (js/navigator.gpu.getPreferredCanvasFormat))))

(defn create-context [device canvas]
  (let [context (.getContext canvas "webgpu")]
    ^js (.configure
         context
         (clj->js
          {:device device
           :format (preferred-presentation-format)}))
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