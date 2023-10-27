(ns gpu.demos.basic
  (:require [gpu.util :as u]
            [gpu.webgpu.core :refer [get-device
                                     create-context-canvas
                                     preferred-presentation-format]]
            [shadow.cljs.modern :refer [js-await]]
            [gpu.dom.canvas :refer [maximize-canvas]]))

(defn start-sketch [device]
  (let [ctx (create-context-canvas device)]
    (maximize-canvas ctx.canvas)))

(defn init []
  (u/log "hello world!")
  (.then (get-device)
         start-sketch))
