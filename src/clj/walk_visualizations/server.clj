(ns walk-visualizations.server
  (:require [clojure.java.io :as io]
            [compojure.core :refer [ANY GET PUT POST DELETE defroutes]]
            [compojure.route :refer [resources]]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.middleware.gzip :refer [wrap-gzip]]
            [ring.middleware.logger :refer [wrap-with-logger]]
            [environ.core :refer [env]]
            [ring.adapter.jetty :refer [run-jetty]]
            [clojure.data.json :as json]
            [http.async.client :as http]
            [zeromq.zmq :as zmq])
  (:gen-class))

(defonce zmq-context (zmq/context 1))

(defonce computers
  (atom {
         :cpp-server "tcp://localhost:5555"
         :rust-server "tcp://localhost:5556"
         }))

(defonce computation-registry
  (atom {
         :straight_skeleton :cpp-server
         :polygon_offset :cpp-server
         :midpoint :cpp-server
         :max_walkables :cpp-server
         :intersect_ray_segments :cpp-server
         :motorcycle_graph :rust-server
         :list_files :rust-server
         :load_scene :rust-server
         :save_scene :rust-server
         }))

(defn- zmq-call [target message]
  (let [context (zmq/context 1)]
    (with-open [socket (doto (zmq/socket context :req)
                         (zmq/connect target))]
      (zmq/send-str socket message)
      (zmq/receive-str socket))))

(defroutes routes
  (GET "/" _
    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (io/input-stream (io/resource "public/index.html"))})
  (POST "/computation/:command" request
        (let [command (keyword (-> request :params :command))
              target ((command @computation-registry) @computers)
              computation-request (slurp (:body request))
              result (zmq-call target computation-request)]
          {:status 200
           :headers {"Content-Type" "text/javascript; charset=utf-8"}
           :body result}))
  (resources "/"))

(def http-handler
  (-> routes
      (wrap-defaults api-defaults)
      wrap-with-logger
      wrap-gzip))

(defn -main [& [port]]
  (let [port (Integer. (or port (env :port) 10555))]
    (run-jetty http-handler {:port port :join? false})))
