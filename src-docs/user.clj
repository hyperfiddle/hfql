(ns user
  (:gen-class)
  (:require [user.teeshirt-orders]))

(def electric-server-config
  {:host           "0.0.0.0"
   :port           8080
   :resources-path "public/hyperfiddle/hfql"
   :manifest-path  "public/hyperfiddle/hfql/js/manifest.edn"})

(declare server)

(def start-server! (delay @(requiring-resolve 'electric-server-java11-jetty10/start-server!)))
(def VERSION (delay @(requiring-resolve 'electric-server-java11-jetty10/VERSION)))

(defn -main [& _args]
  (println "Starting HFQL demo server...")
  (println "Version: " @VERSION)
  (def server (@start-server! electric-server-config)))

(comment
  (.stop server)
  )
