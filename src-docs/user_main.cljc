(ns user-main
  (:require clojure.string
            contrib.ednish
            contrib.uri ; data_readers
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.history :as history]
            [user.teeshirt-orders :refer [Webview-HFQL]]))

(defn route->path [route] (clojure.string/join "/" (map contrib.ednish/encode-uri route)))
(defn path->route [s]
  (let [s (contrib.ednish/discard-leading-slash s)]
    (case s "" nil (->> (clojure.string/split s #"/") (mapv contrib.ednish/decode-uri)))))

(e/defn Main []
  (binding [history/encode route->path
            history/decode #(or (path->route %) [`user.teeshirt-orders/Webview-HFQL])]
    (history/router (history/HTML5-History.)
      (set! (.-title js/document) (some-> (identity history/route) first name))
      (binding [dom/node js/document.body]
        (history/router 1 ; focus on first slot
          (Webview-HFQL.))))))
