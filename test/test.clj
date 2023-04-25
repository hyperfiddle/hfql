(ns test
  (:require [hyperfiddle.rcf :as rcf]
            [clojure.test :as t]))

(defn -main [& args]
  (require 'hyperfiddle.hfql-tests)
  (alter-var-root #'rcf/*generate-tests* (constantly true))
  (require 'hyperfiddle.hfql-tests :reload)
  (let [report (t/run-tests 'hyperfiddle.hfql-tests)]
    (System/exit (if (zero? (:fail report)) 0 1))))
