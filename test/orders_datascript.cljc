(ns orders-datascript
  "query functions used in tee-shirt orders demo"
  (:require [clojure.spec.alpha :as s]
            clojure.string
            contrib.str
            [datascript.core :as d]
            [hyperfiddle.api :as hf]
            [hyperfiddle.rcf :refer [tap % tests]])
  #?(:cljs (:require-macros orders-datascript)))

(s/fdef genders :args (s/cat) :ret (s/coll-of number?))
(defn genders []
  (into [] (sort (d/q '[:find [?e ...] :where [_ :order/gender ?e]] hf/*$*))))

(s/fdef shirt-sizes :args (s/cat :gender keyword?
                                 :needle string?)
        :ret (s/coll-of number?))

(defn shirt-sizes [gender needle]
  ; resolve db/id and db/ident genders to same entity
  ; datomic does this transparently
  ; datascript does not
  (sort
    (if gender
      (d/q '[:in $ ?gender ?needle ?includes-str? ; injected for cljs compat: no runtime resolve
             :find [?e ...]
             :where
             [?e :order/type :order/shirt-size]
             [?e :order/gender ?g]
             [?g :db/ident ?gender]
             [?e :db/ident ?ident]  ; remove
             [(name ?ident) ?nm]
             [(?includes-str? ?nm ?needle)]]
           hf/*$*
           gender (or needle "") contrib.str/includes-str?)
      (d/q '[:in $ ?needle ?includes-str?
             :find [?e ...]
             :where
             [?e :order/type :order/shirt-size]
             [?e :db/ident ?ident]
             [(name ?ident) ?nm]
             [(?includes-str? ?nm ?needle)]]
           hf/*$*
           (or needle "")
           contrib.str/includes-str?))))

(tests
  (shirt-sizes :order/female #_2 "") := [6 7 8]
  (shirt-sizes :order/female #_2 "med") := [7]
  (shirt-sizes :order/female #_2 "d") := [7])

(defn orders [needle]
  (sort (d/q '[:find [?e ...] :in $ ?needle :where
               [?e :order/email ?email]
               [(clojure.string/includes? ?email ?needle)]]
             hf/*$* (or needle ""))))

(tests
  (orders "") := [9 10 11]
  (orders "example") := [9 10 11]
  (orders "b") := [10])

(s/fdef orders :args (s/cat :needle string?)
        :ret (s/coll-of (s/keys :req [:order/email
                                      :order/email1
                                      :order/gender
                                      :order/shirt-size])))

(s/fdef order :args (s/cat :needle string?) :ret number?)
(defn order [needle] (first (orders needle)))

(tests
  (order "") := 9
  (order "bob") := 10)

(s/fdef one-order :args (s/cat :sub any?) :ret any?)
(defn one-order [sub] (hf/*nav!* hf/*$* sub :db/id))

