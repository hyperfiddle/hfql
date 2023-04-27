(ns hyperfiddle.hfql-tests
  (:require
   [hyperfiddle.api :as hf]
   [hyperfiddle.hfql :as hfql :refer [hfql]]
   [hyperfiddle.electric :as p]
   [hyperfiddle.rcf :as rcf :refer [tests with tap %]]
   [datascript.core :as d]
   [clojure.string :as str]
   [clojure.spec.alpha :as s]
   [example-datascript-db :refer [nav! get-schema]]
   [orders-datascript :refer [orders order shirt-sizes one-order]]
   )
  (:import [hyperfiddle.electric Pending])
  #?(:cljs (:require-macros [hyperfiddle.hfql-tests :refer [debug]])))

(comment
  (rcf/enable! true))

(defmacro debug [& body]
  `(try ~@body
        (catch hyperfiddle.electric.Pending e# (throw e#))
        (catch missionary.Cancelled e# (throw e#))
        (catch ~(if (:js-globals &env) :default 'Throwable) e#
          (prn (type e#) (ex-message e#) (ex-data e#) e#) (throw e#))))

(tests
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!]
                      (hfql [] :db/id 9)))))
  % := 9)

(tests
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!]
                      (hfql [] [:db/id] 9)))))
  % := {:db/id 9})

(p/def String-renderer (p/fn [{::hf/keys [Value]}] (str (new Value))))

(tests
  "hf/render"
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!
                              hfql/Render hfql/EdnRender]
                      (hfql [] (props :db/id {::hf/render String-renderer}) 9) ))))
  % := "9")

(tests
  "hf/render inline"
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!
                              hfql/Render hfql/EdnRender]
                      (hfql [] (props :db/id {::hf/render (p/fn [{::hf/keys [Value]}] (str (new Value)))}) 9)))))
  % := "9")

(tests
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!
                              hfql/Render hfql/EdnRender]
                      (hfql [] [(props :db/id {::hf/render String-renderer})] 9)))))
  % := {:db/id "9"})

(tests
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!
                              hf/*schema* get-schema]
                      (hfql [] [{:order/gender [:db/ident]}] 9) ))))
  % := {:order/gender {:db/ident :order/female}})

(tests
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!]
                      (hfql (order ""))  ))))
  % := 9)

(tests
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!]
                      (hfql {(order "") [:db/id]}) ))))
  % := {`(order "") {:db/id 9}})

(tests
  "Two levels of nesting"
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!
                              hf/*schema* get-schema]
                      (hfql {(order "") [{:order/shirt-size [:db/ident]}]}) ))))
  % := {`(order "") {:order/shirt-size {:db/ident :order/womens-large}}})

(tests
  "multiplicity many"
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!]
                      (hfql {(orders "") [:db/id]}) ))))
  % := {`(orders "") [{:db/id 9} {:db/id 10} {:db/id 11}]})

(tests
  (with (p/run (tap (binding [hf/db hf/*$*
                              hf/*nav!* nav!
                              hfql/Render hfql/EdnRender]
                      (hfql {(orders "") [(props :db/id {::hf/render String-renderer})]})))))
  % := {`(orders "") [{:db/id "9"} {:db/id "10"} {:db/id "11"}]})

(p/defn Throwing-renderer [ctx] (prn "BOOM") (throw (ex-info "I fail" {})))
(p/defn Ignoring-renderer [ctx] "ignored")

(tests
  (with (p/run (tap (binding [hf/db       hf/*$*
                              hf/*nav!*   nav!
                              hf/*schema* get-schema
                              hfql/Render hfql/EdnRender]
                      (hfql [] [{(props :order/gender {::hf/render Ignoring-renderer})
                                 [(props :db/ident {::hf/render Throwing-renderer})]}]
                        9) ))))
  % := {:order/gender "ignored"} ; note it didn’t throw
  )

;; Insight: Let the renderer decide of the options' continuation.
(p/defn Select-option-renderer [{::hf/keys [Value options continuation] :as ctx}]
  (into [:select {:value (hfql/JoinAllTheTree. ctx)}]
    (p/for [e (new options)]
      [:option (if continuation (hfql/JoinAllTheTree. (new continuation e)) e)])))

(tests
  (with (p/run (tap (binding [hf/db       hf/*$*
                              hf/*nav!*   nav!
                              hf/*schema* get-schema
                              hfql/Render hfql/EdnRender]
                      (hfql [] [(props :order/shirt-size {::hf/render  Select-option-renderer
                                                          ::hf/options (shirt-sizes :order/female "")})]
                        9)))))
  % := {:order/shirt-size [:select {:value :order/womens-large} [:option 6] [:option 7] [:option 8]]}
  )

(tests
  "hf/options can continue with parent pullexpr"
  (with (p/run
          (tap (binding [hf/db       hf/*$*
                         hf/*nav!*   nav!
                         hf/*schema* get-schema
                         hfql/Render hfql/EdnRender]
                 (hfql [] {(props :order/shirt-size {::hf/render  Select-option-renderer
                                                     ::hf/options (shirt-sizes :order/female "")})
                           [:db/ident]} 9) ))))
  % := {:order/shirt-size [:select {:value #:db{:ident :order/womens-large}}
                           [:option #:db{:ident :order/womens-small}]
                           [:option #:db{:ident :order/womens-medium}]
                           [:option #:db{:ident :order/womens-large}]]})

(tests
  "Argument reference"
  (with (p/run (tap (binding [hf/db       hf/*$*
                              hf/*nav!*   nav!
                              hf/*schema* get-schema
                              hfql/Render hfql/EdnRender]
                      (hfql [] [{:order/gender [:db/ident]}
                                (props :order/shirt-size {::hf/render  Select-option-renderer
                                                          ::hf/options (shirt-sizes db/ident "")})]
                        9) ))))
  % := {:order/gender     {:db/ident :order/female}
        :order/shirt-size [:select {:value :order/womens-large} [:option 6] [:option 7] [:option 8]]}
  )

(tests
    "Argument reference under card n"
    (with (p/run (try (tap (binding [hf/db       hf/*$*
                                     hf/*nav!*   nav!
                                     hf/*schema* get-schema
                                     hfql/Render hfql/EdnRender]
                             (hfql [] {(orders "") [{:order/gender [:db/ident]}
                                                    (props :order/shirt-size {::hf/render  Select-option-renderer
                                                                              ::hf/options (shirt-sizes db/ident "")})]}
                               9)
                             ))
                      (catch Pending _))))
    % := {`(orders "")
          [{:order/shirt-size [:select {:value :order/womens-large} [:option 6] [:option 7] [:option 8]],
            :order/gender     {:db/ident :order/female}}
           {:order/shirt-size [:select {:value :order/mens-large} [:option 3] [:option 4] [:option 5]],
            :order/gender     {:db/ident :order/male}}
           {:order/shirt-size [:select {:value :order/mens-medium} [:option 3] [:option 4] [:option 5]],
            :order/gender     {:db/ident :order/male}}]})

(tests
    "lexical env"
    (let [needle1 ""
          needle2 "small"]
      (with (p/run (try (tap (binding [hf/db       hf/*$*
                                       hf/*nav!*   nav!
                                       hf/*schema* get-schema
                                       hfql/Render hfql/EdnRender]
                               (hfql [] {(orders needle1) [:order/email
                                                           {:order/gender [(props :db/ident {::hf/as gender})]}
                                                           {(props :order/shirt-size {::hf/render  Select-option-renderer
                                                                                      ::hf/options (shirt-sizes gender needle2)})
                                                            [:db/ident]}]}
                                 9) ))
                        (catch Pending _)))))
    % := {'(orders-datascript/orders needle1)
          [{:order/shirt-size
            [:select
             {:value {:db/ident :order/womens-large}}
             [:option {:db/ident :order/womens-small}]],
            :order/email  "alice@example.com",
            :order/gender {:db/ident :order/female}}
           {:order/shirt-size
            [:select
             {:value {:db/ident :order/mens-large}}
             [:option {:db/ident :order/mens-small}]],
            :order/email  "bob@example.com",
            :order/gender {:db/ident :order/male}}
           {:order/shirt-size
            [:select
             {:value {:db/ident :order/mens-medium}}
             [:option {:db/ident :order/mens-small}]],
            :order/email  "charlie@example.com",
            :order/gender {:db/ident :order/male}}]})

(p/defn Typeahead-option-renderer [{::hf/keys [Value options continuation] :as ctx}]
  (into [:typeahead {:value (hfql/JoinAllTheTree. ctx)}]
    (p/for [e (new options "small")]
      [:option (if continuation (hfql/JoinAllTheTree. (new continuation e)) e)])))

(tests
  "free inputs"
  (with (p/run (try (tap (binding [hf/db       hf/*$*
                                   hf/*nav!*   nav!
                                   hf/*schema* get-schema
                                   hfql/Render hfql/EdnRender]
                           (hfql {(orders .) [{:order/gender [(props :db/ident {::hf/as gender})]}
                                              {(props :order/shirt-size {::hf/render  Typeahead-option-renderer
                                                                         ::hf/options (shirt-sizes gender .)})
                                               [:db/ident]}]}) ) )
                    (catch Pending _))))
  % := {'(orders-datascript/orders .)
        [{:order/gender {:db/ident :order/female},
          :order/shirt-size
          [:typeahead
           {:value {:db/ident :order/womens-large}}
           [:option {:db/ident :order/womens-small}]]}
         {:order/gender {:db/ident :order/male},
          :order/shirt-size
          [:typeahead
           {:value {:db/ident :order/mens-large}}
           [:option {:db/ident :order/mens-small}]]}
         {:order/gender {:db/ident :order/male},
          :order/shirt-size
          [:typeahead
           {:value {:db/ident :order/mens-medium}}
           [:option {:db/ident :order/mens-small}]]}]})

(defn suber-name [e]
  (first (str/split (:order/email (d/entity hf/*$* e)) #"@" 2)))

(s/fdef suber-name :ret string?)

(tests
  "function navigation"

  (with (p/run (try (tap
                      (binding [hf/db     hf/*$*
                                hf/*nav!* nav!]
                        (hfql [hf/*$* hf/db]
                          {(orders "") [:db/id suber-name]}) ))
                    (catch Pending _))))
  % := `{(orders-datascript/orders "") [{:db/id 9, suber-name "alice"} {:db/id 10, suber-name "bob"} {:db/id 11, suber-name "charlie"}]})

(def ^:dynamic *db*)

(s/fdef bound-order :args (s/cat :needle string?) :ret any?)

(defn bound-order [needle]
  (binding [hf/*$* *db*]
    (orders-datascript/order needle)))

(tests
  "Binding conveyance"

  (with (p/run (try (tap
                      (binding [hf/db hf/*$*
                                hf/*nav!* nav!]
                        (hfql [*db* hf/db]
                          {(bound-order "alice") [:db/id]}) ))
                    (catch Pending _))))
  % := '{(hyperfiddle.hfql-tests/bound-order "alice") #:db{:id 9}})

(tests
  (with (p/run (tap (new (tap (with-meta (p/fn [] 1) {:contains :integer, :flow-type :constant}))))))
  (meta %) := {:contains :integer, :flow-type :constant}
  % := 1)


(tests
  "Static Link on attribute"
  (with (p/run (tap (debug (-> (hfql/precompile (props :db/id {::hf/link [:home]}))
                             (::hf/link)
                             (new))))))
  % := [:home])

(tests
  "Templated Link"
  (with (p/run (tap (binding [hf/db hf/*$*
                              hf/*nav!* nav!]
                      (debug (-> (hfql/precompile [] [:db/id (props :order/email {::hf/link [:link db/id]})] 9)
                               ::hf/values
                               (get 1)
                               (::hf/link)
                               (new)))))))
  % := [:link 9])

(p/defn Expr [] (hfql/precompile [] (props :order/email {::hf/link [:link %]}) 9))

(tests
  "Templated Link with % ref"
  (with (p/run (tap (binding [hf/db hf/*$*
                              hf/*nav!* nav!]
                      (debug (-> (Expr.)
                               (::hf/link)
                               (new)))))))
  % := [:link 9])

(tests
  "Self referencing link"
  (with (p/run (tap (binding [hf/db hf/*$*
                              hf/*nav!* nav!]
                      (debug (-> (hfql/precompile [] (props :db/id {::hf/link [:link db/id]}) 9)
                               (::hf/link)
                               (new)))))))
  % := [:link 9])


(comment
  (tests
    "Deep referencing link"
    (with (p/run (tap (binding [hf/db hf/*$*
                                hf/*nav!* nav!]
                        (debug (-> (hfql/precompile [] (props :db/id {::hf/link [:link [:db/id db/id]]}) 9)
                                 (::hf/link)
                                 (new)))))))
    % := '(:link 9)))



(comment

  (hyperfiddle.hfql-compiler/graph '(props :db/id {::hf/link '(:link db/id)}))

  (precompile (props :db/id {::hf/link '(:link db/id)}))
  (hyperfiddle.hfql-compiler/graph '[:db/id
                                  (props :order/email {::hf/link '(:link db/id)})])

  )

(defn foo [a] a)

(s/fdef foo :args (s/cat :a string?) :ret string?)

(tests
  "default on fn arg" ; only for gray inputs
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!]
                      (hfql [] (foo (or nil "ORed")) ; TODO allow referencing lexical scope from nested sexprs
                        )))))
  % := "ORed")

(p/defn Default [a]
  ;; TODO Default fn should see hf context (e.g. injected from dynamic scope.
  (or a "defaulted from Electric"))

(tests
  "default of fn argument" ; only for gray inputs
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!]
                      (hfql (foo (Default. nil)))   ; TODO allow referencing lexical scope from nested sexprs
                      ))))
  % := "defaulted from Electric")


(comment
  "default of fn argument" 
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!]
                      (debug (hfql [:db/id (foo (Default. db/id))]))   ; TODO allow referencing lexical scope from nested sexprs
                      ))))
  % := "defaulted from Electric")


(comment
  ;; TODO some default logic requires all arguments:
  (defn default [eid nom] [eid (if (and eid (empty? nom)) (suber-name eid) nom)])
  )


(tests
  "::hf/defaults"
  ;; Previous tests shows ::hf/default only exists because we cannot detect
  ;; lexical references in nested sexprs.
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!]
                      (hfql [] [:db/id
                                :order/email
                                (foo (props order/email {::hf/default (p/fn [a] (or a "defaulted"))}))]
                        12)
                      ))))
  % := {:db/id 12,
        :order/email nil,
        '(hyperfiddle.hfql-tests/foo order/email) "defaulted"})

;; (hfql/precompile [:order/email
;;                   (foo (props order/email {::hf/default (p/fn [a] (or a "defaulted"))}))]) 


(tests
  "HFQL on top level entity"
  (with (p/run (tap (hfql 12))))
  % := 12)

(tests
  "HFQL on top level entity"
  (with (p/run (tap (debug (binding [hf/db     hf/*$*
                                     hf/*nav!* nav!
                                     hf/*schema* get-schema]
                             (hfql {12 [:db/id]}))))))
  % := '{12 {:db/id 12}})

(tests
  "HFQL on top level entity"
  ;; in `e` is a function, it should act as a
  ;; function, if it’s not it should be bound to
  ;; the entity at point
  (with (p/run (tap (debug (binding [hf/db     hf/*$*
                                     hf/*nav!* nav!
                                     hf/*schema* get-schema]
                             (let [e 12]
                               (hfql {e [:db/id]})))))))
  % := '{e {:db/id 12}})


(tests
  "props on point"
  (with (p/run (debug (let [plan (hfql/precompile (props 1 {:foo :bar}))]
                        (tap (hfql/JoinAllTheTree. plan))
                        (tap (:foo plan))))))
  % := 1
  % := :bar)

(tests
  "props on point 2"
  (with (p/run (debug (let [plan (hfql/precompile (props [1] {:foo :bar})) ]
                        (tap (hfql/JoinAllTheTree. plan))
                        (tap (-> plan ::hf/values first :foo))))))
  % := {1 1}
  % := :bar)

(tests
  "props on point 3"
  (with (p/run (debug (let [plan (hfql/precompile [(props 1 {:foo :bar})]) ]
                        (tap (hfql/JoinAllTheTree. plan))
                        (tap (-> plan ::hf/values first :foo))))))
  % := {1 1}
  % := :bar)


(tests
  "Link refer to lexical env"
  (with (p/run (debug (let [e    9
                            plan (hfql/precompile {e [(props "link" {::hf/link [e]})]})]
                        (tap (hfql/JoinAllTheTree. plan))
                        (tap (-> plan ::hf/values first ::hf/Value (new) ::hf/values first ::hf/link (new)))))))
  % := {'e {"link" "link"}}
  % := [9])

(p/defn Foo [x] x)
(s/fdef Foo :args (s/cat :x any?) :ret any?)

(tests
  "Call Electric function"
  (with (p/run (tap (hfql (Foo. 1)))))
  % := 1)

(tests
  "Escape to Electric in rendering point"
  (with (p/run (tap (hfql [(Foo. 1)]))))
  % := `{(hyperfiddle.hfql-tests/Foo 1) 1})

(tests
  "Navigate through Electric function"
  (with (p/run (tap (debug (binding [hf/db     hf/*$*
                                     hf/*nav!* nav!
                                     hf/*schema* get-schema]
                             (hfql {(Foo. 9) [:order/email]}))))))
  % := `{(hyperfiddle.hfql-tests/Foo 9) {:order/email "alice@example.com"}})

(comment

  (hfql/precompile {e [(props "link" {::hf/link [e]})]}) 
  (hfql/precompile {e [(props "link" {#_#_::hf/link [e]})]}) 
  (hyperfiddle.hfql-compiler/graph '{e [(props "link" {::hf/link [e]})]} )

  )

(comment
  (rcf/enable!))

(comment

  (hyperfiddle.hfql-compiler/analyze '[(props :order/shirt-size {::hf/options (shirt-sizes :order/male .)})])
  (hyperfiddle.hfql-compiler/graph '[(props :order/shirt-size {::hf/options (shirt-sizes :order/male .)})])

  (hfql/precompile [(props :order/shirt-size {::hf/options (shirt-sizes :order/male .)})]) 
  )


;; Is there such a thing as gray options?
;; Yes options on gray inputs

;; typing in the typeahead does not write to the route
;; picking a value does

;; can there be a typeahead with 2 free inputs?
;; maybe it can but we don’t have a use case today
