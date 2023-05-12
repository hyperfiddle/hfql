(ns hyperfiddle.hfql-tree-grid
  (:require [hyperfiddle.electric :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.hfql :as hfql]
            [hyperfiddle.popover2 :as popover]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-svg :as svg]
            [hyperfiddle.spec :as spec]
            [hyperfiddle.spec.type :as-alias hf-type]
            [clojure.datafy :refer [datafy]]
            [clojure.string :as str]
            ;; [contrib.ednish :as ednish]
            [contrib.color :as c]
            [contrib.data :as data]
            [hyperfiddle.electric-ui4 :as ui4]
            [hyperfiddle.rcf :refer [tests with % tap]]
            [missionary.core :as m]
            [hyperfiddle.history :as router])
  (:import (hyperfiddle.electric Pending))
  #?(:cljs (:require-macros [hyperfiddle.hfql-tree-grid]))
  #?(:cljs (:refer-clojure :exclude [List])))

(defn attr-spec [attr]
  (cond
    (ident? attr) attr
    (seq? attr)   (attr-spec (first attr))))

(defn spec-value-type [attr] ; TODO extract spec for quoted sexpr ; TODO support args
  (when (qualified-ident? attr)
    (spec/type-of attr)))

(defn schema-value-type [schema-f db a]
  (let [attr (schema-f db a)]
    (spec/valueType->type (or (:db/valueType attr) (:hf/valueType attr))))) ; datascript rejects valueType other than ref.

(defn schema-cardinality [schema-f db a]
  (case (:db/cardinality (schema-f db a))
    :db.cardinality/one  ::hf/one
    :db.cardinality/many ::hf/many
    nil))

(defn spec-description [prefer-ret? attr]
  (when (qualified-ident? attr)
    (when-let [spec (datafy (spec/spec attr))]
      (if prefer-ret?
        (case (::spec/type spec)
          ::spec/fspec (::spec/ret spec)
          (::spec/description spec))
        (::spec/description spec)))))

;; ----

(p/def table-picker-options {::group-id nil, ::current-value nil}),

(p/def grid-width 2) ; TODO infer from ctx

(p/def grid-row 1) ; TODO not called with new, don’t capitalize
(p/def grid-col 1)
(p/def indentation 0)
(p/def pagination-offset)

(defmacro cell [row col & body]
  `(dom/div
     (dom/props {::dom/role  "cell"
                  ::dom/style {:grid-column ~col
                                :grid-row    ~row}})
     ~@body))

(defn find-best-identity [v] ; TODO look up in schema
  (cond (map? v) (or (:db/ident v) (:db/id v))
        :else    v))

(p/defn Identity [x] x)

(p/def Render)

(defn reductions* "like reductions but stop on first reduced value"
  [f init coll]
  (reduce (fn [[r prev] v] (let [new (f prev v)]
                             (if (reduced? new)
                               (reduced (conj r (unreduced new)))
                               [(conj r new) new])))
    [[] init] coll))

#?(:cljs (defn draw-lines! [node color width height gap rows columns]
           (when (and (pos? width) (pos? height)
                   (some pos? rows)
                   (some pos? columns))
             (let [xs  (reductions (partial + gap) columns)
                   ys  (reductions* (fn [r row]
                                      (let [h (+ r row gap)]
                                        (if (>= h height)
                                          (reduced h)
                                          h)))
                         0
                         (concat rows (repeat (last rows))))
                   ctx (and (.-getContext node) (.getContext node "2d"))]
               (when ctx
                 (.clearRect ctx 0 0 width height)
                 (set! (.-fillStyle ctx) color)
                 (doseq [x xs] (.fillRect ctx (int x) 0 gap height))
                 (doseq [y ys] (.fillRect ctx 0 (int y) width gap)))
               ))))

#?(:cljs (defn extract-borders [^js style]
           [(.-gridTemplateRows style)
            (.-gridTemplateColumns style)
            (.-width style)
            (.-height style)
            (.-gap style)
            (.getPropertyValue style "--hf-cell-border-color")]))

#?(:cljs
   (defn -parse-float [str]
     (let [f (js/parseFloat str)]
       (if (NaN? f) 0 f))))

#?(:cljs (defn parse-borders [[gridTemplateRows gridTemplateColumns width height gap hf-cell-border-color]]
           [(map -parse-float (str/split gridTemplateRows #"px\s"))
            (map -parse-float (str/split gridTemplateColumns #"px\s"))
            (-parse-float width)
            (-parse-float height)
            (-parse-float gap)
            hf-cell-border-color]))

;; This should not be see in userland because it’s an implementation detail
;; driven by Photon not supporting mutual recursion as of today.

(defmacro with-gridsheet-renderer* [& body]
  `(p/client ; FIXME don’t force body to run on the client
     (binding [grid-row 1
               grid-col 1]
       (dom/div (dom/props {:class "hyperfiddle-gridsheet-wrapper"})
         (dom/div (dom/props {:class "hyperfiddle-gridsheet"})
           ~@body)
         (let [wrapper-height# (new ComputedStyle #(-parse-float (.-height %)) dom/node)]
           (when-let [node (.querySelector dom/node ".hyperfiddle-gridsheet")]
             (let [[rows# columns# width# height# gap# color#] (parse-borders (new ComputedStyle extract-borders node))
                   [scroll-top# scroll-height# client-height#] (new (ui4/scroll-state< node))
                   height#                                     (if (zero? scroll-height#) height# scroll-height#)]
               (dom/canvas (dom/props {:class  "hf-grid-overlay"
                                       :width  (str width# "px")
                                       :height (str wrapper-height# "px")})
                 (draw-lines! dom/node color# width# wrapper-height# gap# rows# columns#)))))))))

(defmacro with-gridsheet-renderer [& body]
  `(p/server
     (binding [Table     Table-impl
               Form      Form-impl
               Popover   Popover-impl
               Render    Render-impl
               hfql/Render Render-impl]
       (with-gridsheet-renderer* ~@body))))

(defn grab
  ([ctx k] (or (get ctx k) (get (::hf/parent ctx) k)))
  ([ctx k default] (or (get ctx k) (get (::hf/parent ctx) k) default)))

(defn has-needle? [ctx] (seq (grab ctx ::hf/options-arguments)))

(defn ->picker-type [needle? many?]
  (cond many?   ::tag-picker
        needle? ::typeahead
        :else   ::select))

(defmacro options-props [disabled? dom-props]
  `(do (dom/props {:role     "cell"
                   :style    {:grid-row grid-row, :grid-column grid-col :overflow "visible"}
                   :disabled ~disabled?})
       (dom/props ~dom-props)))

(p/defn Cardinality [ctx]
  (or (::hf/cardinality ctx) (schema-cardinality hf/*schema* hf/db (::hf/attribute ctx))))

(p/defn Options [ctx]
  (let [options      (grab ctx ::hf/options)
        option-label (grab ctx ::hf/option-label Identity)
        continuation (grab ctx ::hf/continuation Identity)
        tx           (grab ctx ::hf/tx)
        tx?          (some? tx)
        dom-props    (data/select-ns :hyperfiddle.electric-dom2 ctx)
        v            (find-best-identity (hfql/JoinAllTheTree. ctx))
        V!           (if tx? (p/fn [v] (tx. ctx v)) Identity)
        OptionLabel  (p/fn [id] (option-label. (hfql/JoinAllTheTree. (continuation. id))))]
    (case (->picker-type (has-needle? ctx) (= ::hf/many (Cardinality. ctx)))
      ::typeahead (ui4/typeahead v V! options OptionLabel (options-props (not tx?) dom-props))
      ::select    (ui4/select v V! options OptionLabel (options-props (not tx?) dom-props))
      ::tag-picker (let [unV! (if-some [untx (grab ctx ::hf/untx)] (p/fn [v] (untx. ctx v)) Identity)]
                     (ui4/tag-picker v V! unV! options OptionLabel (options-props (not tx?) dom-props))))))

(defmacro input-props [readonly? grid-row grid-col dom-for]
  `(do
     (dom/props {::dom/role     "cell"
                  ::dom/disabled ~readonly?
                  ::dom/style    {:grid-row ~grid-row, :grid-column ~grid-col}})
     (when ~dom-for
       (dom/props {::dom/id ~dom-for}))))

(p/defn Input [{::hf/keys [attribute tx link] :as ctx}]
  (let [spec-value-type   (spec-value-type attribute)
        schema-value-type (schema-value-type hf/*schema* hf/db attribute)
        defined-by-spec?  (and spec-value-type (not schema-value-type))
        route             (when link (new link))
        option-label      (grab ctx ::hf/option-label Identity)
        v                 (hfql/JoinAllTheTree. ctx)
        label             (option-label. v)
        tx?               (some? tx)
        readonly?         (or defined-by-spec? (not tx?))
        dom-for           (::dom/for ctx)]
    (cond
      (some? route)
      (p/client (cell grid-row grid-col (router/link route (dom/text label))))

      (= ::hf/many (Cardinality. ctx))
      (ui4/tag-picker v (when-not readonly? (p/fn [_])) (p/fn [_]) nil option-label
        (input-props readonly? grid-row grid-col dom-for)
        (dom/props {:style {:display "inline-flex"}}))

      :else
      (p/client
        (let [Tx (when-not readonly? (p/fn [v] (p/server (hf/Transact!. (tx. ctx v)) nil)))]
          (case (or spec-value-type schema-value-type)
            (::hf-type/boolean) (ui4/checkbox       v Tx (input-props readonly? grid-row grid-col dom-for))
            (::hf-type/instant) (ui4/datetime-local (when v (-> v .toISOString (.slice 0 -1))) Tx (input-props readonly? grid-row grid-col dom-for))
            (::hf-type/bigdec
             ::hf-type/long)    (ui4/long           v Tx (input-props readonly? grid-row grid-col dom-for))
            (::hf-type/float
             ::hf-type/double)  (ui4/double         v Tx (input-props readonly? grid-row grid-col dom-for))
            (::hf-type/symbol)  (ui4/symbol         v Tx (input-props readonly? grid-row grid-col dom-for))
            (::hf-type/keyword) (ui4/keyword        v Tx (input-props readonly? grid-row grid-col dom-for))
            (::hf-type/uuid)    (ui4/uuid           v Tx (input-props readonly? grid-row grid-col dom-for))
            (::hf-type/uri
             ::hf-type/string
             ::hf-type/ref
             nil)               (ui4/input    (str label) Tx (input-props readonly? grid-row grid-col dom-for))))))))

(p/defn Simple [ctx] (if (grab ctx ::hf/options) (Options. ctx) (Input. ctx)))

;; TODO adapt to new HFQL macroexpansion
(p/defn Render-impl [{::hf/keys [type render popover Value] :as ctx}]
  (cond
    popover (new Popover ctx)
    render  (p/client (cell grid-row grid-col (p/server (render. ctx))))
    :else
    (case type
      ::hf/leaf (Simple. ctx)
      ::hf/keys (Form. ctx)
      (case (Cardinality. ctx)
        ::hf/many (Table. ctx)
        (let [v (Value.)]
          (cond
            (vector? v) (Table. ctx)
            (map? v)    (Render. (assoc v ::hf/parent ctx))
            :else       (throw (ex-info "unreachable" {:v v}))))))))

(defn height
  ([default-height ctx] (height default-height ctx (::value ctx)))
  ([default-height {::hf/keys [height arguments keys attribute] :as ctx} value]
   (cond
     (::hf/popover ctx) 1
     :else
     (let [argc         (count arguments)
           height       (or height default-height)
           actual-count (min (::count ctx) height)]
       (+ argc
         (cond
           (= '_ attribute)                      1
           ;; user provided, static height
           (some? (::hf/height ctx))             (+ height argc)
           ;; transposed form (table)
           (and keys (pos-int? (::count ctx)))   (+ 1 ; table header
                                                   (if (> (::count ctx) height) 1 0) ; pagination
                                                   (max actual-count 1) ; rows
                                                   (if (pos? argc) 1 0)) ; args pushes table to next row
           ;; leaf
           (or (set? value) (sequential? value)) (count value)
           ;; static form
           (some? keys)                          (+ 1 (count keys)) ; form labels on next row
           :else                                 (max actual-count 1)))))))

(defn non-breaking-padder [n] (apply str (repeat n " ")) )

(defn field-name [attr]
  (if (seq? attr)
    #_(cons (symbol (field-name (first attr))) #_(seq (::spec/keys (clojure.datafy/datafy (spec/args (first attr))))))
    (symbol (field-name (first attr))) ; only print fn name, not args
    (name attr)))

(defmacro gray-input-props [id props list-id options name readonly]
  `(do (dom/props {:id ~id, :disabled ~readonly})
       (when (seq ~props) (dom/props ~props))
       (when (some? ~options) (dom/props {::dom/list ~list-id}))))

;; Idea
;; (p/defn ResolveSpec [s] (or (spec/resolve s) ~@(spec/resolve s)))

(defn css-class [x]
  (cond
    (coll? x)    (str/join " " (eduction (remove nil?) (map css-class x)))
    (keyword? x) (subs (str x) 1)
    (= '. x)     nil
    :else        (str x)))

(p/defn AlertCircleIcon [props]
  (svg/svg (dom/props {::dom/viewBox "0 0 24 24"})
    (dom/props props)
    (svg/line (dom/props {:stroke-linejoin "round", :y1 "8", :stroke-linecap "round", :stroke-width "2", :x1 "12", :y2 "12", :x2 "12"}))
    (svg/line (dom/props {:stroke-linejoin "round", :y1 "16", :stroke-linecap "round", :stroke-width "2", :x1 "12", :y2 "16", :x2 "12"}))
    (svg/circle (dom/props {:r "10", :stroke-linejoin "round", :stroke-linecap "round", :stroke-width "2", :cx "12" :cy "12"}))))

(defmacro input-validator [Validate & body]
  `(p/client
     (let [validate-message (new ~Validate)]
       (dom/div (dom/props {::dom/class ["hfql-input-validator" (when validate-message "hfql-invalid")]})
         ~@body
         (when validate-message
           (AlertCircleIcon. {::dom/class "hfql-input-validator-invalid-icon"})
           (dom/span (dom/props {::dom/class "hfql-input-validator-invalid-message"})
             (dom/span (dom/text validate-message))))))))

(p/defn ValidationMessage [input-name]
  (when-some [hints (seq (get hf/validation-hints [input-name]))]
    (str/join "\n" (map :reason hints))))

(p/defn GrayInput [label? spec props [name {::hf/keys [read path options option-label readonly] :as arg}]]
  (let [value    (read.)
        options? (some? options)]
    (p/client
      (let [id       (random-uuid)
            list-id  (random-uuid)
            arg-spec (spec/arg spec name)]
        (when label?
          (dom/label
            (dom/props {::dom/role  "cell"
                        ::dom/class "label"
                        ::dom/for   id,
                        ::dom/title (str/join " - " (filter some? [(field-name name) (some-> (:hyperfiddle.spec/form arg-spec) pr-str)]))
                        ::dom/style {:grid-row    grid-row
                                     :grid-column grid-col
                                     :color       :gray}})
            (dom/text (str (non-breaking-padder indentation) (field-name  name)))))
        (cond

          options?
          ;; FIXME Call Options
          (input-validator
            (p/fn [] (ValidationMessage. name))
            (dom/props {:role "cell", :style {:grid-row grid-row, :grid-column (inc grid-col)}})
            (p/server
              (if (has-needle? arg)
                (ui4/typeahead value (p/fn [v] (p/client (router/swap-route! assoc-in path v)))
                  options
                  (or option-label Identity)
                  (dom/props {:id list-id, :disabled readonly}))
                (ui4/select value
                  (p/fn [v] (p/client (router/swap-route! assoc-in path v)))
                  options
                  (or option-label Identity)
                  (dom/props {:id list-id, :disabled readonly})))))

          :else
          (let [WriteToRoute (p/fn [v] (router/swap-route! assoc-in path v) nil)
                props (assoc props :class (css-class path))]
            (input-validator
              (p/fn [] (ValidationMessage. name))
              (dom/props {::dom/role "cell", ::dom/style {:grid-row grid-row, :grid-column (inc grid-col)}})
              (case (spec/type-of spec name) ; Always resolve specs on the server (might be defined in a .clj files)
                (::hf-type/boolean) (ui4/checkbox value WriteToRoute (gray-input-props id props list-id options name readonly))
                (::hf-type/double
                 ::hf-type/float)   (ui4/double   value WriteToRoute (gray-input-props id props list-id options name readonly))
                (::hf-type/bigdec
                 ::hf-type/long)    (ui4/long     value WriteToRoute (gray-input-props id props list-id options name readonly))
                (::hf-type/instant) (ui4/date     (when value (-> value .toISOString (subs 0 10))) WriteToRoute (gray-input-props id props list-id options name readonly))
                (::hf-type/keyword) (ui4/keyword  value WriteToRoute (gray-input-props id props list-id options name readonly))
                (::hf-type/symbol)  (ui4/symbol   value WriteToRoute (gray-input-props id props list-id options name readonly))
                (::hf-type/uuid)    (ui4/uuid     value WriteToRoute (gray-input-props id props list-id options name readonly))
                (::hf-type/string
                 ::hf-type/uri
                 ::hf-type/ref
                 nil)               (ui4/input    value WriteToRoute (gray-input-props id props list-id options name readonly))))))
        value))))

(defn apply-1 [n F args]
  (let [syms (vec (repeatedly n gensym))]
    `(let [~syms ~args]
       (new ~F ~@syms))))

(defmacro applier [n F args]
  (let [Fsym     (gensym "f")
        args-sym (gensym "args")
        cases    (mapcat (fn [n] [n (apply-1 n Fsym args-sym)]) (rest (range (inc n))))]
    `(let [~Fsym     ~F
           ~args-sym ~args
           n#        (count ~args-sym)]
       (case n#
         0 (new ~Fsym)
         ~@cases
         (throw (ex-info (str "Apply is defined for up to 20 args, given " n# ".") {}))))))

(p/defn Apply [F args] (applier 20 F args))

(tests
  (p/defn Plus [a b c] (+ a b c))
  (with (p/run (tap (Apply. Plus [1 2 3]))))
  % := 6)

(p/defn GrayInputs [{::hf/keys [tx attribute arguments] :as ctx}]
  (when-some [arguments (seq arguments)]
    (let [args (p/for-by second [[idx arg] (map-indexed vector arguments)]
                 (p/client
                   (binding [grid-row (+ grid-row idx)]
                     (p/server
                       ;; TODO spec search below will break for gray input nested forms
                       (GrayInput. true (-> arg second ::hf/path first attr-spec) nil arg)))))]
      (when (some? tx)
        (Apply. tx args)))))

(p/def Form)

(p/defn Form-impl [{::hf/keys [keys values] :as ctx}]
  (let [parent-ctx ctx
        values (p/for [ctx values]
                 (assoc ctx ::count (new (::hf/count ctx (p/fn [] 0)))))]
    (p/client
      (dom/form
        (dom/on! "submit" (fn [e] (.preventDefault e))) ; an HFQL form is semantic only, it is never submitted
        (dom/props {::dom/role  "form"
                     ::dom/style {:border-left-color (c/color hf/db-name)}})
        (p/server
          (let [heights (vec (reductions + 0 (map (partial height default-height) values)))]
            (into [] cat
              (p/for-by (comp first second) [[idx [key ctx]] (map-indexed vector (partition 2 (interleave keys values)))]
                (let [leaf? (= ::hf/leaf (::hf/type ctx))
                      argc  (count (::hf/arguments ctx))
                      h     (get heights idx)]
                  (p/client
                    (let [row     (+ grid-row idx (- h idx))
                          dom-for (random-uuid)]
                      (dom/label
                        (dom/props
                          {::dom/role  "cell"
                           ::dom/class "label"
                           ::dom/for   dom-for
                           ::dom/style {:grid-row         row
                                         :grid-column      grid-col
                                         #_#_:padding-left (str indentation "rem")}
                           ::dom/title (str/join " - " (filter some? [(field-name key)
                                                                      (some-> (or (spec-description false (attr-spec key))
                                                                                (p/server (schema-value-type hf/*schema* hf/db key)))
                                                                        pr-str)]))})
                        (dom/text (str (non-breaking-padder indentation) (field-name key))))
                      (into [] cat
                        [(when-not (p/server (::hf/popover ctx))
                           (binding [grid-row    (inc row)
                                     indentation (inc indentation)]
                             (p/server (GrayInputs. ctx))))
                         (binding [grid-row    (cond leaf?       row
                                                     (pos? argc) (+ row (inc argc))
                                                     :else       (inc row))
                                   grid-col    (if leaf? (inc grid-col) grid-col)
                                   indentation (if leaf? indentation (inc indentation))]
                           (p/server
                             (let [ctx (assoc ctx ::dom/for dom-for)]
                               (Render. (assoc ctx ::dom/for dom-for ::parent-argc argc, ::hf/parent parent-ctx)))))])
                      )))))))))))

(defn closest-function-call-in-path [path]
  (->> (reverse path) (filter seq?) (first)))

(tests
  (closest-function-call-in-path '[admin (change-email admin .) :email]) := '(change-email admin .)
  (closest-function-call-in-path '[(foo) (change-email admin .) :email]) := '(change-email admin .)
  (closest-function-call-in-path '[(change-email admin .) (foo)]) := '(foo)
  (closest-function-call-in-path '[(change-email admin .)]) := '(change-email admin .)
)

(defn select-context [ctx keys]
  (assert (= ::hf/keys (::hf/type ctx)))
  (let [keys          (set keys)
        indexes       (into {} (map-indexed (fn [idx k] [k idx]) (::hf/keys ctx)))
        filtered-keys (into [] (filter keys) (::hf/keys ctx))
        keep-indexes  (set (vals (select-keys indexes keys)))
        filtered-vals (->> (map-indexed vector (::hf/values ctx))
                        (filter (fn [[idx value]]
                                  (keep-indexes idx)))
                        (mapv second))]
    (assoc ctx ::hf/keys filtered-keys, ::hf/values filtered-vals)))

(tests
  (select-context {::hf/type ::hf/keys
                   ::hf/keys ["ka" "kb" "kc"]
                   ::hf/values ["va" "vb" "vc"]}
    ["ka" "kc"])
  := {::hf/type ::hf/keys
      ::hf/keys ["ka" "kc"]
      ::hf/values ["va" "vc"]})

;; Popover as an HFQL Renderer
(p/defn Popover-impl [ctx]
  (p/client
    (router/router (router/proxy-history router/!history) ; scoped popover state
      (p/server
        (let [path          (-> (::hf/arguments ctx) first second ::hf/path) ; arguments looks like [[:needle {::hf/path '[route-segment …]}] …]
              args          (into [] (p/for-by first [[k ctx] (::hf/arguments ctx)]
                                       [k (new (::hf/read ctx))]))
              function-call (closest-function-call-in-path path)
              Validate      (p/fn [] (when function-call (apply spec/explain-fspec-data (first function-call) (map second args))))
              Transact      (p/fn [] (when-let [Tx (::hf/tx ctx)] (hf/Transact!. (Apply. Tx (map second args)))))
              Render        (p/fn [ctx] (hfql/Render. (-> (::hf/parent ctx)
                                                        (select-context #{function-call})
                                                        (assoc ::hf/values [(dissoc ctx ::hf/popover)]) ; prevent infinite recursion
                                                        )))]
          (p/client
            (popover/Popover.
              (p/server (::hf/popover-label ctx "Open"))
              {::dom/style {:grid-row grid-row, :grid-column grid-col}}
              (p/fn [] (p/server (Validate.)))
              (p/fn [] (p/server (Transact.)))
              (p/fn [] (router/swap-route! empty)) ; discard
              (p/fn []
                (with-gridsheet-renderer* ; reentrance
                  (p/server
                    (cond
                      (::hf/render ctx)
                      (p/client (cell grid-row grid-col
                                  (p/server ; intercept hfql/Render for progressive enhancement
                                    (binding [hfql/Render (p/fn [ctx]
                                                            (binding [hfql/Render Render-impl]
                                                              (with-gridsheet-renderer*
                                                                (p/server (Render. ctx)))))]
                                      (new (::hf/render ctx) ctx)))))

                      (= ::hf/keys (::hf/type (::hf/parent ctx)))
                      (Render. ctx)

                      :else
                      (throw "Don't know how to render a popover for this context."
                        {:type  (::hf/type ctx)
                         :?keys (::hf/keys ctx)})))))))))))
  nil)

(p/def Popover)

(p/defn Row [{::hf/keys [keys values] :as ctx}]
  (p/client
    (dom/tr
      (when-let [id (::group-id table-picker-options)]
        (let [value (p/server (hfql/JoinAllTheTree. ctx))]
          (ui4/checkbox (= (::current-value table-picker-options) value) (p/fn [_])
            (dom/props {::dom/role "cell", ::dom/name id, ::dom/style {:grid-row grid-row, :grid-column grid-col}}))))
      (p/server
        (into [] cat
          (p/for-by second [[idx ctx] (map-indexed vector values)]
            (p/client
              (binding [grid-col (+ grid-col idx)]
                (dom/td (p/server (binding [Form  Simple
                                            Table Simple]
                                    (Render. ctx))))))))))))

(p/def default-height 10)

(defn clamp [lower-bound upper-bound number] (max lower-bound (min number upper-bound)))

(defn give-card-n-contexts-a-unique-key [offset ctxs]
  (let [offset (max offset 0)]
    (into [] (map-indexed (fn [idx ctx] (assoc ctx ::key (+ offset idx)))) ctxs)))

(p/def Table)
(p/defn Table-impl [{::hf/keys [keys height Value] :as ctx}]
  (let [actual-count  (new (::hf/count ctx (p/fn [] 0)))
        actual-height (min default-height actual-count)
        height        (clamp 1 (or height default-height) actual-height)
        list?         (::list? ctx)
        nested?       (and (some? (::dom/for ctx)) (not list?))
        shifted?      (or list? (and (::parent-argc ctx) (zero? (::parent-argc ctx))))
        !pagination-offset (atom 0)]
    (binding [hf/page-drop (p/watch !pagination-offset)
              hf/page-take height]
      (p/client
        (binding [grid-col (if nested? (inc grid-col) grid-col)
                  grid-row (if (or shifted? list?) (dec grid-row) grid-row)]
          #_(stepped-scroll (count keys) height actual-height)
          (dom/table
            (dom/props {::dom/role "table"})
            (when-not list?
              (dom/thead
                (dom/tr
                  (when (::group-id table-picker-options)
                    (dom/th (dom/props {::dom/role  "cell"
                                        ::dom/style {:grid-row grid-row, :grid-column grid-col}})))
                  (p/for-by second [[idx col] (map-indexed vector keys)]
                    (dom/th (dom/props {::dom/role  "cell"
                                        ::dom/class "label"
                                        ::dom/title (pr-str (or (spec-description true (attr-spec col))
                                                              (p/server (schema-value-type hf/*schema* hf/db col)))),
                                        ::dom/style {:grid-row    grid-row,
                                                     :grid-column (+ grid-col idx)
                                                     :color       (c/color hf/db-name)}})
                      (dom/text (field-name col)))))))
            (dom/tbody
              (p/server
                (let [value (give-card-n-contexts-a-unique-key hf/page-drop (Value.))
                      result
                      (p/for-by (comp ::key second) [[idx ctx] (map-indexed vector value)]
                        (p/client (binding [grid-row (+ grid-row idx 1)]
                                    (p/server (Row. ctx)))))]
                  (p/client
                    (when (> actual-count height)
                      (p/client
                        (dom/tr
                          (dom/td
                            (dom/div (dom/props {::dom/role "cell",
                                                 ::dom/style {:grid-row (+ grid-row (inc height))
                                                              :grid-column grid-col}})
                              (dom/div (dom/props {::dom/style {:display :grid
                                                                :grid-template-columns "auto 1fr auto"
                                                                :place-items "center"}})
                                (ui4/button (p/fn [] (p/server (swap! !pagination-offset #(max 0 (- %1 %2)) (p/server hf/page-take))))
                                  (dom/props {::dom/disabled (p/server (= 0 hf/page-drop))})
                                  (dom/text "<"))
                                (dom/span (dom/text (p/server hf/page-drop) ".." (p/server (+ hf/page-drop hf/page-take)) " / " (p/server actual-count)))
                                (ui4/button (p/fn [] (p/server (swap! !pagination-offset #(min (+ %1 %2) %3) (p/server hf/page-take) actual-count)))
                                  (dom/props {::dom/disabled (p/server (>= (+ hf/page-drop hf/page-take) actual-count))})
                                  (dom/text ">")))))))))
                  result)))))))))

(defn compute-offset [scroll-top row-height]
  #?(:cljs (max 0 (js/Math.ceil (/ (js/Math.floor scroll-top) row-height)))))


#?(:cljs (defn set-css-var! [^js node key value]
           (.setProperty (.-style node) key value)))

#?(:cljs
   (defn parse-row-height [str]
     (let [float (js/parseFloat str)]
       (if (NaN? float)
         (js/parseFloat (first (re-find #"[0-9]+(\.[0-9]+)?" str)))
         float))))

(defmacro stepped-scroll [actual-width max-height actual-height & body]
  `(let [row-height#    (parse-row-height (ComputedStyle. #(.-gridAutoRows %)
                                            (.closest dom/node ".hyperfiddle-gridsheet")))
         actual-height# (* row-height# ~actual-height)
         !scroller#     (atom nil)
         !scroll-top#   (atom 0)]
     (dom/div
       (dom/props {::dom/role  "scrollbar"
                    ::dom/style {:grid-row-start (inc grid-row)
                                  :grid-row-end   (+ (inc grid-row) ~max-height)
                                  :grid-column    (+ grid-col ~actual-width)
                                 :max-height      (str "calc((var(--hf-grid-row-height) + var(--hf-grid-gap)) * " ~max-height ")")}})
       (do (reset! !scroller# dom/node)
           (let [[scroll-top#] (new (ui4/scroll-state< dom/node))]
             (reset! !scroll-top# scroll-top#))
           nil)
       (dom/div (dom/props {::dom/role "filler" "data-height" actual-height# ::dom/style {:height (str actual-height# "px")}})))

     (dom/div (dom/props {::dom/role "scrollview"})
       (dom/on! dom/node "wheel" ; TODO support keyboard nav and touchscreens
         (fn [e#] (let [scroller# @!scroller#]
                    (set! (.. scroller# -scrollTop) (+ (.. scroller# -scrollTop) (.. e# -deltaY)))))
         {:passive true})
       (let [offset# (compute-offset (p/watch !scroll-top#) row-height#)]
         (p/server
           (binding [hf/page-drop offset#
                     hf/page-take ~max-height]
             (p/client
               ~@body)))))))

(defn get-computed-style [node] #?(:cljs (js/getComputedStyle node)))

#?(:cljs
   (defn ticker [interval-ms]
     (m/observe (fn [!]
                  (! ::tick)
                  (let [ref (.setInterval js/window #(! ::tick) interval-ms)]
                    #(.clearInterval js/window ref))))))

(p/defn Sampler [rate-ms f]
  (if (= "visible" p/dom-visibility-state)
    (new (m/relieve {} (m/eduction (dedupe) (m/sample f (ticker rate-ms))))) ; one ticker per sampler, not shared
    (throw (Pending.))))

(p/defn ComputedStyle
  "Calls the `keyfn` clojure function, passing it the given DOM node’s
  CSSStyleDeclaration instance. `keyfn` is meant to extract properties from the
  live computed style object."
  ;; Does not return CSSStyleDeclaration directly because a CSSStyleDeclaration
  ;; is a live object with a stable identity. m/cp would dedupe it even if
  ;; properties might have changed.
  ;; NOTE: beware of expensive keyfn
  [keyfn node]
  (let [live-object (get-computed-style node)]
    (Sampler. 250 #(keyfn live-object))))

(p/defn Text [RenderF]
  (p/fn [ctx]
    (p/client
      (dom/props {:class "hyperfiddle-text"
                  :style {:grid-column (str (dec grid-col) " / span 2")}})
      (p/server (RenderF. ctx)))))
