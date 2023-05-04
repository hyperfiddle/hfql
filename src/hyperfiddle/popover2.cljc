(ns hyperfiddle.popover2
  #?(:cljs (:require-macros hyperfiddle.popover))
  (:import [hyperfiddle.electric Pending])
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.hfql :as hfql]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]
            [missionary.core :as m]
            [hyperfiddle.spec :as spec]
            [hyperfiddle.history :as router]))

(e/defn BranchWrap2 [Validate Transact Body-client] ; todo colorless p/fns
  (binding [hf/validation-hints (spec/reformat-explain-data (Validate.))]
    (e/server
      (hf/branch (e/client (Body-client.)) hf/stage)
      (e/client
        (dom/hr)
        (let [return (m/dfv)]
          (ui/button (e/fn [] (Transact. ) (return :commit))
            (when hf/validation-hints
              (dom/props {::dom/disabled true}))
            (dom/text "commit!"))
          (ui/button (e/fn []
                       (return :discard)) (dom/text "discard"))
          ;; TODO simplify this gymnastic
          (try (new (e/task->cp return)) ; Entrypoint treats pending as loading state which this is not
               (catch Pending _ nil)))))))

(e/defn PopoverBody2 [Validate Transact Body]
  (dom/div (dom/props {:class    "hyperfiddle popover-body"
                       :tabIndex "1"})
    (dom/on! "click" (fn [e]
                       (when (= (.-target e) (.-currentTarget e)) ; click on self
                         (.focus (.-currentTarget e)))))
    (BranchWrap2. Validate Transact (e/fn [] (Body.)))))

(e/defn Popover2 [label Validate Transact Body]
  (let [!open? (atom false), open? (e/watch !open?)]
    (dom/div (dom/props {:class "hyperfiddle popover-wrapper"})
      (ui/button (e/fn [] (swap! !open? not)) (dom/text label)) ; popover anchor
      (when open?
        (case (PopoverBody2. Validate Transact Body)
          (:commit :discard) (swap! !open? not)
          nil                (do))
        nil))))

(defmacro popover2*
  ([label body]
   `(popover2* ~label (e/fn []) ~body))
  ([label Transact body]
   `(popover2* ~label (e/fn []) ~Transact ~body))
  ([label Validate Transact & body]
   `(e/client
     (router/router (router/proxy-history router/!history) ; sever popover state from URL
      (new Popover2 ~label ~Validate ~Transact (e/fn [] ~@body))))))

;; TODO Move to own namespace so we can retire popover1
(defmacro popover2 
  ([label HFQL-Expr]
   `(popover2 ~label (e/fn []) ~HFQL-Expr))
  ([label Transact HFQL-Expr]
   `(popover2 ~label (e/fn []) ~Transact ~HFQL-Expr))
  ([label Validate Transact HFQL-Expr]
   `(popover2* ~label ~Validate ~Transact
               (ttgui/with-gridsheet-renderer
                 (e/server
                  (hfql/hfql [hf/*$* hf/db, suber.web.globals/*db* hf/db, hf/*nav!* hf/*nav!*]
                           ~HFQL-Expr))))))
