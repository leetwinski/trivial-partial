(ns trivial-partial.core
  (:require [clojure.walk :as w]
            [trivial-partial.utils :as utils]))

(def ^:const +placeholder-symbol+ '__)

(defn- no-gaps-in-literal-maps-and-sets?
  [args placeholder]
  (->> args
     (tree-seq sequential? seq)
     (filter (some-fn map? set?))
     (not-any? (partial utils/deep-find placeholder))))

(defn- process-arglist [args placeholder]
  (let [n (utils/deep-count-items placeholder args)
        argnames (vec (repeatedly n (partial gensym "ARG_")))
        f-args (utils/deep-replace-with-seq placeholder argnames args)]
    [argnames f-args]))

(defmacro make-partial
  "generates new function using gaps from args (using deep lookup in arguments).
  the gap symbol is defined by +placeholder-symbol+ and can be redefined.
  by default it is equal to '__"
  [f args & {:keys [placeholder] :or {placeholder +placeholder-symbol+}}]
  
  {:pre [(no-gaps-in-literal-maps-and-sets? args placeholder)]}
  
  (let [[argnames f-args] (process-arglist args placeholder)]
    `(fn ~(vec argnames) (~f ~@f-args))))

(defmacro def-partial
  ([name doc-string attr-map wrapped-fn args]
   
   {:pre [(no-gaps-in-literal-maps-and-sets? args +placeholder-symbol+)
          (vector? args)
          (symbol? wrapped-fn)]}

   (println +placeholder-symbol+)
   
   (let [doc-string (or doc-string "")
         attr-map (or attr-map {})
         [argnames f-args] (process-arglist args +placeholder-symbol+)]
     `(defn ~name
        ~doc-string
        ~attr-map
        ~(vec argnames)
        (~wrapped-fn ~@f-args))))
  
  ([name doc-or-attr-map wrapped-fn args]
   (cond (string? doc-or-attr-map) '(def-partial ~name
                                      ~doc-or-attr-map
                                      nil
                                      ~wrapped-fn
                                      ~args)
         (or (nil? doc-or-attr-map) (map? doc-or-attr-map)) `(def-partial ~name
                                                              nil
                                                              ~doc-or-attr-map
                                                              ~wrapped-fn
                                                              ~args)
         :else (throw (IllegalArgumentException.
                       (str
                        "expected function docstring or attributes-map but found "
                        doc-or-attr-map)))))
  
  ([name wrapped-fn args] `(def-partial ~name nil nil ~wrapped-fn ~args)))
