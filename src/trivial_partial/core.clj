(ns trivial-partial.core
  (:require [clojure.walk :as w]))

(def ^:dynamic *placeholder-symbol* '__)

(defmacro throw-replacement-error [item replacements data]
  `(throw (IllegalArgumentException.
           (str "not enough replacements in \"" ~replacements
                "\" to replace item \"" ~item "\" in \"" ~data "\""))))

(defn- replace-with-seq
  "replaces all entries of item in data with values from replacements seq one by one"
  [item replacements data & {:keys [compare-fn] :or {compare-fn =}}]
  
  (loop [[r & rs :as rpl] replacements
         [x & xs :as dt] data
         res []]
    (cond (empty? dt) (seq res)
          (compare-fn item x) (if (empty? rpl)
                                (throw-replacement-error item replacements data)
                                (recur rs xs (conj res r)))
          :else (recur rpl xs (conj res x)))))

(defn- deep-replace-with-seq
  [item replacements data & {:keys [compare-fn] :or {compare-fn =}}]
  (let [rep-a (atom (seq replacements))]
    (w/prewalk #(if (compare-fn item %)
                  (let [[r & _ :as rep] @rep-a]
                    (when (empty? rep) (throw-replacement-error item replacements data))
                    (swap! rep-a rest)
                    r)
                  %)
               data)))

(defn- deep-count-items
  [x data & {:keys [compare-fn item-key] :or {compare-fn =
                                              item-key identity}}]
  (letfn [(counti [data]
            (cond (compare-fn x (item-key data)) 1
                  (coll? data) (apply + (map counti data))
                  :else 0))]
    (counti data)))

(defn- count-items
  "counts the number of \"x\" in \"data\""
  [x data & {:keys [compare-fn item-key] :or {compare-fn =
                                              item-key identity}}]
  (loop [data data res 0]
    (if (empty? data)
      res
      (recur (rest data) (if (compare-fn x (item-key (first data)))
                           (inc res)
                           res)))))

(defmacro make-partial
  "generates new function using gaps from args.
  the gap symbol is defined by *placeholder-symbol* and can be redefined.
  by default it is equal to '__"
  [f args & {:keys [placeholder] :or {placeholder *placeholder-symbol*}}]
  (let [n (count-items placeholder args)
        argnames (vec (repeatedly n gensym))
        f-args (replace-with-seq placeholder argnames args)]
    `(fn ~(vec argnames) (~f ~@f-args))))

(defmacro make-partial*
  "generates new function using gaps from args (using deep lookup in arguments).
  the gap symbol is defined by *placeholder-symbol* and can be redefined.
  by default it is equal to '__"
  [f args & {:keys [placeholder] :or {placeholder *placeholder-symbol*}}]
  (let [n (deep-count-items placeholder args)
        argnames (vec (repeatedly n gensym))
        f-args (deep-replace-with-seq placeholder argnames args)]
    `(fn ~(vec argnames) (~f ~@f-args))))
