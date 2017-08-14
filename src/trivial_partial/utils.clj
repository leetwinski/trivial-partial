(ns trivial-partial.utils
  (:require [clojure.walk :as w]))

(defmacro throw-replacement-error [item replacements data]
  `(throw (IllegalArgumentException.
           (str "not enough replacements in \"" ~replacements
                "\" to replace item \"" ~item "\" in \"" ~data "\""))))

(defn deep-replace-with-seq
  [item replacements data & {:keys [compare-fn] :or {compare-fn =}}]
  (let [rep-a (atom (seq replacements))]
    (w/prewalk #(if (compare-fn item %)
                  (let [[r & _ :as rep] @rep-a]
                    (when (empty? rep) (throw-replacement-error item replacements data))
                    (swap! rep-a rest)
                    r)
                  %)
               data)))

(defn deep-find [x data & {:keys [compare-fn item-key] :or {compare-fn =
                                                             item-key identity}}]
  "searches for an item in collection depth first"
  (->> data
     (tree-seq coll? seq)
     (some #(when (compare-fn x (item-key %)) %))))

(defn deep-count-items
  [x data & {:keys [compare-fn item-key] :or {compare-fn =
                                              item-key identity}}]
  (->> data
     (tree-seq coll? seq)
     (filter #(compare-fn x (item-key %)))
     count))
