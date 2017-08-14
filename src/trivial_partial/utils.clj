(ns trivial-partial.utils
  (:require [clojure.walk :as w]))

(defn deep-replace-with-seq
  "substitutes all the entries of an item in an arbitrary nested collection
  with the values from replacements collection, in order, 
  preserving the data collection's form

  throws an IllegalArgumentException in case of there is not enough replacements to
  replace all the item entries

  item - item to be replaced
  replacements - collection of substitutions to replace the item entries
  data - collection to be searched through

  optional keyword parameters:
  :item-key - function to be applied to the currently examined item (defaults to 'identity')
  :compare-fn - function (fn [x current-item]), the search criteria. (defaults to '=')"
  
  [item replacements data & {:keys [compare-fn item-key] :or {compare-fn =
                                                              item-key identity}}]
  (let [rep-a (atom (seq replacements))]
    (w/prewalk #(if (compare-fn item (item-key %))
                  (let [[r & _ :as rep] @rep-a]
                    (when (empty? rep)
                      (throw (IllegalArgumentException.
                              (str "not enough replacements in \"" replacements
                                   "\" to replace item \"" item "\" in \"" data "\""))))
                    (swap! rep-a rest)
                    r)
                  %)
               data)))

(defn deep-find [x data & {:keys [compare-fn item-key] :or {compare-fn =
                                                            item-key identity}}]
  "searches for an item in arbitrary nested collection depth first
  x - item to be found
  data - collection to be searched through

  optional keyword parameters:
  :item-key - function to be applied to the currently examined item (defaults to 'identity')
  :compare-fn - function (fn [x current-item]), the search criteria. (defaults to '=')"
  (->> data
     (tree-seq coll? seq)
     (some #(when (compare-fn x (item-key %)) %))))

(defn deep-count-items
  [x data & {:keys [compare-fn item-key] :or {compare-fn =
                                              item-key identity}}]
  "counts the number of item's entries in an arbitrary nested collection
  x - item to be counted
  data - collection to be searched through

  optional keyword parameters:
  :item-key - function to be applied to the currently examined item (defaults to 'identity')
  :compare-fn - function (fn [x current-item]), the search criteria. (defaults to '=')"
  (->> data
     (tree-seq coll? seq)
     (filter #(compare-fn x (item-key %)))
     count))
