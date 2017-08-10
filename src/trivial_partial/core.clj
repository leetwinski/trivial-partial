(ns trivial-partial.core)

(def ^:dynamic *placeholder-symbol* '_)

(defn- replace-with-seq
  "replaces all entries of item in data with values from replacements seq one by one"
  [item replacements data & {:keys [compare-fn] :or {compare-fn =}}]
  
  (loop [[r & rs :as rpl] replacements
         [x & xs :as dt] data
         res []]
    (cond (empty? dt) (seq res)
          (empty? rpl) (throw (IllegalArgumentException.
                                        (str "not enough replacements for " item
                                             " in " replacements
                                             " for data " data)))
          (compare-fn item x) (recur rs xs (conj res r))
          :else (recur rpl xs (conj res x)))))

(defn- count-items
  "counts the number of x in data"
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
  by default it is equal to _"
  [f args]

  (let [n (count-items *placeholder-symbol* args)
        argnames (repeatedly n gensym)
        f-args (replace-with-seq *placeholder-symbol* argnames args)]
    `(fn ~(vec argnames) (~f ~@f-args))))
