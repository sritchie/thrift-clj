(ns com.twitter.thrift-clj.util)

;; ## Utilities

(defn strip-extension
    "Remove the extension from the supplied relative filepath."
    [^String s]
    (let [idx (.indexOf s ".")]
      (if (= idx -1)
        s
        (subs s 0 idx))))

(defn remove-falsey
  "Removes kv pairs with nil or false values from the supplied map."
  [m]
  (into {} (filter val m)))

(defn to-str [chars] (apply str chars))
