(ns com.wsscode.pathom.connect.ynab.entity-adapter
  (:require [clojure.string :as str]))

(defn adapt-key [k]
  (str/replace k #"_" "-"))

(defn update-if [m k f & args]
  (if (contains? m k)
    (apply update m k f args)
    m))

(defn update-if-some [m k f & args]
  (if (some? (get m k))
    (apply update m k f args)
    m))

(defn set-ns
  "Set the namespace of a keyword"
  [ns kw]
  (keyword ns (adapt-key (name kw))))

(defn set-ns-seq
  "Set the namespace for all keywords in a collection. The collection kind will
  be preserved."
  [ns s]
  (into (empty s) (map #(set-ns ns %)) s))

(defn set-ns-x
  "Set the namespace of a value. If sequence will use set-ns-seq."
  [ns x]
  (if (coll? x)
    (set-ns-seq ns x)
    (set-ns ns x)))

(defn namespaced-keys
  "Set the namespace of all map keys (non recursive)."
  [e ns]
  (reduce-kv
    (fn [x k v]
      (assoc x (set-ns ns k) v))
    {}
    e))

(defn pull-key
  "Pull some key"
  [x key]
  (-> (dissoc x key)
      (merge (get x key))))

(defn pull-namespaced
  "Pull some key, updating the namespaces of it"
  [x key ns]
  (-> (dissoc x key)
      (merge (namespaced-keys (get x key) ns))))
