(ns web-steroid.core.alpha.shadow-cljs
  (:require
   [clojure.data.json :as json]
   [clojure.java.io :as jio]
   [weavejester.dependency :as dep]
   ))


(defn- dependency-graph
  [modules-config]
  (reduce
    (fn [g {:keys [module-id depends-on]}]
      (reduce #(dep/depend %1 module-id %2) g depends-on))
    (dep/graph)
    modules-config))


(defn transitive-modules
  [modules-config]
  (let [deps-graph (dependency-graph modules-config)]
    (reduce
      (fn [ret {:keys [module-id] :as module}]
        (let [transitive-deps (dep/transitive-dependencies deps-graph module-id)]
          (assoc ret module-id
            (conj
              (into []
                (filter (fn [{:keys [module-id]}] (contains? transitive-deps module-id)))
                modules-config)
              module))))
      {}
      modules-config)))


(defn transitive-body-script-modules
  [trans-modules]
  (reduce-kv
    (fn [m module-id modules]
      (assoc m module-id
        (into []
          (map #(select-keys % [:output-name]))
          modules)))
    {}
    trans-modules))


(defn read-manifest-json
  "return `modules-config`"
  [from]
  (with-open [rdr (jio/reader from)]
    (json/read rdr :key-fn keyword)))
(ns web-steroid.core.alpha.shadow-cljs)
