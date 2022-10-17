(ns web-steroid.hooks.alpha
  (:require
   [clojure.string :as str]
   [clojure.java.io :as jio]
   [web-steroid.core.alpha.shadow-cljs :as w.shadow-cljs]
   ))


(defn copy-manifest-file
  "Delete :from when build.mode is release"
  {:shadow.build/stage :flush}
  [state ssr-root & [delete?]]
  (let [manifest-name (get-in state [:shadow.build/config :build-options :manifest-name])
        from          (jio/file (get-in state [:shadow.build/config :output-dir]) manifest-name)
        to            (jio/file ssr-root manifest-name)]
    (jio/copy from to)
    (when (or delete? (= (:shadow.build/mode state) :release))
      (jio/delete-file from to)))
  state)


(defn print-body-script-modules
  {:shadow.build/stage :flush}
  [state ssr-root & [name]]
  (let [name (or name "body_script_modules")
        name (if (= (:shadow.build/mode state) :release)
               (str name ".edn")
               (str name ".dev.edn"))]
    (with-open [w (jio/writer (jio/file ssr-root name))]
      (binding [*out* w]
        (println (str ";; " (System/currentTimeMillis)))
        (-> (w.shadow-cljs/read-manifest-json
              (jio/file
                (get-in state [:shadow.build/config :output-dir])
                (get-in state [:shadow.build/config :build-options :manifest-name])))
          (w.shadow-cljs/transitive-modules #_(vals (:shadow.build.modules/config state)))
          (w.shadow-cljs/transitive-body-script-modules)
          ;; apply cljs :asset-path
          (update-vals
            (fn [modules]
              (reduce
                (fn [ret module]
                  (conj
                    ret
                    (-> module
                      (update :output-name
                        (fn [output-name]
                          (if (str/starts-with? output-name "/")
                            output-name
                            (str (jio/file (get-in state [:shadow.build/config :asset-path]) output-name))))))))
                (empty modules)
                modules)))
          (prn)))))
  state)
