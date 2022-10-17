(ns web-steroid.core.alpha.merge
  (:require
   [clojure.spec.alpha :as s]
   ))


;;


(def ^:private last-wins (comp last #(remove nil? %) vector))
(def ^:private append (comp vec concat))
(def ^:private append-unique (comp vec distinct concat))


(def ^:private default-rules
  {
   :merge         merge
   :last-wins     last-wins
   :append        append
   :append-unique append-unique
   })


(s/def ::rule #(or (fn? %) (find default-rules %)))


;;


(defn- coerce-rule
  "Return nil if x is not a rule"
  [x]
  (if (fn? x) x (get default-rules x)))


(defn merge-or-replace
  "If maps, merge, otherwise replace"
  [& vals]
  (when (some identity vals)
    (reduce
      (fn [ret val]
        (if (and (map? ret) (map? val))
          (merge ret val)
          (or val ret)))
      nil vals)))


(defn rule-from-key
  [rules key]
  (coerce-rule (get rules key)))


(defn rule-from-value
  [val]
  (let [meta (meta val)
        ks   (keys meta)
        rule (if (> (count ks) 1)
               (:merge-rule meta)       ; multi meta key
               (first ks))]             ; single meta key -> key is the merge-rule
    (coerce-rule rule)))


(defn choose-rule
  [rules k v]
  {:post [(fn? %)]}
  (or
    (rule-from-key rules k)      ; rule from key
    (rule-from-value v)          ; rule from value
    (if (map? v)                 ; just merge-or-replace when rule decl not found
      merge
      (fn [_v1 v2] v2))))


(defn merge-maps
  [rules & ms]
  {:pre [(or (nil? rules) (map? rules))
         (every? (fn [[_ val]] (s/valid? ::rule val)) rules)]}
  (reduce
    #(reduce
       (fn [m [k v]]
         (let [rule (choose-rule rules k v)]
           (try
             (update m k rule v)
             (catch Throwable _
               (throw (ex-info "Merge failed." {:key k :val v :rule rule}))))))
       %1 %2)
    {} ms))
