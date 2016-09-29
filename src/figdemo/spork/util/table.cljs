;;Selected functions borrowed from spork.
;;We'll actually port these later, ideally
;;spork should be xcompatible with cljs
;;and clj
(ns figdemo.spork.util.table
  (:require [figdemo.spork.util.parsing :as parse]))

(defn split-by-tab [x] (clojure.string/split x #"\t"))
;;borrowed from spork
(defn unify-schema [s fields]
  (let [fk (keyword? (first fields))
        xform (if fk (fn [x] (if (keyword? x) x (keyword x)))
                  name)]
    (reduce-kv (fn [acc k v]
                 (assoc acc (xform k) v))
               {} s)))

(defn derive-type [x]
  (cond (number? x) (if (integer? x) :long :double)
        (string? x)  :string
        (symbol? x)  :symbol
        (keyword? x) :keyword 
        (vector? x)  :vector
        (map? x)     :map
        (seq x)      :seq
        :else (throw (js/Error. (str "unsupported parsing type " x)))
        ))

(defn derive-schema [row & {:keys [parsemode]}]
  (let [xs     (split-by-tab row)
        parser (if (= parsemode :scientific)  parse/parse-string
                    parse/parse-string-nonscientific);clojure.edn/read-string             
        types (map  (comp derive-type parser) xs)]
    (mapv (fn [t]
            (case t 
               (:string :symbol)  :text
               :long :long
               :double  :double               
               :else (throw (js/Error. (str "unsupported parsing type " t))))) types)))

(defn lines->records
  "Produces a reducible stream of 
   records that conforms to the specifications of the 
   schema.  Unlike typed-lines->table, it does not store
   data as primitives.  Records are potentially ephemeral 
   and will be garbage collected unless retained.  If no 
   schema is provided, one will be derived."
  [ls schema & {:keys [parsemode keywordize-fields?] 
                :or   {parsemode :scientific
                       keywordize-fields? true}}]
  (let [raw-headers   (mapv clojure.string/trim (clojure.string/split  (first ls) #"\t" ))
        fields        (mapv (fn [h]
                              (let [root  (if (= (first h) \:) (subs h  1) h)]
                                (if keywordize-fields?
                                  (keyword root)
                                  root)))
                            raw-headers)
        schema    (if (empty? schema)
                    (let [types (derive-schema (first (drop 1 ls)) :parsemode parsemode)]
                      (into {} (map vector fields types)))
                    schema)
        s         (unify-schema schema fields)
        parser    (parse/parsing-scheme s)
        idx       (atom 0)
        idx->fld  (reduce (fn [acc h]
                            (if (get s h)
                              (let [nxt (assoc acc @idx h)
                                    _   (swap! idx unchecked-inc)]
                                nxt)
                              (do (swap! idx unchecked-inc) acc))) {} fields)
        ;;throw an error if the fld is not in the schema.
        _ (let [known   (set (map name (vals idx->fld)))
                missing (filter (complement known) (map name (keys s)))]
            (assert (empty? missing) (str [:missing-fields missing])))]                                          
    (->> ls
         (drop 1)
         (map  (fn [^String ln] (.split ln "\t")))
         (map  (fn [^objects xs]
                   (reduce-kv (fn [acc idx fld]
                                  (assoc acc fld (parser fld (aget xs idx))))
                              {} idx->fld))))))
        

(defn records->tab-delimited [xs]
  (let [hd   (first xs)
        flds (vec (keys hd))
        sep  (str \tab)
        header-record (reduce-kv (fn [acc k v]
                                   (assoc acc k
                                          (name k)))
                                 hd
                                 hd)]
    (clojure.string/join \n
      (concat [header-record]
              (map (fn [r] (clojure.string/join sep
                                                (map r flds))))
                   (rest xs)))))
