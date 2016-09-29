(ns figdemo.tadmudi
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as async]
            [goog.dom :as dom]
            [goog.events :as events]
            [figdemo.util :as util]
            [figdemo.io :as io]
            [figdemo.controls :as controls]
            [figdemo.gantt :as gantt]
            [figdemo.spork.util.table :as tbl]
            [figdemo.projector :as proj]))

;;the table schema we're working with is...
;;currently
(def tadschema
  {:SRC          :text 
   :ACInventory  :long
   :RCInventory  :long
   :SimulationPolicy :text 
   :DemandSignal :text 
   :AnalysisType :text 
   :ResponseType :text 
   :Period       :text
   :Response     :double})

;;Go grab our records for the database.
;;The strategy here is to build a computational
;;pipeline that we can operate on.
(defn tad-records [xs]
  (tbl/lines->records xs tadschema))

;;once we have the records, we can build a
;;database from them...
;;Let's go ahead and create a supply key.
;;Since most of our queries will be
;;based on the supply/demand for nearest neighbor
;;resolution, we can cache our dataset into a
;;bunch of groups, keyed off [supply demand].
;;So, we'll get...
;; {[ACInventory RCInventory]
;;    {:SRC
;;     {[:SimulationPolicy :DemandSignal  :AnalysisType]
;;      {:ResponseType {:Period       
;;                      :Response}}}}}

;;our taxonomy is
;;[SRC
;; [SimulationPolicy DemandSignal AnalysisType]
;; ResponseType
;; [ACInventory RCInventory]
;; Period
;; Response]
(defn path-key [{:keys [ACInventory RCInventory
                        SRC SimulationPolicy DemandSignal AnalysisType
                        ResponseType]}]
  [SRC
   [SimulationPolicy DemandSignal AnalysisType]
   ResponseType
   [ACInventory RCInventory]])
  
(defn distinct-fields [xs]
  (reduce (fn [acc r]
            (reduce-kv (fn [acc k v]
                         (assoc acc k
                                (conj (get acc k #{}) v)))
                       acc r))
          {} xs))

;;Builds up a nested database
(defn tad-db [xs & {:keys [keyf]
                    :or   {keyf  (juxt :ACInventory
                                       :RCInventory)}}]
  (let [field-domains (distinct-fields xs)]
    (reduce (fn [acc  r]
              (let [k  (path-key r)
                    rs (get-in acc k [])]
                (assoc-in acc k (conj rs (select-keys  r [:Period :Response])))))
            (with-meta {} {:domains (distinct-fields xs)})
            xs)))

;;we can query the db to find samples.
(defn find-sample [db [src [pol demand atype] rtype [ac rc]]]
  (for [[p xs]  (->> [src [pol demand atype] rtype [ac rc]]
                     (get-in db)
                     (group-by :Period))
        r xs]
    [p (:Response r)]))

;;what if we can't find a sample?
(defn sample-neighbors [db [src [pol demand atype] rtype [ac rc]]]
  (get-in db [src [pol demand atype] rtype]))

;;we could replace this with core.logic
;;relations...


;;Note:  I'm operating off the assumption
;;that like-samples exist between cases...

;;Construct a sample by:
;;-Select a Demand  
;;  -Select an SRC
;;    -Select a Supply [ac rc]

;;one common operation will be...
;;find left sample,
;;find right sample?
;;  Do we provide compatible samples?
(defn compatible-samples [db l & {:keys [same-src?]}]
  (let [[src case res & more] l]
    (for [[other-src cases] db
          [othercase results] cases
          [restype qs] results 
          [q xs] qs
          :when (and (= case othercase)
                     (= restype  res)
                     (if same-src? (= src other-src)
                         true))]
      [[other-src case restype q] xs])))

;;we have a couple of operations that we'd like to
;;perform:
;;browse sample(s)...
;;A sample points to a collection of records.
;;The records are keyed by period and response.

;;
(def lits #{"[" "{" "#" ":"})
(defn read-path [xs]
  (for [x xs]
    (if (lits (aget x 0)) (cljs.reader/read-string x)
        x)))

(defn db->pathdb [db]
  (if (map? db) 
    (for [[k v] (seq db)]    
      (if (map? v)
        {:name k
         :childNodes (vec (db->pathdb v))}
        {:name k
         :url "blah"}))
    {:name db
     :url "blah"}))


;;given a set of compatible-samples...
;;how can we interpolate?
   
(comment ;testing
  (def res  (io/file->lines (io/current-file)))
  (def recs (tbl/lines->records (clojure.string/split-lines @res)
               figdemo.tadmudi/tadschema))
  (def db (figdemo.tadmudi/tad-db recs))


  (defn make-sample-data []
    (for [src ["770200R00" 
               "Bilbo"
               "Garth"
               "Jabberwocky"]
          policy ["MaxUtilization"
                  "Rotational"]
          demand ["SteadyState"
                  "SS+Surge"]
          atype ["Dynamic"]
          ac-inv (range 10 20 3)
          rc-inv (range 4  18 4)
          r-type ["Fill" "Surplus"]
          p ["PreSurge" "Surge" "PostSurge"]]
      {:SRC src :ACInventory ac-inv :RCInventory rc-inv :SimulationPolicy policy
       :DemandSignal demand :AnalysisType atype :ResponseType r-type :Period p
       :Response (case r-type
                   "Fill"  (rand)
                   (rand-int 10))}))
    
        
)

(defn td [])
