;;Port of the google charts gannt chart demo to
;;clojurescript.
(ns figdemo.gantt
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as async]
            [goog.dom :as dom]
            [goog.events :as events]))

;;how do we accomplish this?
;;google.charts.load('current', {'packages':['gantt']});
;;google.charts.setOnLoadCallback(drawChart);

;;this form doesn't work, complains about no load method...
;;(.load js/google "charts" "current" (clj->js {:packages ["gantt"]}))
;;if you look at what the google example pulls in, there's
;;a google object with :charts field
;;which has a :load field...

;;(.load js/google "charts" "current" (clj->js {:packages ["gantt"]}))

(defn load-charts [& packages]
 (let [f (.-load (.-charts js/google))]      
   (f "current" (clj->js {:packages (vec packages)}))))
(load-charts "gantt" "table")

;;so, this is a hacked way to accomplish the load process...
;;it appears to work ok...
;;more idiomatic way?
;(.charts js/google   (load "current" (clj->js {:packages ["gantt"]})))

;;(.setOnLoadCallback js/google draw-chart)

(defn days->millis [d] (* d 24 60 60 1000))

;;var formatter_long = new google.visualization.DateFormat({formatType: 'long'});
(defn ->formatter []
  (google.visualization.DateFormat.
   (clj->js {:formatType "medium"})))
;;var formatter_medium = new google.visualization.DateFormat({formatType: 'medium'});
;;var formatter_short = new google.visualization.DateFormat({formatType: 'short'});


;;these are the fields the google charts
;;api expects
(def ganttschema ["Task ID" "string"
                  "Task Name" "string"
                  "Start Date" "date" 
                  "End Date" "date"
                  "Duration" "number" 
                  "Percent Complete" "number"
                  "Dependencies" "string"])

(defn if-empty
  ([x v]
   (if (= x "") v
       x))
  ([x v f]
   (if (= x "") v
       (f x))))

;; (defn ->date [x]
;;   (js/Date. x))

(defn ->date [x]
  (let [[d m y] (clojure.string/split x #"/")]
    (js/Date. (long y)  (dec (long d)) (dec (long m)) 0 0)))

;;note the wierdness...
;;we use cljs.read/read-string
;;instead of read-string

;;currently we assume rows are lines.
;;coerce a line of text into a gantt row.
(defn gantt-row [l]
  (let [xs (clojure.string/split l #"\t")
        [task name start end dur per deps] xs]
    [task
     name
     (if-empty start nil ->date)
     (if-empty end nil ->date)
     (if-empty dur nil cljs.reader/read-string)
     (if-empty per 0 cljs.reader/read-string)
     deps]))

(defn ->data-table
  ([] (js/google.visualization.DataTable.))
  ([fld-types] (reduce (fn [acc [type fld]]
                      (do (. acc (addColumn type fld))
                          acc))
                    (js/google.visualization.DataTable.)
                    fld-types)))

(defn conj-rows [tbl xs]
  (doseq [r   (->> xs
                   (map clj->js))]
    (do (println [:adding r])
        (.addRow tbl r)))
  tbl)

(defn gantt-table [& [xs]]
  (let [schm (->> ganttschema
                  (partition 2)
                  (map (fn [[l r]] [r l])))]
    (->
     (->data-table schm)
      (conj-rows  xs)
     )
  )
)

;; //the the chart, defined by xs, into element tgt.
;; function drawChart(lines, tgt){
;;     var data = ganttTable(lines);        
;;     var options = {height: 275};
;;     var chart = new google.visualization.Gantt(document.getElementById(tgt));
;;     chart.draw(data, options);
;; }
(defn draw-chart [data tgt & {:keys [height] :as options :or {height 275}}]
  (let [opts  (clj->js ;(assoc
                        options
                              ;:hAxis
                           ;   {:format "M/d/yy"}
        ;                      )
        )
        el    (dom/getElement tgt)
        chart (google.visualization.Gantt. el)]
    (do (. chart (draw data opts)))))
  
(defn draw-table [data tgt]
  (let [tbl  (google.visualization.Table. (dom/getElement tgt))]
    (. tbl (draw data (clj->js {:showRowNumber true,
                                :width "100%"
                                :height "100%"})))))
