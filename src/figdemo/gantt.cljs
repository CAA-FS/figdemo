;;Port of the google charts gannt chart demo to
;;clojurescript.
(ns figdemo.gantt
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as async]
            [goog.dom :as dom]
            [goog.events :as events]))

;;how do we accomplish this?
;google.charts.load('current', {'packages':['gantt']});
;google.charts.setOnLoadCallback(drawChart);

(.load js/google "charts" "current" (clj->js {:packages ["gantt"]})) 

;;(.setOnLoadCallback js/google draw-chart)

(defn days->millis [d] (* d 24 60 60 1000))

;;these are the fields the google charts
;;api expects
(def ganntschema ["Task ID" "string"
                  "Task Name" "string"
                  "Start Date" "date" 
                  "End Date" "date"
                  "Duration" "number" 
                  "Percent Complete" "number"
                  "Dependencies" "string"])

(defn if-empty
  [x v]
  (if (= x "") v
      x))

;;currently we assume rows are lines.
;;coerce a line of text into a gantt row.
(defn gannt-row [l]
  (let [xs (clojure.string/split l #"\t")
        [task name start end dur per deps] xs]
    [task
     name
     (js/Date start)
     (js/Date end)
     (if-empty dur nil)
     (if-empty per 0)
     deps]))

(defn ->data-table
  ([] (js/google.visualization.DataTable.))
  ([fld-types] (reduce (fn [acc [fld type]]
                      (do (. acc (addColumn fld type))
                          acc))
                    (js/google.visualization.DataTable.)
                    fld-types)))

(defn conj-rows [tbl xs]
  (do  (->> xs
            (map clj->js)
            (addRows)
            (. tbl ))
       tbl))

(defn gantt-table [& xs]
  (conj-rows (->data-table ganntschema) xs))

;; //the the chart, defined by xs, into element tgt.
;; function drawChart(lines, tgt){
;;     var data = ganttTable(lines);        
;;     var options = {height: 275};
;;     var chart = new google.visualization.Gantt(document.getElementById(tgt));
;;     chart.draw(data, options);
;; }
(defn draw-chart [data tgt & {:keys [height] :as options :or {height 275}}]
  (let [opts  (clj->js options)
        el    (js/getElement tgt)
        chart (google.visualization.Gantt. el)]
    (do (. chart (draw data options)))))
  

