(ns figdemo.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as async]
            [goog.dom :as dom]
            [goog.events :as events]
            [figdemo.util :as util]
            [figdemo.io :as io]
            [figdemo.controls :as controls]
            [figdemo.gantt :as gantt]
            [figdemo.tadmudi :as tad]
            [figdemo.spork.util.table :as tbl]
            [figdemo.bmi  :as bmi]
            [figdemo.high :as high]
            [reagent.core :as r]))

(enable-console-print!)

(println "This text is printed from src/figdemo/core.cljs. Go ahead and edit it and see reloading in action.")
(println "heyo!!!")
;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {:text "Hello world!"
                          }))

;;we can use channels to munge around the async wierdness.
(defn draw-current-chart []
  (when-let [chrt (io/current-file)]
    (go 
    (let [_     (println (str "drawing the chart at " (.-name chrt)))
          xs    (async/<!     (io/file->lines!! chrt))
          lines (clojure.string/split-lines xs)
          _     (println [:lines (take 2 lines)])
          recs  (map gantt/gantt-row (rest lines))
          data  (gantt/gantt-table (map clj->js recs))          
          ]
      (do (gantt/draw-chart data controls/the-chart)
          (gantt/draw-table data controls/the-table)
          )))))

;;notice, these are reactions, we're wiring stuff up manually.
;;in other frameworks, like re-frame, we can define this a bit
;;more declaratively. 
(defn load-tad []
  (go (let [xs (async/<!    (io/file->lines!! (io/current-file :el "tad-file")))
            db (tad/txt->tad-db xs)            
            ;;this gives us a renderable widget ala util/render! 
            the-path (util/db->path db :id "the-tree")]
        (swap! app-state assoc :path the-path
               :db db)
        ;;given the-path, we can
        (util/render! the-path "the-tree")
        )))

(swap! app-state assoc
       :draw draw-current-chart
       :load-tad load-tad)

;;now, can we build a damn gannt chart or what?
;;we'll also need to use .-name a lot....

(defn on-js-reload []
  ;;optionally touch your app-state to force rerendering depending on
  ;;your application
  ;;(swap! app-state update-in [:__figwheel_counter] inc)
  (swap! app-state assoc :draw draw-current-chart
                         :load-tad load-tad))

;;setup our button click to trigger rendering the chart
(util/listen! controls/draw-button "click"
              (fn [_] ((:draw @app-state))))

(util/listen! controls/load-tad-button "click"
  (fn [_] ((:load-tad @app-state))))


;;hiccupification of our html.  should translate.  
(defn app-body [] 
  [:div {:id "highchart-app"}
   [:h2 "This is all reactive..."]
   [:p "We'll show some interaction here too, charts and sliders."]
  ;; [:div {:id "tad"}
  ;;  [:form 
  ;;   "TADMUDI-file:"   [:input {:type "file"
  ;;                              :name "tad-file"
  ;;                              :id   "tad-file"}]
  ;;   "Load-TADMUDI:"   [:input {:type "button"
  ;;                              :name "load-tad-button"
  ;;                              :id "load-tad-button"}]]]
  ;; [:div {:id "the-tree"}]
  ;; ;;where we'll store our gannt chart input and other stuff
  ;; [:div {:id "ganttdemo"}
  ;;  ;;let's work on replacing this with some hiccup html
  ;;  [:form 
  ;;   "GanttFile:"    [:input {:type "file" :name "file" :id "file"}]
  ;;   "DrawGantt:"    [:input {:type "button" :name "drawchart" :id "drawchart"}]]
  ;;  [:table {:align  "left"}
  ;;   [:tr   {:valign "top" }
  ;;    [:td
  ;;     [:div {:id "the-table" :style "width: 700px; height: 300px;"}]]         
  ;;    [:td
  ;;     [:div {:id "the-chart" :style "align: center; width: 1400px; height: 300px;"}]]]]
   ;[:div {:id "bar-chart"}
     [high/home]]
;  ]
)

;;just an example of rendering react components to dom targets.
(defn mount-it
  ([target]
   (r/render-component [app-body] target))
  ([] (mount-it (.-body js/document))))

;(defn ^:export main [] 
   (r/render [app-body] 
             (.getElementById js/document "reagent-app"))
;) 


(comment ; testing
  (do 
    (def res   (io/file->lines (io/current-file)))
    (def xs    @res)
    (def lines (clojure.string/split-lines xs))
    (def recs  (map gantt/gantt-row (rest lines)))
    (def data  (gantt/gantt-table (map clj->js recs)))
    (def el    (dom/getElement "the-chart"))
    (def chart (google.visualization.Gantt. el))
    )
  


)
