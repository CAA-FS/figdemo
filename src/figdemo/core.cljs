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
            [reagent.core :as r]
            [re-com.core   :refer [h-box gap v-box hyperlink-href p]]))

(enable-console-print!)

(println "This text is printed from src/figdemo/core.cljs. Go ahead and edit it and see reloading in action.")
(println "heyo!!!")
;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {:text "Hello world!"
                          :table-node "Table goes here!"
                          :chart-node "Chart-goes here!"
                          :tree-node  "Tree goes here!"
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
(defn load-tad [& {:keys [source] :or {source "tad-file"}}]
  (go (let [xs (async/<!    (io/file->lines!! (io/current-file :el source)))
            db (tad/txt->tad-db xs)            
            ;;this gives us a renderable widget ala util/render! 
            the-path (util/db->path db :id "the-tree")]
        (swap! app-state assoc :path the-path
               :db db)
        ;;given the-path, we can
        #_(util/render! the-path "the-tree")
        )))

;;we'll break up loading and rendering...

(swap! app-state assoc
       :draw draw-current-chart
       :load-tad load-tad)

;;now, can we build a damn gannt chart or what?
;;we'll also need to use .-name a lot....

(defn on-js-reload []
  ;;optionally touch your app-state to force rerendering depending on
  ;;your application
  ;;(swap! app-state update-in [:__figwheel_counter] inc)
  (swap! app-state assoc :draw     draw-current-chart
                         :load-tad load-tad))

;;setup our button click to trigger rendering the chart
;;the "old fashioned" way
(util/listen! controls/draw-button     "click" (fn [_] ((:draw @app-state))))
(util/listen! controls/load-tad-button "click" (fn [_] ((:load-tad @app-state))))

;;we can actually refactor these into "selector" components.
(defn tad-selector []
   [:div {:id "tad-selector"}
       "A form"
       [:form 
        "TADMUDI-file:"   [:input {:type "file"
                                   :name "tad-file-r"
                                   :id   "tad-file-r"
                                   :on-click (fn [e] (println "loading-tad!"))}]
        "Load-TADMUDI:"   [:input {:type "button"
                                   :name "load-tad-button-r"
                                   :id   "load-tad-button-r"
                                   :on-click (fn [e]
                                               (do (load-tad :source "tad-file-r"))
                                                   (println "loading-tad-db!!"))}]]])

(defn gantt-selector []
  [:div {:id "gantt-selector"}
   "Our gannt input form..."
   ;;let's work on replacing this with some hiccup html
   [:form 
    "GanttFile:"    [:input {:type "file"
                             :name "gantt-file-r"
                             :id   "gantt-file-r" :on-click (fn [e] (println "loading-gantt!"))}]
    "DrawGantt:"    [:input {:type "button" :name "drawgantt-r" :id "drawgantt-r"
                             :on-click (fn [e] (println "drawing-gantt!"))}]]])

;;using vbox instead of divs and friends.
(defn app-body []
  (let [{:keys [table-node chart-node tree-node]} @app-state]
    [v-box
     :size     "auto" 
     :gap      "10px"
     :children
     [[:h2 "This is all reactive..."]
      [:p "We'll show some interaction here too, charts and sliders."]
      [tad-selector]
      [:div {:id "the-tree"}
       tree-node]
      ;;where we'll store our gannt chart input and other stuff
      [gantt-selector]
      ;;look into using an h-box alternately.
       [:table #_{:align  "left"}
        [:tbody
         [:tr   #_{:valign "top" }
          [:td
           [:div {:id "the-table" :style {:width "700px" :height "300px"}}
            table-node]]         
          [:td
           [:div {:id "the-chart" :style {:align "center" :width "1400px" :height "300px"}}
            chart-node]]]]]
       [:div {:id "bar-chart"}
        [high/chart-component]]
       [:div {:id "bmi"}
        [bmi/bmi-component]]
       ]]))

;;just an example of rendering react components to dom targets.
(defn mount-it
  ([target]
   (r/render-component [app-body] target))
  ([] (mount-it (.-body js/document))))

;(defn ^:export main []
;;initialize the reactive renderer
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
