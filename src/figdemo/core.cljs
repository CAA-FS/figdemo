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
            [re-com.core   :refer [h-box gap v-box hyperlink-href p] :as recom]
            [re-com.util   :refer [item-for-id]]))

(enable-console-print!)

(println "This text is printed from src/figdemo/core.cljs. Go ahead and edit it and see reloading in action.")
(println "heyo!!!")
;; define your app data so that it doesn't get over-written on reload
(defonce app-state (r/atom {:text "Hello world!"
                          :table-node "Table goes here!"
                          :chart-node "Chart-goes here!"
                          :tree-node  "Tree goes here!"
                            }))

;;testing out to see if a separate atom will work.
;(def menu-items (r/atom nil))

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
(defn load-tad [& {:keys [source data] :or {source "tad-file" data app-state}}]
  (go (let [xs (async/<!    (io/file->lines!! (io/current-file :el source)))
            db (tad/txt->tad-db xs)            
            ;;this gives us a renderable widget ala util/render! 
            ;;the-path (util/db->path db :id "the-tree")
            ]
        (do (swap! data assoc ;:path the-path
                   :db db)
            ;;given the-path, we can
            #_(util/render! the-path "the-tree")
            nil
            )))
      nil)

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
;;so, when we select this, we'll get the tadmudi db loaded up.
;;which creates a :db piece of our app-state.


(defn tad-selector [menu-items]  
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
                                               (do (load-tad :source "tad-file-r" :data menu-items)                                                   
                                                   )
                                               (println "loading-tad-db!!")
                                               )}]]])

;;currently, we have a path-tree.
;;what we'd really like to do is create a list of selection-boxes.
;;that allow us to select a path in the database.
;;expected to have {:id :label [:group?]}

;;adapted from the re-com tutorial.
(defn selection-list [choice-seq & {:keys [data field] :or {field "Selected Value:"}}]
  (let [selected-choice-id (or data (r/atom nil))]
      (fn [] 
       [v-box 
        :gap      "10px" 
        :children [#_[p "The dropdown below shows how related choices can be displayed in groups."
                      "In this case, several country related groups. e.g. 'EN COUNTRIES'."] 
                   #_[p "This feature is triggered if any choice has a " [:code ":group"]
                    " attribute. Typically all choices will have a " [:code ":group"]
                    " or none will. It's up to you to ensure that choices with the same "
                    [:code ":group"] " are adjacent in the vector."] 
                   #_[p "Because :model is initially nil, the " [:code ":placeholder"] " text is initially displayed."] 
                   #_[p [:code ":max-width"] " is set here to make the dropdown taller."] 
                   [h-box 
                    :gap      "10px" 
                    :align    :center 
                    :children [[recom/single-dropdown 
                                :choices     choice-seq 
                                :model       selected-choice-id 
                                :title?      true 
                                :placeholder "Choose a Value" 
                                :width       "300px" 
                                :max-height  "400px" 
                                :filter-box? false 
                                :on-change   #(reset! selected-choice-id %)]
                               [:div 
                                [:strong field] 
                                (if (nil? @selected-choice-id) 
                                  "None" 
                                  (:label (item-for-id @selected-choice-id choice-seq))
                                       )]]]]])))

;;the problem we have with menu-component, is that we're re-computing the menu, or
;;we mean to.  Here, the menu-component only ever takes a single menu-seq,
;;and never updates it.  What we want is to refresh the menu based on
;;some updated data.

;;we'd like to define a component that can take a seq of [field choice-seq]
;;and construct a control that allows one to construct selection by choosing
;;from multiple drop-down boxes to derive a key.
(defn menu-component [menu-seq]
  (let [db (into {} (for [[id choice-seq] menu-seq]
                      (let [data (r/atom nil)]
                        [id data])))]
    [v-box
     :gap "10px"
     :children
     (if (empty? db)
       [[:label "no menu loaded...."]]
       (into []
             (for [[id choice-seq] menu-seq]
               [(selection-list choice-seq :data (get db id) :field id)])))]))

;;given a tad db, compute the menu choices from it.
;;it'd be even better if we could compute the
;;choices interactively...
(defn compute-menu
  ([acc db]
   (if-let [ks (and (map? db) (keys db))]
     (let [lvl (conj acc ks)]
       (compute-menu lvl (get db (first ks))))
     acc)))

(defn db->menu [db]
  (mapv  (fn [id xs]
          [id (map-indexed (fn [idx x]
                             {:id idx :label (str x)}) (sort xs))])
        ;these are pre-baked at the moment...
        ["SRC" "Scenario"  "Measure" "[AC RC]"]
        (compute-menu [] db)))

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
  (let [path (r/atom nil)
        menu-items (r/atom nil)]
    (fn [] 
    (let [{:keys [table-node chart-node tree-node db]} @app-state
          menu   (db->menu  (:db @menu-items))]
      [v-box
       :size     "auto" 
       :gap      "10px"
       :children
       [[:h2 "This is all reactive..."]
        [:p "We'll show some interaction here too, charts and sliders."]
        [tad-selector menu-items]
        [:div {:id "Selection"}
         #_tree-node ;
         #_[selection-list [{:id 1 :label "A"}
                            {:id 2 :label "B"}]]
         [menu-component menu]]
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
        ]]))))

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
