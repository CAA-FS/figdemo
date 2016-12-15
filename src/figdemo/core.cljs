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
                   :db db) ;;this is not showing up....
            (swap! app-state assoc :db db)
            ;;given the-path, we can
            #_(util/render! the-path "the-tree")
            nil
            )))
      nil)

;;we'll break up loading and rendering...

;(swap! app-state assoc
;       :draw draw-current-chart
;       :load-tad load-tad)

;;now, can we build a damn gannt chart or what?
;;we'll also need to use .-name a lot....

(defn on-js-reload []
  ;;optionally touch your app-state to force rerendering depending on
  ;;your application
  ;;(swap! app-state update-in [:__figwheel_counter] inc)
  #_(swap! app-state assoc :draw     draw-current-chart
         :load-tad load-tad)
  )

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
;;we can change this to directly store the selected value...
;;so, a [:field, id] pair..., rather than just the selected id.
(defn selection-list [choice-seq & {:keys [data field] :or {field "Selected Value:"}}]
  (let [selected-choice-id  (r/atom nil)
        data  (or data (r/atom nil))
        _     (add-watch selected-choice-id :update
                          (fn [atm k old new]
                            (reset! data (:label (item-for-id new choice-seq)))))]
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
                                #_(if (nil? @selected-choice-id) 
                                  "None" 
                                   @data #_(item-for-id @selected-choice-id choice-seq)
                                       )]]]]])))

;;the problem we have with menu-component, is that we're re-computing the menu, or
;;we mean to.  Here, the menu-component only ever takes a single menu-seq,
;;and never updates it.  What we want is to refresh the menu based on
;;some updated data.

;;A path is just a traversal of selected components.
;;If anything is nil, we don't have a path.
;;Otherwise, we a path into the db...

;;we'd like to define a component that can take a seq of [field choice-seq]
;;and construct a control that allows one to construct selection by choosing
;;from multiple drop-down boxes to derive a key.

;;need to store the selection...
;;If there's an item selected from each menu, then we have a path right?

;;also, it'd be nice to have a dependent menu.  I,e:
;;  select from one menu, creates the next...
;;  After you bottom out, result is stored in a provided atom.
;;  So you have to pass storage in.

;;We'd like to store the selected menu somewhere too...
;;result could be a channel or an atom...
(defn menu-component [menu-seq result]
  (let [update-id! (fn [id]
                     (fn [a k old new]
                       (swap! result assoc id new)))
        db     (into {} (for [[id choice-seq] menu-seq]
                          (let [data (r/atom nil)
                                _   (add-watch data id (update-id! id))]
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

(def structures #{"{" "["})
(defn as-key [x]
  (if (structures (first x))
    (cljs.reader/read-string x)
    (keyword x)))

(defn as-val [x]
  (if (structures (first x))
    (cljs.reader/read-string x)
     x))


(defn current-path []    (get @app-state :current-path))
(defn current-samples []
  (when-let [xs (seq (current-path))]
    (get-in (:db @app-state) xs )))

;;we need a relation between the app data, the path, and the
;;measure.  Basically, we need to tap into
;;the functions in tadmudi/

;;using vbox instead of divs and friends.
(defn app-body []
  (let [menu-items (r/atom nil) ;;if we don't use a ratom, we don't get our path to update on fileload.
        selection  (r/atom nil)
        function-data (r/atom {:x 50
                               :y 50})]
    (fn [] 
      (let [{:keys [table-node chart-node tree-node db]} @app-state
            menu     (db->menu  (:db @menu-items))
            the-path (reduce  (fn [acc [k _]]
                                (if-let [v (get @selection k)]
                                  (conj acc [k (as-val v)])
                                  (reduced nil))) []  menu)
            _        (when the-path (swap! app-state assoc :current-path (mapv second the-path)))
            xy       (second (last the-path))]
        [v-box
         :size     "auto" 
         :gap      "10px"
         :children
         [[:h2 "This is all reactive..."]
          [:p "We'll show some interaction here too, charts and sliders."]
          [tad-selector menu-items]
          [:div {:id "Selection"}
           [menu-component menu selection]       
           [:label {:id "the-path"} (str the-path)]]
          ;;given a path, we'll let the last segment be reactive....
          [:div {:id "Coordinates"}
           ;;Allow the user to dynamically vary the x/y coordinates to recompute Z.
           ;;In this case, x : ac, y : rc, z : measure   
           [bmi/function-slider
            [[:x [0 100]]
             [:y [0 100]]]
            [:z [0 200]]
            (fn [x y] (+ (int x) (int y)))
            function-data
            :title "z = x + y"]          
           ]
          ;;we'll put our reactive bar-chart here...
          ;;Figure out how to change the data for the bar chart dynamically.
          ;;Optionally re-render the whole thing.
          [:div {:id "tad-bar"}
           [high/chart-component]
           ]
          
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
          #_[:div {:id "bmi"}
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
