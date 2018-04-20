(ns figdemo.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as async]
            [clojure.pprint :as pprint]
            [goog.dom :as dom]
            [goog.events :as events]
            [figdemo.util :as util]
            [figdemo.io :as io]
            [figdemo.controls :as controls]
;            [figdemo.gantt :as gantt]
            [figdemo.tadmudi :as tad]
            [figdemo.spork.util.table :as tbl]
            [figdemo.bmi  :as bmi]
            [figdemo.high :as high]
            [figdemo.heat :as heat] ;;vega-based heatmap
            [reagent.core :as r]
            [re-com.core   :refer [h-box gap v-box hyperlink-href p button] :as recom]
            [re-com.util   :refer [item-for-id]]))
            ;[cljsjs.vega-lite]
            

(enable-console-print!)

(println "This text is printed from src/figdemo/core.cljs. Go ahead and edit it and see reloading in action.")
(println "heyo!!!")

;; define your app data so that it doesn't get over-written on reload
(defonce app-state (r/atom {:text "Hello world!"
                            :table-node "Table goes here!"
                            :chart-node "Chart-goes here!"
                            :tree-node  "Tree goes here!"}))
                            

;;UTILS
(def structures #{"{" "["})
(defn as-key [x]
  (if (structures (first x))
    (cljs.reader/read-string x)
    (keyword x)))

(defn as-val [x]
  (if (structures (first x))
    (cljs.reader/read-string x)
    x))

(defn bind->
  ([l r]
   (add-watch l :binding
              (fn [atm k old new]
                (reset! r new))))
  ([l r f]
   (add-watch l :binding-by
              (fn [atm k old new]
                (swap! r f new)))))

;;not really using this guy...
(defn ->property [nm init]
  (let [p  (r/atom init) 
        _  (bind-> p app-state (fn [s newval] (assoc s nm newval)))]
    p))

(defn coords->bounds [xys]
  (let [[x0 y0] (first xys)
        left    (atom x0)
        right   (atom x0)
        top     (atom y0)
        bottom  (atom y0)
        do-n (fn [n l r]
               (do (when (< n @l)
                     (reset! l n))
                   (when (> n @r)
                     (reset! r n))))]
    (do  (reduce (fn [acc [x y]]
                   (do (do-n x left right)
                       (do-n y bottom top))) nil xys)
         {:xmin @left :xmax @right
          :ymin  @bottom :ymax @top})))

(defn log [msg v]
  (do (println msg)
      v))

(defn atom? [x] (implements? IDeref x))




;;testing out to see if a separate atom will work.
;(def menu-items (r/atom nil))

;;obe 
;;we can use channels to munge around the async wierdness.
#_(defn draw-current-chart []
   (when-let [chrt (io/current-file)]
     (go 
      (let [_     (println (str "drawing the chart at " (.-name chrt)))
            xs    (async/<!     (io/file->lines!! chrt))
            lines (clojure.string/split-lines xs)
            _     (println [:lines (take 2 lines)])
            recs  (map gantt/gantt-row (rest lines))
            data  (gantt/gantt-table (map clj->js recs))]          
          
        (do (gantt/draw-chart data controls/the-chart)
            (gantt/draw-table data controls/the-table))))))
          

;;notice, these are reactions, we're wiring stuff up manually.
;;in other frameworks, like re-frame, we can define this a bit
;;more declaratively. 
(defn load-tad [& {:keys [source data] :or {source "tad-file" data app-state}}]
  (go (let [xs (async/<!    (io/file->lines!! (io/current-file :el source)))
            db (tad/txt->tad-db xs)]            
            ;;this gives us a renderable widget ala util/render! 
            ;;the-path (util/db->path db :id "the-tree")
            
        (do (swap! data assoc ;:path the-path
                   :db db) ;;this is not showing up....
            (swap! app-state assoc :db db)
            ;;given the-path, we can
            #_(util/render! the-path "the-tree")
            nil)))
            
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
         :load-tad load-tad))
  

;;setup our button click to trigger rendering the chart
;;the "old fashioned" way
#_(util/listen! controls/draw-button     "click" (fn [_] ((:draw @app-state))))
#_(util/listen! controls/load-tad-button "click" (fn [_] ((:load-tad @app-state))))

;;we can actually refactor these into "selector" components.
;;so, when we select this, we'll get the tadmudi db loaded up.
;;which creates a :db piece of our app-state.


(defn tad-selector [menu-items]  
   [:div {:id "tad-selector"}
       "Select and Load a Valid TADMUDI Dataset"
       [:form 
        "TADMUDI-file:"   [:input {:type     "file"
                                   :name     "tad-file-r"
                                   :id       "tad-file-r"
                                   :on-click (fn [e] (println "loading-tad!"))}]
        "Load-TADMUDI:"   [:input {:type "button"
                                   :name "load-tad-button-r"
                                   :id   "load-tad-button-r"
                                   :on-click (fn [e]                                               
                                               (do (load-tad :source "tad-file-r" :data menu-items))                                                   
                                                   
                                               (println "loading-tad-db!!"))}]]])
                                               

;;currently, we have a path-tree.
;;what we'd really like to do is create a list of selection-boxes.
;;that allow us to select a path in the database.
;;expected to have {:id :label [:group?]}

;;adapted from the re-com tutorial.
;;we can change this to directly store the selected value...
;;so, a [:field, id] pair..., rather than just the selected id.
(defn selection-list [choice-seq & {:keys [data choice field] :or {field "Selected Value:"}}]
  (let [selected-choice-id  (or choice (r/atom nil))
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
                                :width       "100px" 
                                :max-height  "200px" 
                                :filter-box? false 
                                :on-change   #(reset! selected-choice-id %)]
                               [:div 
                                [:strong field] 
                                #_(if (nil? @selected-choice-id) 
                                   "None" 
                                   @data #_(item-for-id @selected-choice-id choice-seq))]]]]])))
                                   

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


;;we'd like to have a lazily-computed, dependent set of selectors,
;;that is, for each selection, i'd like to only get choices from the current
;;path delineated by the selection, if there are any...

;;so, we have, as an input, a nested map.
;;If we have a terminal node, selection doesn't change anything.
;;If we have a branch node, then changing the selection alters
;;child selector behavior (typically by constraining choices).
;;we only need to maintain the path along the map.
;;the choices at any given level are equivalent to the keys of the map at said level in the path.
(defn maybe-keys [x]   (when (map? x) (keys x)))
(defn choices    [m p] (maybe-keys  (get-in m p)))

;;this gives us an unraveling choice tree.  As we
;;select choices, determined by p, our later choices are
;;constrained.  not worried a whole-lot about performace on
;;this one.
(defn choice-tree [m p]
  (when-let [ks  (maybe-keys m)]
    (concat [(sort ks)]
            (choice-tree (get m (first p)) (rest p)))))



;;given a tad db, compute the menu choices from it.
;;it'd be even better if we could compute the
;;choices interactively...
(defn compute-menu
  ([acc db]
   (if-let [ks (and (map? db) (keys db))]
     (let [lvl (conj acc ks)]
       (compute-menu lvl (get db (first ks))))
     acc)))

(defn db->menu [db & {:keys [labels] :or {labels ["SRC" "Scenario"  "Measure" "[AC RC]"]}}]
  (mapv  (fn [id xs]
          [id (map-indexed (fn [idx x]
                             {:id idx :label (str x)}) (sort xs))])
        ;these are pre-baked at the moment...
        labels
        (compute-menu [] db)))

(defn choices->menu [xs & {:keys [labels] :or {labels ["SRC" "Scenario"  "Measure" "[AC RC]"]}}]
  (mapv  (fn [id xs]
          [id (map-indexed (fn [idx x]
                             {:id idx :label (str x)}) (sort xs))])
        ;these are pre-baked at the moment...
        labels
        xs))


;;we have to use this going to/from the text input to
;;clojure....not a huge deal, but it's something we  have
;;to remember to do, particularly if we have clojure
;;data-structures as keys in nested maps.
(defn coerce [x]
  (if (string? x)
    (if (#{"[" "{" "#"} (first x)) (cljs.reader/read-string x)
        x)
    x))

(defn compute-path [old-path idx new]
  (let [new (coerce new)]
    (cond (zero? idx)  [new]
          (= idx (count old-path))       (conj old-path new)
          (= idx (dec (count old-path))) (assoc old-path idx  new)
          :else
          (conj (into [] (take idx old-path))
                new))))
  
(def last-choices (r/atom nil))
;;the problem atm is that the "model" is the choice var, not the data
;;var.  We need to 

;;so we have a seq of selected-choice-ids (atoms) that get altered.
;;we need to update the id when we rebuild the path.

;;given a map, maintains a computed path through the map, and provides a
;;reactive, dependent set of selection-boxes that reflect the possibly choices
;;given the constraints of the current path.
(defn ->map-selector [path & {:keys [data labels field] :or {field "Selected Value:"}}]
  (let [lbls          (if (seq labels) labels (take 5 (iterate inc 0)))
        idx->lbl      (into {} (map-indexed (fn [idx l] [idx (str l)]) lbls))
        lbl->idx      (reduce-kv (fn [acc k v] (assoc acc v k)) {} idx->lbl)
        compute-menu  (fn [choices p] (-> (choice-tree choices p)
                                          (choices->menu :labels lbls)))
        compute-path! (fn [idx] ;;at the path-index, we have a change.                        
                        (fn [a k old new]
                          (do (swap! path
                                (fn [old-path]                                  
                                  (compute-path old-path idx new))))))
        find-choice-id (fn [choices k]
                         (let [k (str k)]
                           (or (some #(when (= (:label %) k) (:id %)) choices)
                               (throw (js/Error. (str [:invalid-choice! k :for choices]))))))]
    (fn [choices]
      (let [menu-seq    (compute-menu choices @path)
            n           (dec          (count @path))
            ;;db is recomputed after every selection....if we retain the selection, we
            ;;should have our values...
            db  (into {} (for [[lbl choice-seq] menu-seq]
                           (let [data   (r/atom  (let [id (lbl->idx lbl)]
                                                   (when (<= id n)
                                                     (nth @path id))))
                                 choice (r/atom (when @data (find-choice-id choice-seq @data)))
                                 _      (add-watch data lbl (compute-path! (int (lbl->idx lbl))))]
                             [lbl {:data data :choice choice}])))]
            
       [v-box
        :gap "10px"
        :children
        (if  (empty? db)     [[:label "no menu loaded...."]] ;;empty menu
          (into [[:label (str {:path @path})]] ;;menu with current path.
                (for [[lbl choice-seq] menu-seq]
                  (let [{:keys [data choice]} (get db lbl)]
                    [(selection-list choice-seq :data data :choice choice :field lbl)]))))]))))

#_(defn gantt-selector []
   [:div {:id "gantt-selector"}
    "Our gannt input form..."
   ;;let's work on replacing this with some hiccup html
    [:form 
     "GanttFile:"    [:input {:type "file"
                              :name "gantt-file-r"
                              :id   "gantt-file-r" :on-click (fn [e] (println "loading-gantt!"))}]
     "DrawGantt:"    [:input {:type "button" :name "drawgantt-r" :id "drawgantt-r"
                              :on-click (fn [e] (println "drawing-gantt!"))}]]])


(defn current-path []    (get @app-state :current-path))
;;an accessor for getting us a path-map...
(defn path-map
  ([]   (tad/path->map (current-path)))
  ([nm] (tad/path->map (get @app-state nm))))

(defn paths-from [db acc path-spec]
  (when-let [k (first path-spec)]
    (case k
      :?   ;;look up multiple paths.      
      (for [k (keys db)]
        (conj acc (paths-from (get db k) (rest path-spec))))
      (conj acc (get db k) (rest path-spec)))))  ;;look up a single path.      
      
    

;;based on the path-map
;(:SRC :DemandSignal :SimulationPolicy :ResponseType :ACRC)

;;brittle..
(defn current-measure [] (:ResponseType (path-map)))
(defn current-supply  [] (:ACRC (path-map)))

;;looking up values in our database...
(defn current-samples []
  (when-let [xs (seq (current-path))]
    (get-in (:db @app-state) xs)))

(defn butlast-path [p]
  ((juxt :SRC :DemandSignal :SimulationPolicy :ResponseType)
   p))

(defn current-candidates []
  (when-let [p (path-map)]
    (when-let [k (and (:ResponseType p)
                      (butlast-path p))]
      (->> (get-in (:db @app-state) k)
           (map first)))))

;;this is still reporting :bmi, we don't want that.
(defn current-data []
  (when (current-supply)
    (when-let [d (get @app-state :function-data)]
      (let [{:keys [AC RC]} @d]
        [(int AC) (int RC)]))))

;;note: since we have a limited number of keys, we can probably
;;cache this...
(defn sample-range []
  (when-let [xs (current-candidates)]
    (assoc (coords->bounds xs)
           :measure (current-measure))))
    
(def nearest! 
  (memoize
    (fn [db p]
      (tad/nearest-samples db p))))

;;this allows us to wrap the tadmudi api,
;;so we can traverse the current db a couple of
;;different ways.
(defn nearest-samples
  ([p xy]
   (let [newp (conj (into [] (butlast p)) xy)]
     (nearest-samples newp)))
  ([p]
   (let [db (:db @app-state)]
     (when db
       (if-let [res (get-in db p)]
         res
         (#_tad/nearest-samples nearest! db p)))))
  ([]
   (when-let [xs (seq (current-path))]
     (when (= (count xs) 5) ;;brittle!
       (nearest-samples xs)))))

(defn nearest-trends
  ([p xy]
   (when-let [xs (seq (nearest-samples p xy))]
     (into [] (map (fn [kv]
                     (if (map? kv) kv
                         (let [[k v] kv]
                           {:Period   k
                            :Response (nth v 2)}))) xs))))
   
  ([xy] (nearest-trends (current-path) xy))
  ([] (when-let [acrc (current-data)]                 
        (nearest-trends acrc))))

(defn random-samples
  ([n p]
   (when-let [bounds (sample-range)]
     (let [{:keys [xmin xmax ymin ymax]} bounds
           w (- xmax xmin)
           h (- ymax ymin)
           rand-point (fn []                       
                        [(+ xmin (rand-int w))
                         (+ ymin (rand-int h))])]
       (map (fn [_] (let [xy (rand-point)]
                      [xy (nearest-trends p xy)])) (range n)))))
  ([n] (random-samples n)))

(defn enumerated [bounds]
  (let [{:keys [xmin xmax ymin ymax]} bounds]
    (* (- xmax xmin) (- ymax ymin))))

(defn sparse-samples
  ([n p]
   (when-let [bounds (sample-range)]
     (let [{:keys [xmin xmax ymin ymax measure]} bounds
           w   (- xmax xmin)
           h   (- ymax ymin)]
       (if (<= (* w h) n)
         (for [x (range xmin xmax)
               y (range ymin ymax)]
           (mapv (fn [r]
                   (merge {:AC x :RC y :measure measure} r)) (nearest-trends p [x y])))
         (random-samples n)))))
  ([n] (sparse-samples n (current-path))))

;;generates a sample from the surface, trying to keep the
;;total sample size reasonable.  If it's possible to
;;discretely sample everything, will do.
(defn sample-surface
  ([p]
   (when-let [bounds  (sample-range)]
     (let [total (enumerated bounds)]
       (apply concat (if (< total 1000)
                       (sparse-samples total p)
                       (sparse-samples 1000 p))))))
  ([] (sample-surface (current-path))))

;;let's define a function that takes the current path, and
;;some variables, and returns corresponding paths...
;;note: this is ripe for logic programming
     
        

;;allows us to select numeric ranges for the key.
;;f(ac rc) -> (current
(defn ->range-selector [& {:keys [function-data f]}]
  (let [function-data (or function-data (r/atom {:AC 0 :RC 0}))
        f             (or f   (fn [x y] (+ (int x) (int y))))]
    (fn [& [ac-rc]]      
      (when-let [sr (sample-range)]
        (let [{:keys [xmin xmax ymin ymax measure]} sr
              _ (when ac-rc (swap! function-data
                                   #(merge % {:AC (first ac-rc)
                                              :RC (second ac-rc)})))]
          [bmi/function-slider
           [[:AC [xmin xmax]]
            [:RC [ymin ymax]]]
           [measure []]
           f
           function-data
           :title (str "z = " measure "(AC,RC)")])))))

;;we need a relation between the app data, the path, and the
;;measure.  Basically, we need to tap into
;;the functions in tadmudi/
(defn current-trends []
  (when-let [samp (current-samples)]
    (when (vector? samp) samp)))

(defn trends->txt [xs]
  (when @xs
    (reduce (fn [acc m]
              (conj acc 
                    [:div {}
                     [:label (str (m :Period) ": " (m :Response))]]))
            [:div {:id "blah"}]  @xs)))

(defn acrc-changed [old new]
   (let [{:keys [AC RC]} new
         AC (int AC)
         RC (int RC)]
     (when
       (or (not= (int (:AC old)) AC) 
           (not= (int (:RC old)) RC))
       [AC RC])))
  
  
;;we want to render our path as a collection of surfaces.
(defn render-surface!
  ([xs]
   (let [m (:measure (first xs))]
     (let [fd (:function-data @app-state)
           xy @fd
           _  (heat/draw! :surface-chart
               (heat/data->heatfacet! xs :AC :RC :Response :Period :xtitle "AC" :ytitle "RC" :ztitle m))          
           _  (add-watch fd :cursor-movement 
                (fn [a k old new]
                  (when-let [acrc (acrc-changed old new)]
                    (let [[AC RC] acrc]
                      (heat/set-cursor (:surface-chart-view @heat/app-state) 
                        :AC :RC AC RC)))))])))
  ([] (render-surface! (sample-surface))))

;;The way FM has it, we have by surge by policy...
;;Values are normalized.

(defn group-paths []
  (let [p              (current-path)
        [src demand policy measure acrc] p
        acrc (or (current-data) acrc)
        db (:db @app-state)]
    (for [[dem xs] (get db src)
          [pol ys] xs]
          
      [src dem pol measure acrc]))) 
      

(defn group-data []
  (apply concat
    (for [[src dem pol measure acrc :as p] (group-paths)]
      (map #(merge % {:SRC src :demand dem :policy pol :measure measure}) (nearest-trends p (last p))))))

;;for a given selection of trends, render the corresponding
;;grouped bar chart that looks good.
(defn render-groups!
  ([xs]
   (let [m  (:measure (first xs))
         fd (:function-data @app-state)         
         xy @fd
         groups->datums (fn [xs]
                          (->> xs
                               (mapv (fn [r] (assoc r :trend
                                                    (str #_(:SRC r) #_"-" (:demand r) "-" (:policy r))
                                                    :Response (util/as-decimal (:Response r) 2))))))
                                                    
                       
         grps (-> xs
                  (groups->datums)
                  (heat/grouped-bars {:valfield   :Response
                                      :trendfield :trend
                                      :catfield   :Period
                                      :xtitle (:measure (first xs))
                                      :ytitle "Period"}))         
           ;;when we move our acrc supply cursor, we get different bars.
         _ (add-watch fd :group-movement
                     (fn [a k old new]
                       (when (acrc-changed old new)
                         (let [grps (->> (group-data)
                                         (groups->datums)
                                         (clj->js))
                               bc   (:bar-chart-view @heat/app-state)
                               
                               data (. bc data "table")
                               _    (.remove data (fn [_] true))
                               _    (.insert data grps)]
                           
                           #_(heat/draw! :bar-chart grps)
                           (.update bc)))))]     
     (heat/draw! :bar-chart grps)))
  ([] (render-groups! (group-data))))

(defn draw-charts! []
  (do (render-groups!)
      (render-surface!)))

(defn clear-charts! []
  (do (heat/draw! :bar-chart #js{})
      (heat/draw! :surface-chart #js{})))

;;layout helpers, makes it a tiny bit more readable...
(defn beside [& xs]
  [h-box :size     "auto" 
         :gap      "10px"
   :children (vec xs)])

(defn above [ & xs]
  [v-box
   :size     "auto" 
   :gap      "10px"
   :children (vec xs)])

(defn notes []
  [hyperlink-href 
   :label     "Read Notes" 
   :tooltip   "Learn more about the tool..." 
   :href       "assets/notes.html"])


;;we want to have a chart rendering channel.
;;Basically, communicate with the chart by pushing new trends to a channel,
;;then have interested subscribers update based on said trend.
;;Using channels simplifies this significantly....
;;Alternately, we can use atoms...

;;using vbox instead of divs and friends.
(defn app-body []
  (let [menu-items     (r/atom nil) ;;if we don't use a ratom, we don't get our path to update on fileload.
        selection      (r/atom nil)        
        function-data  (r/atom {:AC 0 :RC 0});(->property  :function-data {:AC 0 :RC 0})
        map-path       (r/atom []) ;(->property  :current-path  [])
        nt             (r/atom nil)
        update-trends! (fn [x y] (let [tr   (nearest-trends)
                                        _   (reset! nt tr)]
                                       
                                   tr))
                                        ;(+ x y)
                                     
        _             (bind-> map-path app-state (fn [s newval] (assoc s :current-path newval)))
        _             (swap! app-state assoc :function-data function-data)]
        
    (fn [] 
      (let [{:keys [table-node chart-node tree-node db]} @app-state
            mitems   (:db @menu-items) ;;this changes...
            menu     (db->menu  mitems)
            the-path (reduce  (fn [acc [k _]]
                                (if-let [v (get @selection k)]
                                  (conj acc [k (as-val v)])
                                  (reduced nil))) []  menu)
            _        (when the-path (swap! app-state assoc :current-path (mapv second the-path)))]           
            
        [above
         [:h2 "Welcome to the TADMUDI Data Exploration Extravaganza"]
         [notes]
         [:p "Once you select a database, interactive widgets will pop up, as well as charts!"]
         [tad-selector menu-items]
         [beside
          [above 
           ;;on ice for now.
           [:div {:id "Map Selector"}
            ;;map-selector returns a function, initialized off mitems, that stores a dynamic path in
            ;;map-path.  That is then applied to the mitems going forward.  Probably a better idea to
            ;;just pass in the atom though....
            [(->map-selector map-path :labels tad/path-labels #_["SRC" "Scenario"  "Measure" "[AC RC]"]) mitems]]
            
           ;;given a path, we'll let the last segment be reactive....
           [:div {:id "Coordinates"}
            ;;Allow the user to dynamically vary the x/y coordinates to recompute Z.
            ;;In this case, x : ac, y : rc, z : measure
            [(->range-selector :function-data function-data :f update-trends!)
             (current-supply)]]
           [:div {:id "sample"}
            ;;Note: we had to wrap a former call to (nearest-trends) in an atom, nt, and pass
            ;;that as the argument for trends->txt.  Otherwise, we ended up with looping re-rendering.
            [trends->txt nt]]]
          ;;our surface is here.
          [heat/vega-root]]
         [:div {:id "button"}
          [button :label "render-charts!" :on-click (fn [_] (draw-charts!))]]]
         
          ;;we'll put our reactive bar-chart here...
          ;;Figure out how to change the data for the bar chart dynamically.
          ;;Optionally re-render the whole thing.
        #_[:div {:id "tad-bar"}
           [high/chart-component]]
             
          #_[:div {:id "selection"}
             [selection-list  [{:id "a" :label "a"}
                               {:id "b" :label "b"}
                               {:id "c" :label "c"}] :field "a|b|c|"]]
          ;
          ;;where we'll store our gannt chart input and other stuff
          #_[gantt-selector]
          ;;look into using an h-box alternately.
          #_[:table #_{:align  "left"}
             [:tbody
              [:tr   #_{:valign "top"}
               [:td
                [:div {:id "the-table" :style {:width "700px" :height "300px"}}
                 table-node]]         
               [:td
                [:div {:id "the-chart" :style {:align "center" :width "1400px" :height "300px"}}
                 chart-node]]]]]
          #_[:div {:id "bmi"}
             [bmi/bmi-component]]
          #_[:div {:id "gyp"}           
             [gyp/root]]))))
         
          

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
    (def chart (google.visualization.Gantt. el))))
    


