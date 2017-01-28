;;wrapping vega a bit.
(ns figdemo.heat
  (:require [promesa.core :as p] ;;ugh
            [cljs.reader :as reader]
            [clojure.pprint :as pprint]
            [reagent.core :as r]
            [vega-tools.core :as vega-tools]))

(defn limit-values [n xs]
  (let [v (vec xs)
        k (count v)]
    (if (<= k n) v
        (let [step (quot k n)]
          (map #(nth v %)
               (range 0 k step))))))
;;helper.
(defn datum [p]
  (str "datum." (name p)))

;;derived from  <https://github.com/vega/vega/blob/76ab79f711b80840e34484974c9b717f584e8f7c/examples/bar.json>

;;works if we have a normalized set of [x y z]
;;coordinates.
(defn data->heatspec! [xs]
    {:width 400,
     :height 400,
     :padding "strict",
     :data
     [{:name  "table",
       :values xs}],
     :scales
     [{:name "x",
       :type  "ordinal",
       :range "width",
       :domain {:data "table", :field "x"}
                                        ;:points true
       }
      {:name "y",
       :type   "ordinal",
       :range  "height",
       :reverse true ;else we get upside down...     
       :domain {:data "table", :field "y"},
                                        ;:points true
                                        ;:nice true
       }
      {:name "z",
       :type "linear",
       :domain [0 #_0.25 #_0.5 #_0.75 1],
       :range ["#a50026" #_"#ffcc66" #_"#ffffbf" "#66ff33"],
       :zero false}],
     :axes [{:type  "x",
             :scale "x"
             :title "X-axis"
              }
            {:type "y",
             :scale "y"
             :title "Y-axis"
             }],
     :legends [{:fill "z" :values [0.0  0.5  1.0] :orient "right"}]
     :marks
     [{:type "rect",
       :from {:data "table"},
       :properties
       {:enter
        {:x {:scale "x", :field "x"},
         :width {:scale "x", :band true },
         :y {:scale "y", :field "y"},
         :height {:scale "y", :band true},
         :fill {:scale "z", :field "z"}}}}]})


(defn set-cursor
  ([v fx fy x y]
   (let [d (.data v)
         c (aget d "cursor")
         ]
     (do (aset c  0 (clj->js {fx x fy y}))         
         (.update v)))))

(defn get-cursor [v]
  (aget (.data v) "cursor"))

;;Assuming the plots are independent,
;;we could elevate them..
;;basically, pull the plot data out as named group data?

(defn data->heatfacet! [xs xfield yfield zfield rowfield & {:keys [xtitle ytitle ztitle]
                                                            :or   {xtitle "X"
                                                                   ytitle "Y"}}]
  (let [from      "table"
        stackname "thestack"
        g1    (get (first xs) rowfield)
        xs-ys (->> xs
                   (filter #(=     (get % rowfield) g1))
                   (map     (juxt #(get % xfield) #(get % yfield))))
        xvals (limit-values 10 (distinct (map first xs-ys)))
        yvals (limit-values 10 (distinct (map second xs-ys)))
        cursor [{xfield  (nth xvals 4)
                 yfield  (nth yvals 4)}]                
         ]
    {:width  500
     :height 500
     :padding "auto" ;"strict"
     :data
     [{:name   from
       :values xs}
      {:name  :cursor
       :values cursor}]
     :scales
     [{:name "x"
       :type  "ordinal"
       :range "width"
       :domain {:data from, :field xfield}
       }
      {:name "z",
       :type "linear",
       :domain {:data from :field zfield};[0 #_0.25 #_0.5 #_0.75 1],
       :range ["#a50026" #_"#ffcc66" #_"#ffffbf" "#66ff33"],
       :zero false}
      
      {:name "group",
       :type "ordinal",
       :range "height",
       :padding 0.1,
       :domain
       {:data from,
        :field rowfield,
        #_:sort #_{:field valfield, :op "median"}},
       :reverse true}]
     ,
     :marks
     [{:name stackname
       :type "group"
       :from {:data from, :transform [{:type "facet", :groupby [rowfield]}]}
       :properties ;;set up where to plot the marks for each group..
         {:enter
          {:x      {:value 0.5}, ;;all charts are stacked on the same x-coordinate, {:scale "group", :field "key"} makes them diagonal
           :y      {:scale "group", :field "key"}, ;;gives us ordinal coords by group-key [0..n]
           :height {:scale "group", :band true :offset -20},  ;;use height offset to spread out the groups.
           :width  {:field {:group "width"}},
           :stroke {:value "#ccc"}}}
       :legends  [{:fill "z" :values [0.0  0.5  1.0] :orient "right"
                   :title (or ztitle zfield)}]
        :scales
       [{:name "y",
         :type "ordinal",
         :range "height",
         ;:points true,
         ;:padding 1.2,
         :domain
         {:data from,
          :field yfield,
          },
         :reverse true}],
       :axes [{:type  "x",
               :scale "x"
               :title xtitle
               :values xvals
               }
              {:type  "y",
               :scale "y"
               :title ytitle
               :values yvals
               }]       
       :marks [{:type "rect",
                :properties
                {:enter
                 {:x {:scale "x", :field xfield},
                  :width {:scale "x", :band true },
                  :y {:scale "y", :field yfield},
                  :height {:scale "y", :band true},
                  :fill   {:scale "z", :field zfield}}}}
               
               {:type "rect",
                :from {:data :cursor} 
                :properties
                {:enter
                 {:x {:scale "x", :field xfield},
                  :width {:scale "x",  :band true},
                  :y {:scale "y", :field yfield},
                  :height {:scale "y", :band true},
                  :stroke {:value "black"}}
                 :update
                 {:x {:scale "x", :field xfield}                 
                  :y {:scale "y", :field yfield}
                  :fillOpacity {:value 0}}
                 :hover
                 {:cursor {:value :pointer}
                  :fill   {:value "black"}
                  :fillOpacity {:value 1.0}}
                  
                 
                   }}]
       }
      ;;labels
      {:type "text",
       :from {:mark stackname},
       :properties
       ;;place a mark at 1/2 the sub-group's width...
       {:enter
        {:x {:field {:group "width"}, :mult 0.9},
         :y {:field "y", :offset 2}, ;;2 pts above the plot  ;;note had to use "y" literal here.
         :fontWeight {:value "bold"},
         :fontSize {:value 14}
         :text  {:field (datum rowfield)},
         :align {:value "center"},
         :baseline {:value "bottom"},
         :fill {:value "#000"}}}}
      ]}))
;(defn layers->heat-specs [groups]
  

(defn re-key [m]
  (cond (map? m) (into {} (map (fn [[k v]]
                                 [(if (string? k) (keyword k) k)
                                  (re-key v)]))
                       (seq m))
        (seqable? m) (into (empty m) (map (fn [x] (re-key x))) m)
        :else m))

(defn json-generate 
   "Returns a newline-terminate JSON string from the given ClojureScript data." 
   [data] 
   (str (.stringify js/JSON (clj->js data)) "\n")) 
  
(defn json-parse 
  "Returns ClojureScript data for the given JSON string." 
  [line & {:keys [keywordize?] :or {keywordize? true}}]
  (let [x (js->clj (.parse js/JSON line))]
    (if keywordize? (re-key x) x)))                           

;;so, vega is pretty badass.
;;The deal is, once you know how to transform data..
;;ala faceting, things are easier..
;;

;;so, rather than manually  grouping EVERYTHINg...
;;we don't we do this...

;;oooookayy...lets make some helpers because this shit is wack.
;;this produces a valid "marks" spec fyi..
;;Say we do our facets.....we want to lay out the data in a 2x4
;;matrix.
;;We just need to project from the key range onto the desired row/col
;;say we have 8 groups.
;;We'll render 0 1 2 3, 4,5,6,7
;;so, coords would be..
;;0,0 1,0 2,0 3,0
;;

(defn ->xy-facet [& {:keys [name from xfield yfield rowfield trendfield]}]
  {:name name,
   :type "group",
   :from {:data from, :transform [{:type "facet", :groupby [rowfield]}]},
   :scales
   [{:name "y",
     :type "ordinal",
     :range "height",
     :points true,
     :padding 1.2,
     :domain
     {:data from,
      :field yfield,
      :sort {:field xfield, :op "median"}},
     :reverse true}],
   :axes
   [{:type  "y",
     :scale "y",
     ;:tickSize 0,
     ;:properties {:axis {:stroke {:value "transparent"}}}
     }],
   :properties ;;set up where to plot the marks for each group..
   {:enter
    {:x      {:value 0.5}, ;;all charts are stacked on the same x-coordinate, {:scale "group", :field "key"} makes them diagonal
     :y      {:scale "group", :field "key"}, ;;gives us ordinal coords by group-key [0..n]
     :height {:scale "group", :band true}, 
     :width  {:field {:group "width"}},
     :stroke {:value "#ccc"}}},
   :marks
   [{:type "symbol",
     :properties
     {:enter
        {:x {:scale "x", :field xfield},
         :y {:scale "y", :field yfield},
         :size {:value 50},
         :stroke {:scale "color", :field trendfield},
         :strokeWidth {:value 2},
         :fill {:value "transparent"}}}}]})

;;presumes a pre-existing scale for "x".
;;Note: we don't have to do this, since we don't care about a
;;shared axis, we can pass in the x and y scales/axes for each chart
;;as in the heat-map spec..
;; (defn ->xy-heat-facet [& {:keys [name from xfield yfield rowfield trendfield]}]
;;   {:name   name
;;    :type   "group"
;;    :from  {:data      from
;;            :transform [{:type    "facet"
;;                         :groupby [rowfield]}]}
;;    :scales
;;    [{:name    "y"
;;      :type    "ordinal"
;;      :range   "height" ;;plot across the group's height
;;      :points  false    ;;don't think we use points here.
;;      :padding 1.2
;;      :domain  {:data  from
;;                :field yfield
;;                #_:sort  #_{:field xfield :op "median"}}
;;      :reverse true}]
;;    :axes
;;    [{:type  "y"
;;      :scale "y"
;;      ;:tickSize 0,
;;      ;:properties {:axis {:stroke {:value "transparent"}}}
;;      }]
;;    :properties
;;    {:enter
;;     {:x      {:value 0.5}
;;      :y      {:scale "group", :field "key"}
;;      :height {:scale "group", :band  true}
;;      :width  {:field {:group "width"}}
;;      :stroke {:value "#ccc"}}}
;;     :marks
;;    [{:type "rect"
;;      :from {:data from}
;;      :properties
;;      {:enter
;;       {:x      {:scale "x" :field xfield}
;;        :width  {:scale "x" :band  true}
;;        :y      {:scale "y" :field yfield}
;;        :height {:scale "y" :band  true}
;;        :fill   {:scale "z" :field trendfield}}}}]})


(defn ->facet-scales [& {:keys [from group x color]}]
  (let [[from rowfield valfield trendfield]
        [from group x color]]
    [{:name "group",
      :type "ordinal",
      :range "height",
      :padding 0.15,
      :domain
      {:data from,
       :field rowfield,
       :sort {:field valfield, :op "median"}},
      :reverse true}
     {:name "x",
      :type "linear",
      :nice true,
      :range "width",
      :domain {:data from, :field valfield}}
     ;;series...
     {:name "color",
      :type "ordinal",
      :range "category10",
      :domain {:data from, :field trendfield}}]))

;;note: x and y will be the same for all surfaces in a
;;heat-trellis...so.
;; (defn ->facet-heat-scales [& {:keys [from group x y z color]}]
;;   (let [[from rowfield valfield trendfield]
;;         [from group x color]]
;;     [{:name "group",
;;       :type "ordinal",
;;       :range "height",
;;       :padding 0.15,
;;       :domain
;;       {:data from,
;;        :field rowfield}
;; ;       :sort {:field valfield, :op "median"}},
;;       :reverse true}
;;      {:name "x",
;;       :type "ordinal",
;;       :nice true,
;;       :range "width",
;;       :domain {:data from, :field valfield}}
;;      ;;series...
;;      {:name "color",
;;       :type "ordinal",
;;       :range "category10",
;;       :domain {:data from, :field trendfield}}])))



(def test-data
  (vec
   (for [year [2009 2010 2011]
         site    ["a" "b" "c"]
         variety ["frankencense" "mir" "gold"]
         ]
     {"year"   year
      "site"    site     
      "variety" variety
      "yield"   (rand-int 100)})))

(def test-heat
  (let [xmax 40
        ymax 40]
    (vec (for [ac     (range 5 xmax)
               rc     (range 10 ymax)
               period ["PreSurge" "Surge" "PostSurge"]]
         {:AC ac
          :RC rc
          :Period period
          :Fill (case period
                  "PreSurge" (rand)
                  "Surge"    (/ (+ ac rc) (+ xmax ymax))
                  (min (/ (+ (* ac 3.0) rc) (+ xmax ymax)) 1.0))}))))
             

;;now lets draw heatmaps!
;;x and y will be the same,
;;ac and rc...
;;so we should be able to stack our heatmaps
;;only difference is
;;ordinal scales.

;;this is just a big compression drill to try to make an
;;easy template for building scatter charts.
(defn stacked! [the-data rowfield xfield yfield trendfield]
  (let [data (if (map? the-data) the-data
                 {:name "some-data"
                  :values the-data})
        from       (:name data)
        stackname  (str rowfield "s")
        ]
    {:width  200,
     :height 720,
     :data   [data]
     :scales (->facet-scales :from  from
                             :group rowfield
                             :x     xfield
                             :color trendfield)
     :axes    [{:type "x", :scale "x"}],
     :legends [{:fill "color", :title trendfield}],
     :marks
     [(->xy-facet :name stackname :from from
                  :xfield xfield :yfield yfield
                  :rowfield rowfield
                  :trendfield trendfield)
      ;;site-names, this is "technically" a function of the preceding marks...
      {:type "text",
       :from {:mark stackname},
       :properties
       ;;place a mark at 1/2 the sub-group's width...
       {:enter
        {:x {:field {:group "width"}, :mult 0.5},
         :y {:field "y", :offset -2}, ;;2 pts above the plot
         :fontWeight {:value "bold"},
         :text {:field (datum rowfield)},
         :align {:value "center"},
         :baseline {:value "bottom"},
         :fill {:value "#000"}}}}]} ))

;;identical to stacked, but provides a unique xaxis for each
;;facet.
(defn stacked-multiple-x! [the-data rowfield xfield yfield trendfield]
  (let [data (if (map? the-data) the-data
                 {:name "some-data"
                  :values the-data})
        from       (:name data)
        stackname  (str rowfield "s")
        ]
    {:width  200,
     :height 720,
     :data   [data]
     :scales (->facet-scales :from  from
                             :group rowfield
                             :x     xfield
                             :color trendfield)
     :legends [{:fill "color", :title trendfield}],
     :marks
     [(-> (->xy-facet :name stackname :from from
                      :xfield xfield :yfield yfield
                      :rowfield rowfield
                      :trendfield trendfield)
          (update   :axes   (fn [x] (conj (or x [])
                               {:type "x", :scale "x"}))))
      ;;site-names, this is "technically" a function of the preceding marks...
      {:type "text",
       :from {:mark stackname},
       :properties
       ;;place a mark at 1/2 the sub-group's width...
       {:enter
        {:x {:field {:group "width"}, :mult 0.5},
         :y {:field "y", :offset -2}, ;;2 pts above the plot
         :fontWeight {:value "bold"},
         :text {:field (datum rowfield)},
         :align {:value "center"},
         :baseline {:value "bottom"},
         :fill {:value "#000"}}}}]}))

(defn stack-test! []
  (stacked! {:name "barley"
             :url "http://idl.cs.washington.edu/projects/vega/examples/data/barley.json"}
            "site"
            "yield"
            "variety"
            "year"))

(defn my-stack! []
  (stacked! test-data "site" "yield" "variety" "year"))

(defn my-stack-xs! []
  (stacked-multiple-x! test-data "site" "yield" "variety" "year"))

(def stackedgroups
  {:width 200,
   :height 720,
   :data [{:name "barley", :url "http://idl.cs.washington.edu/projects/vega/examples/data/barley.json"}],
   :scales
   [{:name "group",
     :type "ordinal",
     :range "height",
     :padding 0.15,
     :domain
     {:data "barley",
      :field "site",
      :sort {:field "yield", :op "median"}},
     :reverse true}
    {:name "x",
     :type "linear",
     :nice true,
     :range "width",
     :domain {:data "barley", :field "yield"}}
    {:name "color",
     :type "ordinal",
     :range "category10",
     :domain {:data "barley", :field "year"}}],
   :axes [{:type "x", :scale "x"}],
   :legends [{:fill "color", :title "year"}],
   :marks
   [{:name "sites",
     :type "group",
     :from {:data "barley", :transform [{:type "facet", :groupby ["site"]}]},
     :scales
     [{:name "y",
       :type "ordinal",
       :range "height",
       :points true,
       :padding 1.2,
       :domain
       {:data "barley",
        :field "variety",
        :sort {:field "yield", :op "median"}},
       :reverse true}],
     :axes
     [{:type "y",
       :scale "y",
       :tickSize 0,
       :properties {:axis {:stroke {:value "transparent"}}}}],
     :properties
     {:enter
      {:x {:value 0.5},
       :y {:scale "group", :field "key"},
       :height {:scale "group", :band true},
       :width {:field {:group "width"}},
       :stroke {:value "#ccc"}}},
     :marks
     [{:type "symbol",
       :properties
       {:enter
        {:x {:scale "x", :field "yield"},
         :y {:scale "y", :field "variety"},
         :size {:value 50},
         :stroke {:scale "color", :field "year"},
         :strokeWidth {:value 2},
         :fill {:value "transparent"}}}}]}
    {:type "text",
     :from {:mark "sites"},
     :properties
     {:enter
      {:x {:field {:group "width"}, :mult 0.5},
       :y {:field "y", :offset -2},
       :fontWeight {:value "bold"},
       :text {:field "datum.site"},
       :align {:value "center"},
       :baseline {:value "bottom"},
       :fill {:value "#000"}}}}]} )


(def scatter
  {:width 600,
   :height 600,
   :data
   [{:name "iris",   :url "http://idl.cs.washington.edu/projects/vega/examples/data/iris.json"}
    ;;common ordinal scale.  note: this is also what we're crossing by.
    {:name "fields", :values ["petalWidth" "petalLength" "sepalWidth" "sepalLength"]}],
   :scales
   ;;establish common scales for the groups to use. 
   [{:name "globalx",
     :type "ordinal",
     :range "width",
     :round true,
     :domain {:data "fields", :field "data"}}
    {:name "globaly",
     :type "ordinal",
     :range "height",
     :round true,
     :reverse true,
     :domain {:data "fields", :field "data"}}
    {:name "c",
     :type "ordinal",
     :domain {:data "iris", :field "species"},
     :range "category10"}],
   :legends
   [{:fill "c",
     :title "Species",
     :offset 10,
     :properties
     {:symbols
      {:fillOpacity {:value 0.5}, :stroke {:value "transparent"}}}}],
   :marks
   [{:type "group",
     ;;data xform builds a crosstab of original iris data...
     ;;cross will produce a data object with two dummy fields,
     ;;[a b], so a.data => computed field a, etc.
     
     :from {:data "fields", :transform [{:type "cross"}]},
     ;;common or global properties....
     :properties
     ;;define how the subplots look by default.
     {:enter
      ;;each group will use the common ordinal scale for its data, but
      ;;plot the field from the grouped dataset (a) onto the scale.
      ;;and plot the 
      {:x      {:scale "globalx", :field "a.data"}, ;;"petalWidth"  ex. = 0
       :y      {:scale "globaly", :field "b.data"}, ;;"petalLength" ex. = 1
       :width  {:scale "globalx", :band true, :offset -35},
       :height {:scale "globaly", :band true, :offset -35},
       :fill   {:value "#fff"},
       :stroke {:value "#ddd"}}},
     :scales
     [{:name "x",
       :range "width",
       :zero false,
       :round true,
       :domain {:data "iris", :field {:parent "a.data"}}}
      {:name "y",
       :range "height",
       :zero false,
       :round true,
       :domain {:data "iris", :field {:parent "b.data"}}}],
     :axes
     [{:type "x", :scale "x", :ticks 5}
      {:type "y", :scale "y", :ticks 5}],
     :marks
     ;;for each subplot, plot the symbols thusly:
     ;;lookup the data in iris...  i guess we have to have a
     
     [{:type "symbol",
       :from {:data "iris"},
       :properties
       {:enter
        {:x {:scale "x", :field {:datum {:parent "a.data"}}},
         :y {:scale "y", :field {:datum {:parent "b.data"}}},
         :fill {:scale "c", :field "species"},
         :fillOpacity {:value 0.5}},
        :update {:size {:value 36}, :stroke {:value "transparent"}},
        :hover {:size {:value 100}, :stroke {:value "white"}}}}]}]})
(defn layers->heatmap [xs]
  
 )

;; #_{:type "rect", :from {:data "table"},
;;        :properties #_{:enter {:x {:scale "x", :field "x"}
;;                               :width {:scale "x", :band true, :offset -1}
;;                               :y {:scale "y", :field "y"}
;;                               :y2 {:scale "y", :value 0}}
;;                       :update {:fill {:value "steelblue"}}
;;                       :hover {:fill {:value "red"}}}}


(def initial-spec
  {:width  400
   :height 200
   :padding {:top 10, :left 30, :bottom 30, :right 10}

   :data
   [{:name "table"
     :values [{:x 1, :y 28} {:x 2, :y 55}
              {:x 3, :y 43} {:x 4, :y 91}
              {:x 5, :y 81} {:x 6, :y 53}
              {:x 7, :y 19} {:x 8, :y 87}
              {:x 9, :y 52} {:x 10, :y 48}
              {:x 11, :y 24} {:x 12, :y 49}
              {:x 13, :y 87} {:x 14, :y 66}
              {:x 15, :y 17} {:x 16, :y 27}
              {:x 17, :y 68} {:x 18, :y 16}
              {:x 19, :y 49} {:x 20, :y 15}]}]

   :scales
   [{:name "x"
     :type "ordinal"
     :range "width"
     :domain {:data "table", :field "x"}}
    {:name "y"
     :type "linear"
     :range "height"
     :domain {:data "table", :field "y"}, :nice true}]

   :axes
   [{:type "x", :scale "x"}
    {:type "y", :scale "y"}]

   :marks
   [{:type "rect", :from {:data "table"},
     :properties {:enter {:x {:scale "x", :field "x"}
                          :width {:scale "x", :band true, :offset -1}
                          :y {:scale "y", :field "y"}
                          :y2 {:scale "y", :value 0}}
                  :update {:fill {:value "steelblue"}}
                  :hover {:fill {:value "red"}}}}]})

(defonce app-state (r/atom {:input (with-out-str (pprint/pprint initial-spec))}))

(defn vega-chart [{:keys [chart cursor]}]
  (r/create-class
   {:display-name "vega-chart"
    :reagent-render (fn [] [:div])
    :component-did-mount
    (fn [this]
      (let [view (chart {:el (r/dom-node this)})
            _    (swap! app-state assoc :view view)]
        (.update view)))
    #_:component-did-update
    #_(fn [this]
     (when-let [view (get @app-state :view)]
       (.update view)))
    #_:component-will-update
    #_(fn [this]
      (let [view (chart {:el (r/dom-node this)})
            _    (swap! app-state assoc :view view)]
        (.update view)))}))

(defn parse-input []
  (let [{:keys [input]} @app-state]
    (swap! app-state assoc :chart nil :error nil)
    (-> (reader/read-string input)
        (vega-tools/validate-and-parse)
        (p/catch #(swap! app-state assoc :error % :view nil))
        (p/then #(swap! app-state assoc :chart %)))))

(defn draw! [s]
  (swap! app-state assoc :chart nil :error nil)
  (-> s
      (vega-tools/validate-and-parse)
      (p/catch #(swap! app-state assoc :error %))
      (p/then #(swap! app-state assoc :chart %))))

(defn vega-root []
  (let [_ (js/console.log "Starting the vega-root")
        _ (parse-input)]
    (fn [] 
      (let [{:keys [input error chart cursor]} @app-state]
        [:div
         ;; [:h1 "vega-tools example"]
         ;; [:div.container-fluid
         ;;  [:div.editor.col-md-6
         ;;   [:button {:on-click #(parse-input)} "Parse"] [:br]
         ;;   [:textarea.spec-input
         ;;    {:default-value input
         ;;     :on-change #(swap! app-state assoc :input (-> % .-target .-value))}]]]
          ;[:div
           (cond
             error [:div
                    [:h2 "Validation error"]
                    [:pre (with-out-str (pprint/pprint error))]]
             chart [vega-chart {:chart chart :cursor cursor}]
             :else "Processing...")]
                                        ;]
      ))))


;;we can get an equivalent table of data...
;;vega tables look like this...
(def d  {"name" "table",
         "values" [ {"x" 1,  "y" 28}, {"x" 2,  "y" 55},
                    {"x" 3,  "y" 43}, {"x" 4,  "y" 91},
                    {"x" 5,  "y" 81}, {"x" 6,  "y" 53},
                    {"x" 7,  "y" 19}, {"x" 8,  "y" 87},
                    {"x" 9,  "y" 52}, {"x" 10, "y" 48},
                    {"x" 11, "y" 24}, {"x" 12, "y" 49},
                    {"x" 13, "y" 87}, {"x" 14, "y" 66},
                    {"x" 15, "y" 17}, {"x" 16, "y" 27},
                    {"x" 17, "y" 68}, {"x" 18, "y" 16},
                    {"x" 19, "y" 49}, {"x" 20, "y" 15}
                   ]})

(comment 
(def chrt (atom nil))

(defn ->mounted-chart [opts]
  (fn [this]
    (let [c  (js/Highcharts.Chart. (reagent/dom-node this) (clj->js opts))
          _  (reset! chrt c)]
      c)))

;;takes a specification for the chart, and creates and mounts our chart from it.
(defn chart-component [& {:keys [spec chartref] :or {spec demo-config}}]
  (let [c  (->mounted-chart spec)
        _  (when chartref (reset! chartref c))]
    (reagent/create-class
     {:reagent-render chart-render
      :component-did-mount c})))
)
