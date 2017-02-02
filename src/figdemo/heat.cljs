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

(defn indices-by [k xs] 
  (into {} (comp (map k) (distinct)  (map-indexed (fn [idx v] [v idx])) )  xs))

;;Rather than have vega do all this, we'll just handle it ourselves man...
;;pre-group the data...
(defn row-col-group [row-field col-field xs]
  (let [rows (indices-by row-field xs)
        cols (indices-by col-field xs)
        ]
    (->> xs 
         (map (fn [r]
                (let [rw (row-field r)
                      cl (col-field r)]
                (assoc r :row (rows rw)
                         :col   (cols cl)
                         :group (str rw "-" cl)
                         )))))))
    
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
    {:width  200
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
                 {:x      {:scale "x", :field xfield},
                  :width  {:scale "x", :band  true },
                  :y      {:scale "y", :field yfield},
                  :height {:scale "y", :band  true},
                  :fill   {:scale "z", :field zfield}}}}
               
               {:type "rect",
                :from {:data :cursor} 
                :properties
                {:enter
                 {:x      {:scale "x", :field xfield},
                  :width  {:scale "x",  :band true},
                  :y      {:scale "y", :field yfield},
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

(defn random-data [xs]
  (let [ds ["D1" "D2" "D3"]
        ps ["P1" "P2"]]
    (apply concat 
           (for [d ds
                 p ps]
             (map #(assoc % :Demand d
                          :Policy p) xs)))))
         
       
(defn data->heatfacets! [xs xfield yfield zfield rowfield colfield & {:keys [xtitle ytitle ztitle]
                                                                      :or   {xtitle "X"
                                                                             ytitle "Y"}}]
  (let [from      "table"
        stackname "thestack"
        xs    (row-col-group rowfield colfield xs) ;;append :row, :col, :group fields.
        g1    (:group (first xs))
        xs-ys (->> xs
                   (filter #(=     (get % :group #_rowfield) g1))
                   (map     (juxt #(get % xfield) #(get % yfield))))
        xvals (limit-values 10 (distinct (map first xs-ys)))
        yvals (limit-values 10 (distinct (map second xs-ys)))
        cursor [{xfield  (nth xvals 4)
                 yfield  (nth yvals 4)}]
        groupfield :group
         ]
    {:width  200
     :height 500
     :padding "auto" ;"strict"
     :data
     [{:name   from
       :values xs}
      {:name  :cursor
       :values cursor}]
     :scales
     [
      {:name "z",
       :type "linear",
       :domain {:data from :field zfield};[0 #_0.25 #_0.5 #_0.75 1],
       :range ["#a50026" #_"#ffcc66" #_"#ffffbf" "#66ff33"],
       :zero false}
     
      {:name "groupy",
       :type "ordinal",
       :range "height",
       :padding 0.1,
       :domain
       {:data from,
        :field "row"
        :reverse true}}
      {:name "groupx",
       :type "ordinal",
       :range "width"
       :padding 0.1,
       :domain {:data from
                :field "col"}}]
     ,
     :marks
     [{:name stackname
       :type "group"
       :from {:data from, :transform [{:type "facet", :groupby [groupfield]}]}
       :properties ;;set up where to plot the marks for each group..
         {:enter
          {:x      {:scale "groupx", :field "col"}, ;;all charts are stacked on the same x-coordinate, {:scale "groupy", :field "key"} makes them diagonal
           :y      {:scale "groupy", :field "row"}, ;;gives us ordinal coords by group-key [0..n]
           :height {:scale "groupy", :band true :offset -20},  ;;use height offset to spread out the groups.
           :width  {:field {:group "width"}},
                   #_{:scale "groupx", :band true #_:offset #_-20}
           :stroke {:value "#ccc"}}}
       :legends  [{:fill "z" :values [0.0  0.5  1.0] :orient "right"
                   :title (or ztitle zfield)}]
       :scales   [{:name "x"
                   :type  "ordinal"
                   :range "width"
                   :domain {:data from, :field xfield}
                   }
                  {:name "y",
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
      #_{:type "text",
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
  

#_(draw! :surface-chart
    (data->heatfacets! (figdemo.core/sample-surface) :AC :RC :Response :Period :xtitle "AC" :ytitle "RC" :ztitle m))          

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
     :range "category20b" #_"category10"}],
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

;;we need to add multiple charts here.
(defonce app-state (r/atom {:input (with-out-str (pprint/pprint initial-spec))
                            #_:charts #_{}}))

(defn vega-chart [{:keys [name chart cursor]}]
  (let [vw (keyword (str name "-view"))]
    (r/create-class
     {:display-name (str name)
      :reagent-render (fn [] [:div])
      :component-did-mount
      (fn [this]
        (let [view (chart {:el (r/dom-node this)})
              _    (swap! app-state assoc vw view)]
          (.update view)))
      #_:component-did-update
      #_(fn [this]
          (when-let [view (get @app-state vw)]
            (.update view)))
      #_:component-will-update
      #_(fn [this]
          (let [view (chart {:el (r/dom-node this)})
                _    (swap! app-state assoc :view view)]
            (.update view)))})))

(defn parse-input []
  (let [{:keys [input]} @app-state]
    (swap! app-state assoc :chart nil :error nil)
    (-> (reader/read-string input)
        (vega-tools/validate-and-parse)
        (p/catch #(swap! app-state assoc :error % :view nil))
        (p/then #(swap! app-state assoc :chart %)))))

(defn draw!
  ([k s] 
   (swap! app-state assoc k nil :error nil)
   (-> s
       (vega-tools/validate-and-parse)
       (p/catch #(swap! app-state assoc :error %))
       (p/then #(swap! app-state assoc k %))))
  ([s] (draw! :chart s)))

;;we can predicate this to look for a specific chart.

#_(defn vega-root []
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

;;instead of :chart, looks for :bars in our app-state.
(defn bars-root    []
   (let [_ (js/console.log "Starting the bars-root")]
    (fn [] 
      (let [{:keys [error bar-chart cursor]} @app-state]
        [:div
         (cond
           error [:div
                  [:h2 "Bars Validation error"]
                  [:pre (with-out-str (pprint/pprint error))]]
           bar-chart [vega-chart {:name "bar-chart" :chart bar-chart :cursor cursor}]
           :else "Processing...Bars")]))))

;;instead of :chart, looks for :surface in our app-state.
(defn surface-root []
  (let [_ (js/console.log "Starting the surface-root")]
    (fn [] 
      (let [{:keys [error surface-chart cursor]} @app-state]
        [:div
         (cond
           error [:div
                  [:h2 "Surface Validation error"]
                  [:pre (with-out-str (pprint/pprint error))]]
           surface-chart [vega-chart {:name "surface-chart" :chart surface-chart :cursor cursor}]
           :else "Processing...Bars")]))))

;;where all of our charts live.
(defn vega-root []
  (let [_ (js/console.log "Starting the vega-root")]
    (fn [] 
      [:div
       [bars-root]
       [surface-root]]       
      )))

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



(def bar-data
    [{:category "A", :position 0, :value 0.1},
     {:category "A", :position 1, :value 0.6},
     {:category "A", :position 2, :value 0.9},
     {:category "A", :position 3, :value 0.4},
     {:category "B", :position 0, :value 0.7},
     {:category "B", :position 1, :value 0.2},
     {:category "B", :position 2, :value 1.1},
     {:category "B", :position 3, :value 0.8},
     {:category "C", :position 0, :value 0.6},
     {:category "C", :position 1, :value 0.1},
     {:category "C", :position 2, :value 0.2},
     {:category "C", :position 3, :value 0.7}])

(def bar-spec 
  {:width 300,
   :height 240,
   :data [{:name "table",
           :values bar-data}]
   :scales [{:name  "cat",
             :type  "ordinal",
             :domain  {:data "table", :field "category"},
             :range  "height",
             :padding 0.2
             },
            {:name  "val",
             :type  "linear",
             :domain  {:data "table", :field "value"},
             :range  "width",
             :round true,
             :nice true
             },
            {:name  "color",
             :type  "ordinal",
             :domain  {:data "table", :field "position"},
             :range  "category10"}]
  :axes [{:type  "y", :scale "cat", :tickSize 0, :tickPadding 8},
         {:type  "x", :scale "val"}],
  :marks [{:type  "group",
           :from {:data "table",
                  :transform [{:type "facet", :groupby ["category"]}]},
           :properties  {:enter  {:y {:scale "cat", :field "key"},
                                  :height {:scale "cat", :band true}}},
           :scales [{:name  "pos",
                     :type  "ordinal",
                     :range  "height",
                     :domain  {:field "position"}
                     }],
           :marks [{:name  "bars",
                    :type  "rect",
                    :properties
                    {:enter  {:y      {:scale "pos", :field "position"},
                              :height {:scale "pos", :band true},
                              :x      {:scale "val", :field "value"},
                              :x2     {:scale "val", :value 0},
                              :fill   {:scale "color", :field "position"}}}},
                   {:type  "text",
                    :from {:mark "bars"},
                    :properties  {:enter  {:x {:field  "x2", :offset -5},
                                           :y {:field  "y"},
                                           :dy {:field  "height", :mult 0.5},
                                           :fill {:value  "white"},
                                           :align {:value  "right"},
                                           :baseline {:value  "middle"},
                                           :text {:field "datum.value"}}}}]
           }
          ]})


(defn grouped-bars [xs {:keys [valfield trendfield catfield
                            xtitle ytitle ]
                     :or {valfield   "value"
                          catfield   "category"
                          trendfield "position"
                          xtitle     valfield
                          ytitle     "Categories"}}]
  (let [from "table"]
    {:width 200
     :height 200
     :data [{:name from
             :values xs}]
     :scales [{:name  "cat"
               :type  "ordinal"
               :domain  {:data from :field catfield}
               :range  "height"
               :padding 0.2}
              {:name  "val"
               :type  "linear"
               :domain  {:data from :field valfield}
               :domainMax 1.0
               :range  "width"
               :round true
               :nice true}
              {:name  "color"
               :type  "ordinal"
               :domain  {:data from :field trendfield}
               :range  "category10"}]
     :axes [{:type  "y" :scale "cat" :tickSize 0 :tickPadding 8 :title ytitle}
            {:type  "x" :scale "val" :title xtitle}]
     :legends [{:fill "color" :title trendfield}]
     :marks [{:type  "group"
              :from {:data from
                     :transform [{:type "facet" :groupby [catfield]}]}
              :properties  {:enter  {:y {:scale "cat" :field "key"}
                                     :height {:scale "cat" :band true}}}
              :scales [{:name  "pos"
                        :type  "ordinal"
                        :range  "height"
                        :domain  {:field trendfield}
                        }]
              :marks [{:name  "bars"
                       :type  "rect"
                       :properties
                       {:enter  {:y      {:scale "pos" :field trendfield}
                                 :height {:scale "pos" :band true}
                                 :x      {:scale "val" :field valfield}
                                 :x2     {:scale "val" :value 0}
                                 :fill   {:scale "color" :field trendfield}}
                        :update  {:y     {:scale "pos" :field trendfield}
                                ;  :height {:scale "pos" :band true}
                                  :x     {:scale "val" :field valfield}
                                  :x2    {:scale "val" :value 0}
                                  }}}
                      {:type  "text"
                       :from {:mark "bars"}
                       :properties  {:enter  {:x {:field  "x2" :offset -5}
                                              :y {:field  "y"}
                                              :dy {:field  "height" :mult 0.5}
                                              :fill {:value  "white"}
                                              :align {:value  "right"}
                                              :baseline {:value  "middle"}
                                              :text {:field (datum valfield)}}
                                     :update  {:x {:field  "x2" :offset -5}
                                               :y {:field  "y"}
                                               :text {:field (datum valfield)}}}}]
              }
             ]}))


;;aborted attempt at vertical bars; needs work.
(defn grouped-barsv [xs {:keys [valfield trendfield catfield
                            xtitle ytitle ]
                     :or {valfield   "value"
                          catfield   "category"
                          trendfield "position"
                          xtitle     valfield
                          ytitle     "Categories"}}]
  (let [from "table"]
    {:width 300
     :height 300
     :data [{:name from
             :values xs}]
     :scales [{:name  "cat"
               :type  "ordinal"
               :domain  {:data from :field catfield}
               :range  "width"
               :padding 0.2}
              {:name  "val"
               :type  "linear"
               :domain  {:data from :field valfield}
               :domainMax 1.0
               :range  "height"
               :round true
               :nice true}
              {:name  "color"
               :type  "ordinal"
               :domain  {:data from :field trendfield}
               :range  "category10"}]
     :axes [{:type "x"  :scale "cat"  :tickSize 0  :tickPadding 8 :title xtitle}
            {:type "y"  :scale "val"  :title ytitle}]
     :legends [{:fill "color" :title trendfield}]
     :marks [{:type  "group"
              :from {:data from
                     :transform [{:type "facet" :groupby [catfield]}]}
              :properties  {:enter  {:x {:scale "cat" :field "key"}
                                     :height {:scale "cat" :band true}}}
              :scales [{:name  "pos"
                        :type  "ordinal"
                        :range  "width"
                        :domain  {:field trendfield}
                        }]
              :marks [{:name  "bars"
                       :type  "rect"
                       :properties
                       {:enter  {:x      {:scale "pos" :field trendfield}
                                 :width {:scale "pos" :band true}
                                 :y      {:scale "val" :field valfield}
                                 :y2     {:scale "val" :value 0}
                                 :fill   {:scale "color" :field trendfield}}
                        :update  {:x     {:scale "pos" :field trendfield}
                                ;  :height {:scale "pos" :band true}
                                  :y     {:scale "val" :field valfield}
                                  :y2    {:scale "val" :value 0}
                                  }}}
                      {:type  "text"
                       :from {:mark "bars"}
                       :properties  {:enter  {:y {:field  "y2" :offset -5}
                                              :x {:field  "x"}
                                              :dx {:field  "width" :mult 0.5}
                                              :fill {:value  "white"}
                                              :align {:value  "right"}
                                              :baseline {:value  "middle"}
                                              :text {:field (datum valfield)}}
                                     :update  {:y {:field  "y2" :offset -5}
                                               :x {:field  "x"}
                                               :text {:field (datum valfield)}}}}]
              }
             ]}))



(def testd
  [{:Period "PreSurge",  :Response 0.542988619,  :SRC "770200R00", :demand "SteadyState", :policy "Rotational", :measure "Fill"}
   {:Period "Surge",     :Response 0.894675476,     :SRC "770200R00", :demand "SteadyState", :policy "Rotational", :measure "Fill"}
   {:Period "PostSurge", :Response 0.31573259,  :SRC "770200R00", :demand "SteadyState", :policy "Rotational", :measure "Fill"}
   {:Period "PreSurge",  :Response 0.431484142,  :SRC "770200R00", :demand "SteadyState", :policy "MaxUtilization", :measure "Fill"}
   {:Period "Surge",     :Response 0.723484714,     :SRC "770200R00", :demand "SteadyState", :policy "MaxUtilization", :measure "Fill"}
   {:Period "PostSurge", :Response 0.717026122, :SRC "770200R00", :demand "SteadyState", :policy "MaxUtilization", :measure "Fill"}])

;; (defn add-trend [xs]
;;   (mapv (fn [x] 

(def testd2
  [{:Period "PreSurge", :Response 0.542988619, :SRC "770200R00", :demand "SteadyState", :policy "Rotational", :measure "Fill"}
   {:Period "Surge", :Response 0.894675476, :SRC "770200R00", :demand "SteadyState", :policy "Rotational", :measure "Fill"}
   {:Period "PostSurge", :Response 0.31573259, :SRC "770200R00", :demand "SteadyState", :policy "Rotational", :measure "Fill"}
   {:Period "PreSurge", :Response 0.431484142, :SRC "770200R00", :demand "SteadyState", :policy "MaxUtilization", :measure "Fill"}
   {:Period "Surge", :Response 0.723484714, :SRC "770200R00", :demand "SteadyState", :policy "MaxUtilization", :measure "Fill"}
   {:Period "PostSurge", :Response 0.717026122, :SRC "770200R00", :demand "SteadyState", :policy "MaxUtilization", :measure "Fill"}
   {:Period "PreSurge", :Response 0.641639285, :SRC "770200R00", :demand "SS+Surge", :policy "Rotational", :measure "Fill"}
   {:Period "Surge", :Response 0.709456869, :SRC "770200R00", :demand "SS+Surge", :policy "Rotational", :measure "Fill"}
   {:Period "PostSurge", :Response 0.810916048, :SRC "770200R00", :demand "SS+Surge", :policy "Rotational", :measure "Fill"}
   {:Period "PreSurge", :Response 0.957844978, :SRC "770200R00", :demand "SS+Surge", :policy "MaxUtilization", :measure "Fill"}
   {:Period "Surge", :Response 0.689798911, :SRC "770200R00", :demand "SS+Surge", :policy "MaxUtilization", :measure "Fill"}
   {:Period "PostSurge", :Response 0.087409272, :SRC "770200R00", :demand "SS+Surge", :policy "MaxUtilization", :measure "Fill"}])

(defn acrc-bars! [xs]
  (-> (->> xs
           (mapv (fn [r] (assoc r :trend (str (:SRC r) "-" (:demand r))))))      
      (grouped-bars  {:valfield   :Response
                      :trendfield :trend #_:policy
                      :catfield   :Period
                      :xtitle (:measure (first xs))
                      :ytitle "Period"})))
