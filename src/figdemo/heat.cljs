;;wrapping vega a bit.
(ns figdemo.heat
  (:require [promesa.core :as p] ;;ugh
            [cljs.reader :as reader]
            [clojure.pprint :as pprint]
            [reagent.core :as r]
            [vega-tools.core :as vega-tools]))

;;normalized linear heat gradient...
(defn linear-heat [w h]
  (for [x (range w)
        y (range h)]
    {:x x
     :y y
     :z (/ (+ x y) (+ w h))}))

(defn linear-heat-from [xmin xmax ymin ymax n]
  (let [dx (-  xmax xmin)
        dy (-  ymax ymin)
        mx (+ xmax ymax)]
    (for [n (range n)]
      (let [x  (+ xmin (int (* dx (rand))))
            y  (+ ymin (int (* dy (rand))))]
        {:x x
         :y y
         :z (/ (+ x y) mx)}))))
       


;;juuuuuust enough info to create a heatmap using vega.
;;For more general purpose solutions, we can use 
(def raw-heat-spec
  {"width" 800
   "height" 500
   "data" [{"name" "temperature"
            "url" "data/seattle-temps.csv"
            "format" {"type" "csv" "parse" {"temp" "number" "date" "date"}}
            "transform" [{"type" "formula" "field" "hour" "expr" "hours(datum.date)"}
                         { "type" "formula" "field" "date"
                          "expr" "datetime(year(datum.date) month(datum.date) date(datum.date))"}]}]
   "scales" [{"name" "x"
              "type" "time"
              "domain" {"data" "temperature"
                        "field" "date"}
              "range" "width"}
             {"name" "y"
              "type" "ordinal"
              "domain" {"data" "temperature"
                        "field" "hour"}
              "range" "height"
              "round" false}
             {"name" "c"
              "type" "linear"
              ;;n break points in the domain
              "domain" [37.5, 47, 56.5, 75.5]
              ;;n colors to define the gradient
              "range" ["#a50026" "#ffcc66" "#ffffbf" "#66ff33"]
              "zero" false}]
   "axes" [{"type" "x" "scale" "x"}
           {"type" "y" "scale" "y"}]
   "legends" [{"fill" "c" "values" [37.5 56.5 75.5]}]
   "marks" [{"type" "rect"
             "from" {"data" "temperature"}
             "properties" {"enter" {"x" {"scale" "x" "field" "date"}
                                    "width" {"value" 5}
                                    "y" {"scale" "y" "field" "hour"}
                                    "height" {"scale" "y" "band" true}
                                    "fill" {"scale" "c" "field" "temp"}}}}]})

;; Copied from <https://github.com/vega/vega/blob/76ab79f711b80840e34484974c9b717f584e8f7c/examples/bar.json>
(def heat-spec
  {:width  400
   :height 400
   :padding {:top 10, :left 30, :bottom 30, :right 10}

   :data
   [{:name "table"
     :values (vec (linear-heat 10 10))}]

   :scales
   [{:name "x"
     :type "ordinal"
     :range "width"
     :domain {:data "table", :field "x"}}
    {:name "y"
     :type "linear"
     :range "height"
     :domain {:data "table", :field "y"},
     :nice true
     }
    {:name "z"
      :type "linear"
     ;;n break points in the domain
      :domain [0.0, 0.25, 0.75, 1.0]
     ;;n colors to define the gradient
      :range ["#a50026" "#ffcc66" "#ffffbf" "#66ff33"]
     :zero false}]
   
   :axes
   [{:type "x", :scale "x"}
    {:type "y", :scale "y"}]

   :marks
   [{:type "rect"
     :from {:data "table"}
     :properties {:enter {:x {:scale "x" :field "x"}
                          :width {:value 5}
                          :y {:scale "y" :field "y"}
                          :height {:scale "y" :band true}
                          :fill {:scale "z" :field "z"}}}}]})


(def actual-spec
  {:width 400,
   :height 400,
   :padding {:top 10, :left 30, :bottom 30, :right 10},
   :data
   [{:name "table",
     :values
     [{:x 0, :y 0, :z 0}
      {:x 0, :y 1, :z 0.05}
      {:x 0, :y 2, :z 0.1}
      {:x 0, :y 3, :z 0.15}
      {:x 0, :y 4, :z 0.2}
      {:x 0, :y 5, :z 0.25}
      {:x 0, :y 6, :z 0.3}
      {:x 0, :y 7, :z 0.35}
      {:x 0, :y 8, :z 0.4}
      {:x 0, :y 9, :z 0.45}
      {:x 1, :y 0, :z 0.05}
      {:x 1, :y 1, :z 0.1}
      {:x 1, :y 2, :z 0.15}
      {:x 1, :y 3, :z 0.2}
      {:x 1, :y 4, :z 0.25}
      {:x 1, :y 5, :z 0.3}
      {:x 1, :y 6, :z 0.35}
      {:x 1, :y 7, :z 0.4}
      {:x 1, :y 8, :z 0.45}
      {:x 1, :y 9, :z 0.5}
      {:x 2, :y 0, :z 0.1}
      {:x 2, :y 1, :z 0.15}
      {:x 2, :y 2, :z 0.2}
      {:x 2, :y 3, :z 0.25}
      {:x 2, :y 4, :z 0.3}
      {:x 2, :y 5, :z 0.35}
      {:x 2, :y 6, :z 0.4}
      {:x 2, :y 7, :z 0.45}
      {:x 2, :y 8, :z 0.5}
      {:x 2, :y 9, :z 0.55}
      {:x 3, :y 0, :z 0.15}
      {:x 3, :y 1, :z 0.2}
      {:x 3, :y 2, :z 0.25}
      {:x 3, :y 3, :z 0.3}
      {:x 3, :y 4, :z 0.35}
      {:x 3, :y 5, :z 0.4}
      {:x 3, :y 6, :z 0.45}
      {:x 3, :y 7, :z 0.5}
      {:x 3, :y 8, :z 0.55}
      {:x 3, :y 9, :z 0.6}
      {:x 4, :y 0, :z 0.2}
      {:x 4, :y 1, :z 0.25}
      {:x 4, :y 2, :z 0.3}
      {:x 4, :y 3, :z 0.35}
      {:x 4, :y 4, :z 0.4}
      {:x 4, :y 5, :z 0.45}
      {:x 4, :y 6, :z 0.5}
      {:x 4, :y 7, :z 0.55}
      {:x 4, :y 8, :z 0.6}
      {:x 4, :y 9, :z 0.65}
      {:x 5, :y 0, :z 0.25}
      {:x 5, :y 1, :z 0.3}
      {:x 5, :y 2, :z 0.35}
      {:x 5, :y 3, :z 0.4}
      {:x 5, :y 4, :z 0.45}
      {:x 5, :y 5, :z 0.5}
      {:x 5, :y 6, :z 0.55}
      {:x 5, :y 7, :z 0.6}
      {:x 5, :y 8, :z 0.65}
      {:x 5, :y 9, :z 0.7}
      {:x 6, :y 0, :z 0.3}
      {:x 6, :y 1, :z 0.35}
      {:x 6, :y 2, :z 0.4}
      {:x 6, :y 3, :z 0.45}
      {:x 6, :y 4, :z 0.5}
      {:x 6, :y 5, :z 0.55}
      {:x 6, :y 6, :z 0.6}
      {:x 6, :y 7, :z 0.65}
      {:x 6, :y 8, :z 0.7}
      {:x 6, :y 9, :z 0.75}
      {:x 7, :y 0, :z 0.35}
      {:x 7, :y 1, :z 0.4}
      {:x 7, :y 2, :z 0.45}
      {:x 7, :y 3, :z 0.5}
      {:x 7, :y 4, :z 0.55}
      {:x 7, :y 5, :z 0.6}
      {:x 7, :y 6, :z 0.65}
      {:x 7, :y 7, :z 0.7}
      {:x 7, :y 8, :z 0.75}
      {:x 7, :y 9, :z 0.8}
      {:x 8, :y 0, :z 0.4}
      {:x 8, :y 1, :z 0.45}
      {:x 8, :y 2, :z 0.5}
      {:x 8, :y 3, :z 0.55}
      {:x 8, :y 4, :z 0.6}
      {:x 8, :y 5, :z 0.65}
      {:x 8, :y 6, :z 0.7}
      {:x 8, :y 7, :z 0.75}
      {:x 8, :y 8, :z 0.8}
      {:x 8, :y 9, :z 0.85}
      {:x 9, :y 0, :z 0.45}
      {:x 9, :y 1, :z 0.5}
      {:x 9, :y 2, :z 0.55}
      {:x 9, :y 3, :z 0.6}
      {:x 9, :y 4, :z 0.65}
      {:x 9, :y 5, :z 0.7}
      {:x 9, :y 6, :z 0.75}
      {:x 9, :y 7, :z 0.8}
      {:x 9, :y 8, :z 0.85}
      {:x 9, :y 9, :z 0.9}]}],
   :scales
   [{:name "x",
     :type "ordinal",
     :range "width",
     :domain {:data "table", :field "x"}}
    {:name "y",
     :type "ordinal",
     :range "height",
     :reverse true ;else we get upside down...     
     :domain {:data "table", :field "y"},
     ;:nice true
     }
    {:name "z",
     :type "linear",
     :domain [0 0.25 0.5 0.75 1],
     :range ["#a50026" "#ffcc66" "#ffffbf" "#66ff33"],
     :zero false}],
   :axes [{:type "x", :scale "x"} {:type "y", :scale "y"}],
   :legends [{:fill "z" :values [0.0 0.25 0.5 0.75 1.0]}]
   :marks
   [{:type "rect",
     :from {:data "table"},
     :properties
     {:enter
      {:x {:scale "x", :field "x"},
       :width {:scale "x", :band true},
       :y {:scale "y", :field "y"},
       :height {:scale "y", :band true},
       :fill {:scale "z", :field "z"}}}}]})

(defn heat-spec! [n]
  {:width 800,
   :height 800,
   :padding "strict" #_{:top 10, :left 60, :bottom 60, :right 10},
   :data
   [{:name "table",
     :values
     (vec (linear-heat n n))}],
   :scales
   [{:name "x",
     :type "ordinal",
     :range "width",
     :domain {:data "table", :field "x"}
     ;:points true
     }
    {:name "y",
     :type "ordinal",
     :range "height",
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
   :axes [{:type "x",
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
       :width {:scale "x", :band true},
       :y {:scale "y", :field "y"},
       :height {:scale "y", :band true},
       :fill {:scale "z", :field "z"}}}}]})



(defn data->heatspec! [xs]
    {:width 400,
     :height 400,
     :padding "strict",
     :data
     [{:name "table",
       :values
       xs}],
     :scales
     [{:name "x",
       :type "ordinal",
       :range "width",
       :domain {:data "table", :field "x"}
                                        ;:points true
       }
      {:name "y",
       :type "ordinal",
       :range "height",
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
     :axes [{:type "x",
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
         :width {:scale "x", :band true},
         :y {:scale "y", :field "y"},
         :height {:scale "y", :band true},
         :fill {:scale "z", :field "z"}}}}]})


;; (defn layers->heatmap [xs]
;;    {:width 400,
;;      :height 400,
;;      :padding "strict",
;;      :data
;;      [{:name "table",
;;        :values
;;        xs}],
;;     "scales" [
;;              {
;;       "name" "g",
;;       "type" "ordinal",
;;       "range" "height",
;;       "padding" 0.15,
;;       "domain" {
;;         "data" "barley", "field" "site",
;;         "sort" {"field" "yield", "op" "median"}
;;       },
;;       "reverse" true
;;     },
;;     {
;;       "name" "x",
;;       "type" "linear",
;;       "nice" true,
;;       "range" "width",
;;       "domain" {"data" "barley", "field" "yield"}
;;     },
;;     {
;;       "name" "c",
;;       "type" "ordinal",
;;       "range" "category10",
;;       "domain" {"data" "barley", "field" "year"}
;;     }
;;   ],
;;   "axes" [
;;     {"type" "x", "scale" "x"}
;;   ],
;;   "legends" [
;;     {"fill" "c", "title" "year"}
;;   ],
;;   "marks" [
;;     {
;;       "name" "sites",
;;       "type" "group",
;;       "from" {
;;         "data" "barley",
;;         "transform" [{"type" "facet", "groupby" ["site"]}]
;;       },
;;       "scales" [
;;         {
;;           "name" "y",
;;           "type" "ordinal",
;;           "range" "height",
;;           "points" true,
;;           "padding" 1.2,
;;           "domain" {
;;             "data" "barley", "field" "variety",
;;             "sort" {"field" "yield", "op" "median"}
;;           },
;;           "reverse" true
;;         }
;;       ],
;;       "axes" [
;;         {
;;           "type" "y",
;;           "scale" "y",
;;           "tickSize" 0,
;;           "properties" {"axis" {"stroke" {"value" "transparent"}}}
;;         }
;;       ],
;;       "properties" {
;;         "enter" {
;;           "x" {"value" 0.5},
;;           "y" {"scale" "g", "field" "key"},
;;           "height" {"scale" "g", "band" true},
;;           "width" {"field" {"group" "width"}},
;;           "stroke" {"value" "#ccc"}
;;         }
;;       },
;;       "marks" [
;;         {
;;           "type" "symbol",
;;           "properties" {
;;             "enter" {
;;               "x" {"scale" "x", "field" "yield"},
;;               "y" {"scale" "y", "field" "variety"},
;;               "size" {"value" 50},
;;               "stroke" {"scale" "c", "field" "year"},
;;               "strokeWidth" {"value" 2},
;;               "fill" {"value" "transparent"}
;;             }
;;           }
;;         }
;;       ]
;;     },
;;     {
;;       "type" "text",
;;       "from" {"mark" "sites"},
;;       "properties" {
;;         "enter" {
;;           "x" {"field" {"group" "width"}, "mult" 0.5},
;;           "y" {"field" "y", "offset" -2},
;;           "fontWeight" {"value" "bold"},
;;           "text" {"field" "datum.site"},
;;           "align" {"value" "center"},
;;           "baseline" {"value" "bottom"},
;;           "fill" {"value" "#000"}
;;         }
;;       }
;;     }
;;   ]

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

(defn vega-chart [{:keys [chart]}]
  (r/create-class
   {:display-name "vega-chart"
    :reagent-render (fn [] [:div])
    :component-did-mount
    (fn [this]
      (.update (chart {:el (r/dom-node this)})))}))

(defn parse-input []
  (let [{:keys [input]} @app-state]
    (swap! app-state assoc :chart nil :error nil)
    (-> (reader/read-string input)
        (vega-tools/validate-and-parse)
        (p/catch #(swap! app-state assoc :error %))
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
      (let [{:keys [input error chart]} @app-state]
        [:div
         [:h1 "vega-tools example"]
         [:div.container-fluid
          [:div.editor.col-md-6
           [:button {:on-click #(parse-input)} "Parse"] [:br]
           [:textarea.spec-input
            {:default-value input
             :on-change #(swap! app-state assoc :input (-> % .-target .-value))}]]]
          [:div
           (cond
             error [:div
                    [:h2 "Validation error"]
                    [:pre (with-out-str (pprint/pprint error))]]
             chart [vega-chart {:chart chart}]
             :else "Processing...")]]))))


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


;; (def fake-temp-data
;;   [{"x": 0, "y": 0, "temp": 0},
;;    {"x": 0, "y": 1, "temp": 1},
;;    {"x": 0, "y": 2, "temp": 2},
;;    {"x": 0, "y": 3, "temp": 3},
;;    {"x": 0, "y": 4, "temp": 4},
;;    {"x": 0, "y": 5, "temp": 5},
;;    {"x": 0, "y": 6, "temp": 6},
;;    {"x": 0, "y": 7, "temp": 7},
;;    {"x": 0, "y": 8, "temp": 8},
;;    {"x": 0, "y": 9, "temp": 9},
;;    {"x": 1, "y": 0, "temp": 1},
;;    {"x": 1, "y": 1, "temp": 2},
;;    {"x": 1, "y": 2, "temp": 3},
;;    {"x": 1, "y": 3, "temp": 4},
;;    {"x": 1, "y": 4, "temp": 5},
;;    {"x": 1, "y": 5, "temp": 6},
;;    {"x": 1, "y": 6, "temp": 7},
;;    {"x": 1, "y": 7, "temp": 8},
;;    {"x": 1, "y": 8, "temp": 9},
;;    {"x": 1, "y": 9, "temp": 10},
;;    {"x": 2, "y": 0, "temp": 2},
;;    {"x": 2, "y": 1, "temp": 3},
;;    {"x": 2, "y": 2, "temp": 4},
;;    {"x": 2, "y": 3, "temp": 5},
;;    {"x": 2, "y": 4, "temp": 6},
;;    {"x": 2, "y": 5, "temp": 7},
;;    {"x": 2, "y": 6, "temp": 8},
;;    {"x": 2, "y": 7, "temp": 9},
;;    {"x": 2, "y": 8, "temp": 10},
;;    {"x": 2, "y": 9, "temp": 11},
;;    {"x": 3, "y": 0, "temp": 3},
;;    {"x": 3, "y": 1, "temp": 4},
;;    {"x": 3, "y": 2, "temp": 5},
;;    {"x": 3, "y": 3, "temp": 6},
;;    {"x": 3, "y": 4, "temp": 7},
;;    {"x": 3, "y": 5, "temp": 8},
;;    {"x": 3, "y": 6, "temp": 9},
;;    {"x": 3, "y": 7, "temp": 10},
;;    {"x": 3, "y": 8, "temp": 11},
;;    {"x": 3, "y": 9, "temp": 12},
;;    {"x": 4, "y": 0, "temp": 4},
;;    {"x": 4, "y": 1, "temp": 5},
;;    {"x": 4, "y": 2, "temp": 6},
;;    {"x": 4, "y": 3, "temp": 7},
;;    {"x": 4, "y": 4, "temp": 8},
;;    {"x": 4, "y": 5, "temp": 9},
;;    {"x": 4, "y": 6, "temp": 10},
;;    {"x": 4, "y": 7, "temp": 11},
;;    {"x": 4, "y": 8, "temp": 12},
;;    {"x": 4, "y": 9, "temp": 13},
;;    {"x": 5, "y": 0, "temp": 5},
;;    {"x": 5, "y": 1, "temp": 6},
;;    {"x": 5, "y": 2, "temp": 7},
;;    {"x": 5, "y": 3, "temp": 8},
;;    {"x": 5, "y": 4, "temp": 9},
;;    {"x": 5, "y": 5, "temp": 10},
;;    {"x": 5, "y": 6, "temp": 11},
;;    {"x": 5, "y": 7, "temp": 12},
;;    {"x": 5, "y": 8, "temp": 13},
;;    {"x": 5, "y": 9, "temp": 14},
;;    {"x": 6, "y": 0, "temp": 6},
;;    {"x": 6, "y": 1, "temp": 7},
;;    {"x": 6, "y": 2, "temp": 8},
;;    {"x": 6, "y": 3, "temp": 9},
;;    {"x": 6, "y": 4, "temp": 10},
;;    {"x": 6, "y": 5, "temp": 11},
;;    {"x": 6, "y": 6, "temp": 12},
;;    {"x": 6, "y": 7, "temp": 13},
;;    {"x": 6, "y": 8, "temp": 14},
;;    {"x": 6, "y": 9, "temp": 15},
;;    {"x": 7, "y": 0, "temp": 7},
;;    {"x": 7, "y": 1, "temp": 8},
;;    {"x": 7, "y": 2, "temp": 9},
;;    {"x": 7, "y": 3, "temp": 10},
;;    {"x": 7, "y": 4, "temp": 11},
;;    {"x": 7, "y": 5, "temp": 12},
;;    {"x": 7, "y": 6, "temp": 13},
;;    {"x": 7, "y": 7, "temp": 14},
;;    {"x": 7, "y": 8, "temp": 15},
;;    {"x": 7, "y": 9, "temp": 16},
;;    {"x": 8, "y": 0, "temp": 8},
;;    {"x": 8, "y": 1, "temp": 9},
;;    {"x": 8, "y": 2, "temp": 10},
;;    {"x": 8, "y": 3, "temp": 11},
;;    {"x": 8, "y": 4, "temp": 12},
;;    {"x": 8, "y": 5, "temp": 13},
;;    {"x": 8, "y": 6, "temp": 14},
;;    {"x": 8, "y": 7, "temp": 15},
;;    {"x": 8, "y": 8, "temp": 16},
;;    {"x": 8, "y": 9, "temp": 17},
;;    {"x": 9, "y": 0, "temp": 9},
;;    {"x": 9, "y": 1, "temp": 10},
;;    {"x": 9, "y": 2, "temp": 11},
;;    {"x": 9, "y": 3, "temp": 12},
;;    {"x": 9, "y": 4, "temp": 13},
;;    {"x": 9, "y": 5, "temp": 14},
;;    {"x": 9, "y": 6, "temp": 15},
;;    {"x": 9, "y": 7, "temp": 16},
;;    {"x": 9, "y": 8, "temp": 17},
;;    {"x": 9, "y": 9, "temp": 18}]
;;   )



(def js-spec (clj->js heat-spec))

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
