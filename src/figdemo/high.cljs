(ns figdemo.high
  (:require [cljsjs.highcharts :as high]
            [reagent.core :as reagent]))
;;note: the require brings in the JS
;;lib, so we have js/Highcharts as a
;;namespace.

;;load up highcharts.
;;(. highcharts.core main)



;;Can we recompute our plot?
;;If the categories change, yes.

;;In this case, our categories are selected SRCs.
;;For right now, we only have one active SRC we're
;;looking at.  We want to move on with this.
(def tad-series
  [{:name "PreSurge"
    :data [107 31 635 203 2]}
   {:name "Surge"
    :data [133 156 947 408 6]}
   {:name "PostSurge"
    :data [973 914 4054 732 34]}])

;;note, our charts are predicated on specific performance metrics...
;;note: categories needs to be a seq of strings we expect to
;;label the x axis
;;series needs to be a seq of {:name :data}, alternately,
;;we should be able to treat  {series [data1 ... datan]}
;;as a map.

(defn tad-config [metric categories series]
  (let [vd (case metric
             "Fill"    "% Fill"
             "Surplus" "UICs")]
    {:chart {:type "bar"}
     :title {:text "Performance by Surge Period"}
     :subtitle {:text "Source: Interpolated Experimental Runs"}
     :xAxis {:categories categories
             :title {:text nil}}
     :yAxis {:min 0
             :title {:text (str metric "(" vd ")")
                     :align "high"}
             :labels {:overflow "justify"}}
     :tooltip {:valueSuffix vd}
     :plotOptions {:bar {:dataLabels {:enabled true}}}
     :legend {:layout "vertical"
              :align "right"
              :verticalAlign "top"
              :x -40
              :y 100
              :floating true
              :borderWidth 1
              :shadow true}
     :credits {:enabled false}
     :series series
     }))

(def demo-series
  [{:name "Year 1800"
    :data [107 31 635 203 2]}
   {:name "Year 1900"
    :data [133 156 947 408 6]}
   {:name "Year 2008"
    :data [973 914 4054 732 34]}])

(def demo-categories ["Africa" "America" "Asia" "Europe" "Oceania"])

(def demo-config
  {:chart {:type "bar"}
   :title {:text "Historic World Population by Region"}
   :subtitle {:text "Source: Wikipedia.org"}
   :xAxis {:categories demo-categories
           :title {:text nil}}
   :yAxis {:min 0
           :title {:text "Population (millions)"
                   :align "high"}
           :labels {:overflow "justify"}}
   :tooltip {:valueSuffix " millions"}
   :plotOptions {:bar {:dataLabels {:enabled true}}}
   :legend {:layout "vertical"
            :align "right"
            :verticalAlign "top"
            :x -40
            :y 100
            :floating true
            :borderWidth 1
            :shadow true}
   :credits {:enabled false}
   :series demo-series
   })



;;we're dealing with categorial data in bar charts, by default.
;;so, data of the form
;;[s1 [y1 y2 y3 y4]
;; s2 [y2 y2 y3 y4]]
;;or, if we're doing categories...
;;[s1 {c1 v c2 v c3 v}
;; s2 {c1 v c2 v c3 v} ...]
(defn categorical-series [xs & {:keys [categories]}]
  (let [cats (or categories (keys (val (first xs))))
        record->data #(into [] (map (fn [k] (get % k))) cats)]
    (into []
          (for [[ser r] xs]
            {:name ser
             :data (record->data r)}))))

(defn ->bar-chart [& {:keys [title subtitle categories
                             x-label y-label series
                             ]}]  
  {:chart    {:type "bar"}
   :title    {:text (or title "Bar Chart")}
   :subtitle {:text (or subtitle "Source: Thin Air")}
   :xAxis    {:categories (mapv str categories)
              :title {:text (when x-label (str x-label))}}
   :yAxis {:min 0
           :title {:text (or (when y-label (str y-label)) "Y")
                   :align "high"}
           :labels {:overflow "justify"}}
   ;:tooltip {:valueSuffix " millions"}
   :plotOptions {:bar {:dataLabels {:enabled true}}}
   :legend {:layout "vertical"
            :align "right"
            :verticalAlign "top"
            :x -40
            :y 100
            :floating true
            :borderWidth 1
            :shadow true}
   :credits {:enabled false}
   :series series})


(def bar-config (->bar-chart
                 :title    "Historic World Population by Region"
                 :subtitle "Source: Wikipedia.org"
                 :x-label  "Country"
                 :y-label  "Population (millions)"
                 :series (categorical-series {"Year 1800"
                                              {"Africa" 107 "America" 31 "Asia" 635 "Europe" 203 "Oceania" 2}
                                              "Year 1900"
                                              {"Africa" 133 "America" 156 "Asia" 947 "Europe" 408 "Oceania" 6}
                                              "Year 2008"
                                              {"Africa" 973 "America" 914 "Asia" 4054 "Europe" 732 "Oceania" 34}})))


;;this is the background, static if you will...
(defn chart-render []
  [:div {:style {:min-width "310px" :max-width "800px" 
                 :height "400px" :margin "0 auto"}}])

;;this is our rendering function.
;;Typically, for the goog libs, we'll call this guy
;;automagically ala render..
(defn chart-did-mount [this]
  (js/Highcharts.Chart. (reagent/dom-node this) (clj->js demo-config)))

;;this is a little inverted...
;;what we'd like to do is take a generic widget, and
;;build a compatibility layer out of it.
;;So, highcharts wants a node, and a confid to render to (in this example)
;;What we really have is a function that instantiates the
;;chart on a node.

(def chrt (atom nil))

(defn ->mounted-chart [opts]
  (fn [this]
    (let [c  (js/Highcharts.Chart. (reagent/dom-node this) (clj->js opts))
          _  (reset! chrt c)]
      c)))

(defn chart-component [& {:keys [spec chartref] :or {spec demo-config}}]
  (let [c  (->mounted-chart spec)
        _  (when chartref (reset! chartref c))]
    (reagent/create-class
     {:reagent-render chart-render
      :component-did-mount c})))

;;working with charts, to dynamically update the data...
(defn series-seq [c]
  (map (fn [x] [(.-name x) x]) (.-series c)))
(defn categories [c]
  (js->clj (.-categories (aget (.-xAxis c) 0))))

(defn series-map [c] (into {} (series-seq c)))

(defn map->point [c m]
  (reduce (fn [acc k]
            (conj acc  (or (get m k) nil)))
          []
          (categories c)))

;;This makes it real easy to update data to a (currently) bar chart.
;;So, if we have something like a channel (or even a fn) that
;;interacts with the chart, we can dynamically update the data
;;in response to user interaction (or some other event).
;;So, the chart becomes a model of the data.
(defn push-data!
  ([c k xs]
   (when-let [s (get (series-map c) k)]
     (.setData s (clj->js   (if (map? xs) (map->point c xs)  xs)))))
  ([c series-updates]
   (when (map? series-updates)
     (doseq [[k xs] series-updates]
       (push-data! c k xs)))))

;;An extension of this idea is to just provide an atom to the chart
;;component (the trends), and the chart component watches the
;;atom for changes (or channel), pushing data as updates come in.


#_(push-data! @chrt {"Year 1900" {"America" (rand-int 500) "Africa" (rand-int 500)} "Year 2008" {"Asia" (rand-int 600) "Europe" (rand-int 700)}})
