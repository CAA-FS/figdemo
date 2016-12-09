(ns figdemo.high
  (:require [cljsjs.highcharts :as high]
            [reagent.core :as reagent]))
;;note: the require brings in the JS
;;lib, so we have js/Highcharts as a
;;namespace.

;;load up highcharts.
;;(. highcharts.core main)

(def chart-config
  {:chart {:type "bar"}
   :title {:text "Historic World Population by Region"}
   :subtitle {:text "Source: Wikipedia.org"}
   :xAxis {:categories ["Africa" "America" "Asia" "Europe" "Oceania"]
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
   :series [{:name "Year 1800"
             :data [107 31 635 203 2]}
            {:name "Year 1900"
             :data [133 156 947 408 6]}
            {:name "Year 2008"
             :data [973 914 4054 732 34]}]
   })

;;this is the background, static if you will...
(defn chart-render []
  [:div {:style {:min-width "310px" :max-width "800px" 
                 :height "400px" :margin "0 auto"}}])

;;this is our rendering function.
;;Typically, for the goog libs, we'll call this guy
;;automagically ala render..
(defn chart-did-mount [this]
  (js/Highcharts.Chart. (reagent/dom-node this) (clj->js chart-config)))

;;this is a little inverted...
;;what we'd like to do is take a generic widget, and
;;build a compatibility layer out of it.
;;So, highcharts wants a node, and a confid to render to (in this example)
;;What we really have is a function that instantiates the
;;chart on a node.

(defn ->mounted-chart [opts]
  (fn [this]
    (js/Highcharts.Chart. (reagent/dom-node this) (clj->js opts))))

(defn chart-component []
    (reagent/create-class {:reagent-render chart-render
                           :component-did-mount (->mounted-chart chart-config)
                           #_chart-did-mount}))