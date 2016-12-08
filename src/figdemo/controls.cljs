;;Handles for predefined sets of input
;;controls.
(ns figdemo.controls
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [figdemo.util :as util]
            [cljs.core.async :as async]
            [goog.dom :as dom]
            [goog.events :as events]))

;;HTMLElements
;;Note: we can access common properties of these
;;guys, as defined at
;;https://developer.mozilla.org/en-US/docs/Web/API/HTMLInputElement
;;using the .-property form.

;;we have a pre-defined element, a button, called
;;"drawchart" in index.html 
(def draw-button (dom/getElement "drawchart"))
;;and a predefined file browser input element...
(def file-browser (dom/getElement "file"))
;;the place where we stick our charts.
(def the-chart (dom/getElement "the-chart"))
;;the place where we stick our table data.
(def the-table (dom/getElement "the-table"))
;;Place where we shove our tree selector
(def the-tree (dom/getElement "the-tree"))
;;from david nolen's tutorial. we'll
;;comment this out in production.
(def high (dom/getElement "high"))


;;stuff for loading/navigating tables
(def table-browser (dom/getElement "tad-file"))
(def load-tad-button  (dom/getElement "load-tad-button"))

(comment 
(let [clicks (util/listen draw-button "click")]
  (go (while true
        (.log js/console (<! clicks)))))
)
