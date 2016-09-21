(ns figdemo.util
    (:require-macros [cljs.core.async.macros :refer [go]])
    (:require [cljs.core.async :as async]
              [goog.dom :as dom]
              [goog.events :as events]))


;;Utils
;;=====

;;a dumb way to get the current state of "this"
;;from js.  better way is to use 'this-as
(defn get-this [] (js* "this"))

;;from david nolen's blogpost
;;note: this will allow us to register multiple listeners
;;in a session.  We may want to preclude this, in production
;;it won't matter.
(defn listen [el type]
  (let [out (async/chan)]
    (events/listen el type
      (fn [e] (async/put! out e)))
    out))

;;just a wrapper around goog/events 
(defn listen! [el type f]
  (events/listen el type f))
   

(defn log! [txt] (.log js/console txt))
