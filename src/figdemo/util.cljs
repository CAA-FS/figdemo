(ns figdemo.util
    (:require-macros [cljs.core.async.macros :refer [go]])
    (:require [cljs.core.async :as async]
              [goog.dom :as dom]
              [goog.events :as events]
              [goog.ui.ComboBox]
              [goog.ui.tree.TreeControl]))

;;Utils
;;=====

(defn element? [obj] (instance? js/Element obj))
;;Note: we can use instance? just like in clojure.
;;Rather than java classes, we have javascript classes..
;;One common on is javascript Element types...which
;;are html elements.
(defn element [nm]
  (if (element? nm)
      nm
      (dom/getElement nm)))

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
(defn clear! [el] (aset (element el) "innerHTML" ""))

;;going to try to use trees for path selection.
;;Either that or comboboxes....
 ;; var treeData = [
 ;;      {name: 'Advanced Tooltip', url: 'advancedtooltip.html'},
 ;;      {name: 'Autocomplete', childNodes: [
 ;;        {name: 'Basic', url: 'autocomplete-basic.html'},
 ;;        {name: 'Remote', url: 'autocompleteremote.html'},
 ;;        {name: 'Rich Remote', url: 'autocompleterichremote.html'}
 ;;      ]},

(def tree-data [{:name "A" :url "blah.html"}
                {:name "B" :childNodes
                 [{:name "Basic" :url "blah.html"}
                  {:name "Remote" :url "remote.html"}
                  {:name "Rich Remote" :url "rr.html"}]}])
  
    ;; function buildNode(parent, nodeArray) {
    ;;   for (var i = 0, node; node = nodeArray[i]; i++) {
    ;;     if (node.name) {
    ;;       var childNode = parent.getTree().createNode();
    ;;       parent.add(childNode);
    ;;       if (node.url) {
    ;;         childNode.setSafeHtml(goog.html.SafeHtml.create('a', {
    ;;           'href': node.url,
    ;;           'title': node.name,
    ;;           'target': 'demo'
    ;;         }, node.name));
    ;;         // Need to prevent BaseNode.onClick_ from calling preventDefault.
    ;;         childNode.onClick_ = goog.nullFunction;
    ;;       } else if (node.childNodes) {
    ;;         childNode.setText(node.name);
    ;;         buildNode(childNode, node.childNodes);
    ;;       }
    ;;     }
    ;;   }
;;                                           }

(defn build-node [parent children]
  (doseq [nd children]
      (let [tr         (.getTree   parent)
            child-node (.createNode tr)
            _          (.setText child-node (:name nd))
            _          (.add parent child-node)]
        (when-let [children (:childNodes nd)]
          (build-node child-node children)))))

(defn init-tree
  ([data]
   (let [tree (goog.ui.tree.TreeControl. "The Tree")
         _    (.setIsUserCollapsible tree false)
         _    (build-node tree data)]
    tree))
  ([] (init-tree tree-data)))

;;interesting....
;;if you use (type ...) at the repl
;;on a js or google closure object,
;;you get back a js map of the object
;;along with its properties.
(defn get-path [nd]
  (loop [acc nil
         x nd]
    (if-let [p (.getParent x)]
      (recur (cons (.getText x) acc)
             p)
      (cons (.getText x) acc))))
 
(defn update-path! [the-path nd]
  (when-not (.hasChildren nd) ;we have a path!
    (reset! the-path (get-path nd))))

(defrecord path-widget [tree path])
(defn path [db]
  (let [tr (init-tree db)
        the-path (atom nil)
        _  (. tr  (addEventListener goog.events.EventType.CHANGE
              (fn [e] (update-path! the-path (.getSelectedItem tr)))))]
    (->path-widget tr the-path)))                                  

(comment
  (def evt (atom nil))
  (def the-tree (atom (init-tree)))      
  
  (. @the-tree
     (addEventListener goog.events.EventType.CHANGE
                       (fn [e] (do
                                 (reset! evt e)
                                 (println "tree-changed!")))))
  )

(defn render! [ctrl el]
  (. ctrl (render (element el))))

