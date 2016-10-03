(ns figdemo.util
    (:require-macros [cljs.core.async.macros :refer [go]])
    (:require [cljs.core.async :as async]
              [goog.dom :as dom]
              [goog.events :as events]
              ;;note, you have to require google classes...
              [goog.ui.ComboBox]
              [goog.ui.Select]
              [goog.ui.Slider]
              [goog.ui.tree.TreeControl]))




;;Utils
;;=====

;;alternately, is dom/isElement 
(defn element? [obj] (instance? js/Element obj))
;;Note: we can use instance? just like in clojure.
;;Rather than java classes, we have javascript classes..
;;One common on is javascript Element types...which
;;are html elements.
(defn element [nm]
  (if (element? nm)
      nm
      (dom/getElement nm)))

;;Generic rendering, where rendering is emitting/inserting
;;html into a node on the dom (a target).
;;This typically affects interop with the closure libs,
;;since most of them (and other libraries) have a
;;render method built in.  So, if we define a simple API
;;that wraps renderin, we can just extend the API to
;;new types as necessary.
(defprotocol IRenderable
  (render- [obj tgt]))
(defprotocol IClearable
  (clear-  [obj]))

;;Thinking about whether this is actually
;;what we want to do...we may not want
;;to allow nil to creep in unexpectedly.
;;for now, we'll explicitly test.
(comment 
(extend-protocol IClearable
  nil
  (clear- [obj] nil))
(extend-protocol IRenderable
  nil
  (render- [obj tgt] nil))
)

(defn render-object! [obj tgt]
  (. obj (render (element tgt)))
  )

;;may be better to use multimethods here..
;;dunno.
(defn render! [obj tgt]
  (if (satisfies?  IRenderable obj) (render- obj (element tgt))
      ;;basically, call the render method on the thing...
      ;;this is a common wrapper for goog.closure stuff.
      (render-object! obj tgt)))

;;turn the element's inner html into nada
(defn clear!  [obj]
  (if (satisfies?  IClearable obj) (clear- obj)     
  (aset (element obj) "innerHTML" "")))

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

;;Path recovery
(def lits #{"[" "{" "#" ":"})
(defn read-path [xs]
  (for [x xs]
    (if (lits (aget x 0)) (cljs.reader/read-string x)
        x)))

;;interesting....
;;if you use (type ...) at the repl
;;on a js or google closure object,
;;you get back a js map of the object
;;along with its properties.
(defn get-path [^goog.ui.tree.TreeNode nd]
  (loop [acc nil
         x nd]
    (if-let [p (.getParent x)]
      (recur (cons (.getText x) acc)
             p)
      (cons (.getText x) acc))))
 
(defn update-path! [the-path nd]
  (when-not (.hasChildren nd) ;we have a path!
    (->> nd
         (get-path)
         (read-path)
         (reset! the-path))))

;;combox-box path...
;;this will be a companion to the
;;path-tree.
;;The combo-path will have
;;one or more combo-boxes for each
;;level of the tree.
;;As we select things, we're basically
;;building up keys for the box.
;;When we select a level, we (maybe? )
;;add another combo-box.
;;Either that, or we pre-compute the
;;values for the next box.
;;If we change values for a box higher
;;or lefter in the tree, then we
;;should unselect (or hide?) all later
;;boxes.

;;We can define a propogation relationship
;;between the boxes...
;;So, if a higher box changes, it propogates
;;constraints to lower boxes...
;;Ah, we're talking constraint prop...
;;We have one-directional flows..
;;-> > > >
;;Just default to changing to the first
;;element in the set....i.e. do a dfs
;;by default.  So if you change something
;;earlier, it just propogates down to the "left"
;;most leaf in the tree.  Easy.
;;So we have several ordered sets of values.
;;Alphabet [a b c] Evens [0 1 3] Odds [2 4 6]


  ;;we can probably create a generic interface
  ;;for all this goog.ui stuff, akin to our
  ;;picc wrapper....

  (defn ^goog.ui.MenuItem ->item [x]
    (goog.ui.MenuItem. (str x))
    )
  (defn ^goog.ui.MenuItem ->combo-item [x]
    (goog.ui.ComboBoxItem. (str x))
    )
  ;;Note: there's a distinction here between
  ;;children and items...."children" are
  ;;other ui nodes, i.e. goog.ui.Control/Component
  ;;So, we need to be careful.  For now my hacky lib
  ;;works okay.
 (defprotocol IWidgetable
   (as-widget [o]))
 (defprotocol IWidget
   (add-child [w c]))

 (defn add! [w c]
   (do (add-child (as-widget w) c)
       w))

 ;;note about custom types.
 ;;protocol methods are typically -deref and the like....
 (extend-type goog.ui.ComboBox
   IWidgetable
   (as-widget [o] o)
   IWidget
   (add-child [w c]
     (doto w (. (addItem (->combo-item c)))))
   IDeref
   (-deref [o] (.getValue o))
   ICounted
   (-count [o] (.getItemCount o))
   ILookup
   (-lookup ([o k] (.getItemAt o k))
            ([o k not-found]
             (try (.getItemAt o k) 
                  (catch :default not-found))))
   )
  
 (defn ->cbox [items & {:as opts :keys [check-box default-text] :or
                        {check-box true
                         default-text "Select a value"}}]
   (let [cb (goog.ui.ComboBox.)
         _  (do (. cb (setUseDropdownArrow check-box))
                (. cb (setDefaultText default-text)))
          _  (doseq [i items]
               (add! cb  i))]
     cb))

 (extend-type goog.ui.Select
   IWidgetable
   (as-widget [o] o)
   IWidget
   (add-child [w c]
     (doto w (. (addItem (->item c)))))
   IDeref
   (-deref [o] (.getValue o))
   ICounted
   (-count [o] (.getItemCount o))
   ILookup
   (-lookup ([o k] (.getItemAt o k))
            ([o k not-found]
             (try (.getItemAt o k) 
                  (catch :default not-found))))
   )
(defn ->select [items & {:keys [id] :or {id "selection"}}]
  (let [s (goog.ui.Select.)
        _ (.setId s id)
        _ (doseq [i items]
            (add! s i))]
    s))


 (extend-type goog.ui.Slider
  ; IWidgetable
  ; (as-widget [o] o)
  ; IWidget
  ; (add-child [w c]
  ;   (doto w (. (addItem (->item c)))))
   IDeref
   (-deref [o] (.getValue o))
   ;; ICounted
   ;; (-count [o] (.getItemCount o))
   ;; ILookup
   ;; (-lookup ([o k] (.getItemAt o k))
   ;;          ([o k not-found]
   ;;           (try (.getItemAt o k) 
   ;;                (catch :default not-found))))
   )

;;Note: the visual stylings don't show anything by default,
;;unless you style the elements according to css/slider.css
;;It just looks like a blank div in this case...
(defn ->vslider [w & {:keys [on-change]}]
  (let [s2   (doto (goog.ui.Slider.)
                 (.setOrientation
                  goog.ui.Slider.Orientation.VERTICAL)
                 (.setStep nil)
                 ) 
        _     (.createDom  s2) ;;this is for programatically creating the DOM el.
        el    (.getElement s2)
        style (aget el "style")
        _     (aset style "width"  "20px")
        _     (aset style "height" "200px")
        _ (when on-change
            (. s2 (addEventListener goog.ui.Component.EventType.CHANGE
                                    on-change)))]
    s2))        
                                      

 ;;Probably want some helpers like as-component. 
(defn control?   [x]  (instance? goog.ui.Control x))
(defn component? [x]  (instance? goog.ui.Component x))



 
 (defn ^goog.ui.Control ->control [x]
   (if  (control? x) x 
       (goog.ui.Control. (str x))))

 ;;Okay....so what I learned about goog....
 ;;Containers are cool, EXCEPT they only work with things that
 ;;subclass goog.ui.Control....which combobox DOES NOT....
 ;;We can approximate combox, though, by using goog.ui.Select,
 ;;which provides a selection model with a menu button.
 ;;Select subclasses control, so it works...

 ;;Yeah, we're going to ditch the whole html shitsandwhich...
 ;;Stick in cljs as much as possible, use combinators to
 ;;declare things easier...
 ;;wrap the goog.ui stuff programmatically...

 (defn ^goog.ui.Container ->shelf [xs & {:keys [id class]
                                         :or {id "Shelf"
                                              class "goog-inline-block"}}]
   (let [hc (goog.ui.Container. goog.ui.Container.Orientation.HORIZONTAL)
         _  (. hc (setId  id))
         _  (doseq [x xs]
              (let [c  (->control x)
                     _ (doto c
                        ;(.addClassName class)
                      ;  (.setId x)
                        ;(.setDispatchTransitionEvents
                        ; goog.ui.Component.State.ALL true)
                        )]
                (. hc (addChild c true))))]
     hc))

(comment

;; // Programmatically create a horizontal container.
   ;;  var hc = new goog.ui.Container(goog.ui.Container.Orientation.HORIZONTAL);
   ;;  hc.setId('Horizontal Container');

   ;;  // Pre-render the container, just to do something different.
   ;;  hc.render(goog.dom.getElement('hc'));
   ;;  goog.array.forEach(
   ;;      ['Happy', 'Sleepy', 'Doc', 'Bashful', 'Sneezy', 'Grumpy', 'Dopey'],
   ;;      function(item) {
   ;;        var c = new goog.ui.Control(item);
   ;;        c.addClassName('goog-inline-block');
   ;;        c.setId(item);
   ;;        // For demo purposes, have controls dispatch transition events.
   ;;        c.setDispatchTransitionEvents(goog.ui.Component.State.ALL, true);
   ;;        hc.addChild(c, true);
   ;;      });
   ;;  hc.getChild('Doc').setEnabled(false);
   ;;  goog.events.listen(hc, EVENTS, logEvent);

   ;;  // Hook up checkboxes.
   ;;  goog.events.listen(goog.dom.getElement('show_hc'),
   ;;      goog.events.EventType.CLICK,
   ;;      function(e) {
   ;;        var t = goog.now();
   ;;        hc.setVisible(e.target.checked);
   ;;        goog.log.info(logger, (e.target.checked ? 'Showed' : 'Hid') +
   ;;            ' horizontal container in ' + (goog.now() - t) + 'ms');
   ;;      });
   ;;  goog.events.listen(goog.dom.getElement('enable_hc'),
   ;;      goog.events.EventType.CLICK,
   ;;      function(e) {
   ;;        var t = goog.now();
   ;;        hc.setEnabled(e.target.checked);
   ;;        // If the container as a whole is disabled, you can't enable/disable
   ;;        // child controls.
   ;;        goog.dom.getElement('enable_doc').disabled = !hc.isEnabled();
   ;;        goog.log.info(logger, (e.target.checked ? 'Enabled' : 'Disabled') +
   ;;            ' horizontal container in ' + (goog.now() - t) + 'ms');
   ;;      });
   ;;  goog.events.listen(goog.dom.getElement('show_doc'),
   ;;      goog.events.EventType.CLICK,
   ;;      function(e) {
   ;;        hc.getChild('Doc').setVisible(e.target.checked);
   ;;      });
   ;;  goog.events.listen(goog.dom.getElement('enable_doc'),
   ;;      goog.events.EventType.CLICK,
   ;;      function(e) {
   ;;        hc.getChild('Doc').setEnabled(e.target.checked);
   ;;      });
   ;;  goog.events.listen(goog.dom.getElement('enable_hc_events'),
   ;;      goog.events.EventType.CLICK,
   ;;      function(e) {
   ;;        hc.forEachChild(function(c) {
   ;;          if (e.target.checked) {
   ;;            // Enable all transition events.
   ;;            c.setDispatchTransitionEvents(goog.ui.Component.State.ALL, true);
   ;;          } else {
   ;;            // Disable all transition events (except for HOVER, which is used
   ;;            // by containers internally).
   ;;            c.setDispatchTransitionEvents(goog.ui.Component.State.ALL, false);
   ;;            c.setDispatchTransitionEvents(goog.ui.Component.State.HOVER,
   ;;                true);
   ;;          }
   ;;        });
   ;;        goog.log.info(logger, (e.target.checked ? 'Enabled' : 'Disabled') +
   ;;            ' state transition events for this container\'s children');
   ;;      });


 
  )



;;Can we create a reactive combo-box tree that
;;allows users to interactively explore paths
;;as with the TreeControl? 

;;so...we build the tree....from there, we can
;;derive a widget that sits (possibly above?)
;;the tree, and listens to selections...
;;The reactive selections manifest as
;;the path in the tree changes...
;;There are k-levels to the combo box(es),
;;which correspond to the tree hierarchy....

;;Maybe this is a path hierarchy?
;;So, the path-tree does what?

;;a simple description of a path that maintains
;;state, deferring to a goog.ui.treecontrol internally.
;;Maintains an atom that listens for changes in the tree,
;;and reactively computes the current path selected.
(defrecord path-tree [^goog.ui.tree.TreeControl tree
                      path
                      id]
  IRenderable
  (render- [this el] (render! tree el))
  IClearable
  (clear- [this]
    (when id (clear! (element id)))))
                  
(defn path [db & {:keys [id]}]
  (let [tr (init-tree db)
        the-path (atom nil)
        _  (. tr  (addEventListener goog.events.EventType.CHANGE
              (fn [e] (update-path! the-path (.getSelectedItem tr)))))]
    (->path-tree tr the-path id)))                                  

;;This is a helper fn to clean up our unordered database, and
;;convert it into a format that's palatable for the pathdb.
(defn db->pathdb [db]
  (if (map? db) 
    (for [[k v] (sort-by first (seq db))]    
      (if (map? v)
        {:name k
         :childNodes (vec (db->pathdb v))}
        {:name k
         :url "blah"}))
    {:name db
     :url "blah"}))

(defn db->path [db & {:keys [id]}]
  (-> db
      (db->pathdb)
      (path :id id)))

(comment
  (def evt (atom nil))
  (def the-tree (atom (init-tree)))      
  
  (. @the-tree
     (addEventListener goog.events.EventType.CHANGE
                       (fn [e] (do
                                 (reset! evt e)
                                 (println "tree-changed!")))))
  )



