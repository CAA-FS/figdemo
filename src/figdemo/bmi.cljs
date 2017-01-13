(ns figdemo.bmi
  (:require [reagent.core :as r]
            [cljsjs.react-slider]
            [figdemo.util :as util]))

;;Update: the assumably idiomitic way to do this in cljs.
(defn weak-fix [& input]
  (let [m (merge {:x 0 :y 0 :min 0 :max 0}
                    (apply hash-map input))]
    m))

;;we'd like to take this, and develop a simple component that can
;;operate as a little control panel.
(def slider-defaults 
  {:min 0 
   :max 100
   :step 1
   :min-distance 0
   :default-value 50
   :value       0
   :orientation :horizontal
   :class-name  :slider
   :handle-class-name :handle
   :with-bars    true})
;; min {number} default: 0

;; ;;The minimum value of the slider.
;; max {number} default: 100

;; ;;The maximum value of the slider.
;; step {number} default: 1

;; ;;Value to be added or subtracted on each step the slider makes. Must be greater than zero. max - min should be evenly divisible by the step value.
;; minDistance {number} default: 0

;; ;;The minimal distance between any pair of handles. Zero means they can sit on top of each other.
;; defaultValue {oneOfType([number, arrayOf(number)])} default: 0

;; ;;Determines the initial positions of the handles and the number of handles if the component has no children.

;; ;;If a number is passed a slider with one handle will be rendered. If an array is passed each value will determine the position of one handle. The values in the array must be sorted. If the component has children, the length of the array must match the number of children.
;; value {oneOfType([number, arrayOf(number)])} default: 0

;; ;;Like defaultValue but for controlled components.
;; orientation {oneOf(['horizontal', 'vertical'])} default: 'horizontal'

;; ;;Determines whether the slider moves horizontally (from left to right) or vertically (from top to bottom).
;; className {string} default: 'slider'

;; ;;The css class set on the slider node.
;; handleClassName {string} default: 'handle'

;; ;;The css class set on each handle node. In addition each handle will receive a numbered css class of the form ${handleClassName}-${i}, e.g. handle-0, handle-1, ...
;; handleActiveClassName {string} default: 'active'

;; ;;The css class set on the handle that is currently being moved.
;; withBars {boolean} default: false

;; ;;If true bars between the handles will be rendered.
;; barClassName {string} default: 'bar'

;; ;;The css class set on the bars between the handles. In addition bar fragment will receive a numbered css class of the form ${barClassName}-${i}, e.g. bar-0, bar-1, ...
;; pearling {bool} default: false

;; ;;If true the active handle will push other handles within the constraints of min, max, step and minDistance.
;; disabled {bool} default: false

;; If true the handles can't be moved.
;; snapDragDisabled {bool} default: false

;; ;;Disables handle move when clicking the slider bar.
;; invert {bool} default: false

;; ;;Inverts the slider.
;; onBeforeChange {func}

;; ;;Callback called before starting to move a handle.
;; onChange {func}

;; ;;Callback called on every value change.
;; onAfterChange {func}

;; ;;Callback called only after moving a handle has ended or when a new value is set by clicking on the slider.
;; onSliderClick {func}

;; ;;Callback called when the the slider is clicked (handle or bars). Receives the value at the clicked position as argument.
;; ;;Methods
;; getValue

;; ;;Returns the current value of the slider, which is a number in the case of a single slider, or an array of numbers in case of a multi slider.

(defn slider-opts [& {:keys
                      [min
                       max
                       step
                       min-distance
                       default-value
                       value
                       orientation
                       class-name
                       handle-class-name
                       with-bars
                       on-before-change
                       on-after-change
                       on-change
                       on-slider-click]
                      :as input}]
  (merge slider-defaults input))

(def default-opts (slider-opts))
(def Slider (r/adapt-react-class js/ReactSlider))

(defn ->slider [min max & opts]
  [Slider (apply slider-opts (into [:min min :max max] opts))
   ])

;;failed attempt at using rc-slider.
#_(defn react-slider
  ([param value min max bmi-data]
   (->slider min
             max 
             :on-change (fn [e]
                          (swap! bmi-data assoc param (.-target.getValue e))
                          (when (not= param :bmi)
                            (swap! bmi-data assoc :bmi nil)))))
  ([param value min max] (react-slider value min max (r/atom {:height 180 :weight 80}))))
                          

;;deviating from the original example...
(defn calc-bmi [bmi-data]
  (let [{:keys [height weight bmi] :as data} @bmi-data
        h (/ height 100)]
    (if (nil? bmi)
      (assoc data :bmi    (/ weight (* h h)))
      (assoc data :weight (* bmi h h)))))

(defn range-opts
  "Inconvenience function to help us get ranges working properly on ie11."
  [& opts]
  (let [m (apply hash-map opts)]
    (-> (if (and (util/ie11?) (:on-change m))
          (-> m (assoc :on-mouse-move (:on-change m)) (dissoc :on-change))
          m)
        (assoc :type "range"))))

;;Specialized slider that works around ie11 funk with the html range
;;widget.
(defn slider [param value min max bmi-data]
    [:input (range-opts :type "range" :value value :min min :max max
                        :style {:width "100%"}
                        ;;almost works.
                        :on-change  (fn [e]
                                      (swap! bmi-data assoc param (.-target.value e))
                                      (when (not= param :bmi)
                                 (swap! bmi-data assoc :bmi nil))))])

;;A slider that doesn't accept user input.
;;note the use of read-only.
(defn frozen-slider [param value min max bmi-data]  
  [:input {:type "range" :value value :min min :max max
           :style {:width "100%"}
           :read-only true
          ; :disabled true
           }])

;;bmi-data  {:height 100
;;           :weight 100}
;;when slider's value changes, updates the associated parameter in
;;bmi-data.
;;Every time bmi-component changes, bmi is recalculated
;;bmi is calculated based on the current value o
  


 
;;This is a "form-2" component, where we provide a render
;;function (basically a 0-arg fn), to display the component..
;;we prep it with some local state.
(defn bmi-component []
  (let [bmi-data (r/atom {:height 180 :weight 80})
        width    400] ;call this once.    
    (fn [] ;;everytime we update the component, we call this.
      (let [{:keys [weight height bmi]} (calc-bmi bmi-data)
            [color diagnose] (cond
                               (< bmi 18.5) ["orange" "underweight"]
                               (< bmi 25)   ["inherit" "normal"]
                               (< bmi 30)   ["orange" "overweight"]
                               :else ["red" "obese"])]
        [:div 
         [:h3 "BMI calculator"]
         
         [:div {:style {:width width}} 
          "Height: " (int height) "cm"
          [slider :height height 100 220 bmi-data]]
         [:div {:style {:width width}}
          "Weight: " (int weight) "kg"
          [slider :weight weight 30 150 bmi-data]]
         [:div {:style {:width width}}
          "BMI: " (int bmi) " "
          [:span {:style {:color color}} diagnose]
          [frozen-slider :bmi bmi 10 50 bmi-data]]]))))

;;let's try to generalize bmi-component.
;;we have a component that has 2 degrees of freedom.
;;similar to our x,y coords.
;;We have a function that computes bmi...

(defn bmi-component2 []
  (let [bmi-data (r/atom {:height 180 :weight 80})
        width    400 ;call this once.
        bmi->colored-diagnosis (fn [bmi]
                                 (cond
                                   (< bmi 18.5) ["orange" "underweight"]
                                   (< bmi 25)   ["inherit" "normal"]
                                   (< bmi 30)   ["orange" "overweight"]
                                   :else ["red" "obese"]))]
                  
    (fn [] ;;everytime we update the component, we call this.
      (let [{:keys [weight height bmi]} (calc-bmi bmi-data)
            [color diagnose] (bmi->colored-diagnosis bmi)]
        [:div 
         [:h3 "BMI calculator"]
         
         [:div {:style {:width width}} 
          "Height: " (int height) "cm"
          [slider :height height 100 220 bmi-data]]
         [:div {:style {:width width}}
          "Weight: " (int weight) "kg"
          [slider :weight weight 30 150 bmi-data]]
         [:div {:style {:width width :style color}}
          "BMI: " (int bmi) " "
          [:span {:style {:color color}} diagnose] ;;maps a color to a response.          
          [frozen-slider :bmi bmi 10 50 bmi-data]]]))))

(defn ^:export run []
  (r/render [bmi-component2]
            (js/document.getElementById "app")))


;;we'd like to define a custom slider that
;;allows us to define a functional relation between
;;variables, and reactively compute the response
;;as we muck with sliders.
;;We'd like to store the resulting values...

;; [function-slider [[:x 0 100]
;;                   [:y 0 100]]
;;  [:z [0 200]]
;;  (fn [x y] (+ x y))
;;  (r/atom {:x 50
;;           :y 50})
;;  :title "f(x) = x + y"]
 

;;creates a slider for input variables.
(defn variable [k x [lower upper] data]
  [:div
   (str (name k) ": "  x)
   [slider k x lower upper data]])

;;creates a reactive output slider (possibly elided).
(defn response [k y [lower upper] data]
  [:div
   (str (name k) ": "  y)
   [frozen-slider k y lower upper data]])

;;you have to provide a receptacle, or optionally
;;a function to jam...
(defn function-slider [var-ranges output-range f data & {:keys [title] :or {title "F(X)"}}]
  (let [;bmi-data (r/atom {:height 180 :weight 80})        
        width    400 ;call this once.
        ;;so, f should map inputs (gathered from the data) to output
        ;;as inputs change, we recompute (f inputs) and store the value
        ;;in output.  Output is updated to reflect new value.        
        inputs (map first var-ranges)
        [output bounds] output-range]                  
    (fn [] ;;everytime we update the component, we call this.
      (let [xs     (map (fn [k] (get @data k)) inputs)         
            result (apply f xs) ;;used to be calc-bmi
            [output bounds] output-range
            ]
        (into [:div {:style {:width width}}
               [:h3 title]]
              (concat (for [[k bounds] var-ranges]
                        [variable k (get @data k) bounds data])
                      [[response output result bounds data]
                       #_[:label result] ]))))))

                        
  
  
