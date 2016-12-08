(ns figdemo.bmi
  (:require [reagent.core :as r]
            [cljsjs.react-slider]))

;;Update: the assumably idiomitic way to do this in cljs.
(defn weak-fix [& input]
  (let [m (merge {:x 0 :y 0 :min 0 :max 0}
                    (apply hash-map input))]
    m))
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

(def bmi-data (r/atom {:height 180 :weight 80}))

(defn calc-bmi []
  (let [{:keys [height weight bmi] :as data} @bmi-data
        h (/ height 100)]
    (if (nil? bmi)
      (assoc data :bmi    (/ weight (* h h)))
      (assoc data :weight (* bmi h h)))))

;;Original
;;See if we can fix this to use react-range.
(defn slider [param value min max]
  [:input {:type "range" :value value :min min :max max
           :style {:width "100%"}
           :on-change (fn [e]
                        (swap! bmi-data assoc param (.-target.value e))
                        (when (not= param :bmi)
                          (swap! bmi-data assoc :bmi nil)))}])

(defn react-slider [param value min max]
 (->slider min
           max 
           :on-change (fn [e]
                        (swap! bmi-data assoc param (.-target.getValue e))
                        (when (not= param :bmi)
                          (swap! bmi-data assoc :bmi nil)))))
(defn bmi-component []
  (let [{:keys [weight height bmi]} (calc-bmi)
        [color diagnose] (cond
                          (< bmi 18.5) ["orange" "underweight"]
                          (< bmi 25) ["inherit" "normal"]
                          (< bmi 30) ["orange" "overweight"]
                          :else ["red" "obese"])]
    [:div
     [:h3 "BMI calculator"]
     [:div
      "Height: " (int height) "cm"
      [slider :height height 100 220]]
     [:div
      "Weight: " (int weight) "kg"
      [slider :weight weight 30 150]]
     [:div
      "BMI: " (int bmi) " "
      [:span {:style {:color color}} diagnose]
      [react-slider :bmi bmi 10 50]]]))

(defn ^:export run []
  (r/render [bmi-component]
            (js/document.getElementById "app")))
