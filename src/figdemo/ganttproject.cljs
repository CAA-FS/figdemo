;;A namespace for reading in
;;GanttProject csv export files.
(ns figdemo.ganttproject
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as async]
            [goog.dom :as dom]
            [goog.events :as events]
            [figdemo.io :as io]))

;;GanttProject files have a specific schema:

(def schemas  
  {:project [:name :string ;"MARATHON Project Management"
             :company :string; "CAA"
             :webLink  :string ;"http://"
             :view-date :date ;"2015-07-01"
             :view-index :long ;"0"
             :gantt-divider-location :long ;"647"
             :resource-divider-location :long ;"528"
             :version :string ;"2.8.1"
             :locale :string ;"en_US"
             ]
   :tasks [:empty-milestones :boolean];"true"
   ;;these are typically default...nothing special
   :taskproperties [:id   :string ;"tpd0"
                    :name :string ;"type"
                    :type :string ;"default"
                    :valuetype :string ;"icon"
                    ]
   :task  [:id :string 
           :name :string 
           :meeting :boolean
           :start :date ;"2017-09-07"
           :duration :long 
           :complete :long 
           :expand :boolean]
   :depend [:id :string ;"5104"
            :type :long ;"2"
            :difference :long ;"0"
            :hardness :string ;"Strong"
            ]
   :taskproperty   [:id :string
                    :name :string
                    :type :string
                    :valuetype :string]
   :resource [:id :string ;"0"
              :name :string ;"Tom Spoon"
              :function :string ;"1"
              :contacts :string; ""
              :phone :string;""
              ]
   :allocation [:task-id :string; "3"
                :resource-id :string ;"0"
                :function :string ;"Default:0"
                :responsible :boolean  ;"true"
                :load :float ;"100.0"
                ]
   :role [:id  :string ;"1"
          :name :string ;"Delusional Architect"
          ]
   }
)
   
;;project
;;   
;;  calendars
;;    day-types
;;      day-type*   
;;  tasks
;;    taskproperties
;;    task* 
;;      depend*
;;      task*

;;resources
;;roles
;;allocations
;;vacations
;;previous
;;roles
;;  role*

;;we can import/export from csv...
;;This is fine for reading from
;;GanttProject, but we lose information
(def csv-schema
  {:tasks   ["ID" "string"
             "Name" "string" 
             "Begin date" "date"
             "End date"  "date" 
             "Duration" "number"
             "Completion" "number"
             "Cost" "number" 
             "Coordinator" "string"
             "Predecessors" "string"
             "Outline number" "number"
             "Resources" "string"
             "Web Link" "string"
             "Notes" "string"]
   :resources  ["ID"   "string" 
                "Name" "string"
                "Default" "string"
                "role" "string"
                "e-mail" "string"
                "Phone" "string"
                "Assignment role" "string"
                "Standard rate" "string"]})
;;note: the gantt-project csv file
;;puts both tables in the same csv...
;;so you get tasks and resources, separated
;;by 2 newlines.

