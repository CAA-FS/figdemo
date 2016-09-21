(ns figdemo.io
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [figdemo.util :as util]
            [cljs.core.async :as async]
            [goog.dom :as dom]
            [goog.events :as events]))

;;note: clojurescript support for the file api
;;is pretty much non-existent at the moment,
;;thus we role our own stuff....

;;so, we need to use array-seq to coerce array-like
;;returns like FileList into seq-compatible stuff.
(defn input->files [& {:keys [el] :or {el "file"}}]
  (->> (dom/getElement el)
       (.-files)
       (array-seq)
       ))

(defn current-file [] (first (input->files)))

(defn input->filenames [& {:keys [el] :or {el "file"}}]
  (->> (input->files :el el)
       (map (fn [x] (.-name x)))))

;;open
(defn ->file-reader [on-load]
  (let [rdr (js/FileReader.)
        _ (set! (.-onload rdr) on-load)
        ]
    rdr))

(defn read-file! [rdr fl]
  (. rdr (readAsText fl)))


;;this isn't exactly what I wanted...
;;I really prefer having a channel to process.
;;I'm still thinking in terms of synchronous io
;;which is utterly incompatible with js/cljs, so
;;we fake it by doing everything via core/async.
;;the downside is, we lose some of the liveness
;;of the repl, i.e. waiting for results doesn't
;;work.  everything happens indirectly....we have
;;to sit back and watch our application - expressed
;;neatly in core/async channel composition -
;;unfold in response to input.
(defn file->lines [fl]
  (let [the-result (atom nil)
        rdr (->file-reader
             (fn [_]
               (this-as this
                  (let [txt (.-result this)
                       ; _ (log! txt)
                        ]
                   ; (async/put! res txt)
                    (reset! the-result txt)))))
        _   (read-file! rdr fl)
        ]
    the-result))

(defn file->lines!! [fl]
  (let [result (async/chan)
        rdr (->file-reader
             (fn [_]
               (this-as this
                  (let [txt (.-result this)
                       ; _ (log! txt)
                        ]
                    (go                     
                      (async/put! result txt)
                      (async/close! result)
                      )))))
        _  (read-file! rdr fl)]
    result))

