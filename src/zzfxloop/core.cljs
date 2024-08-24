(ns zzfxloop.core
  (:require
    [reagent.dom :as rdom]
    [promesa.core :as p]
    ["itwriter$default" :as itwriter]
    ["zzfx/ZzFX" :refer [ZZFX]]))

(defn download-file [file]
  (let [url (js/URL.createObjectURL file)
        link (js/document.createElement "a")]
    (aset link "href" url)
    (aset link "download" (aget file "name"))
    (js/document.body.appendChild link)
    (.click link)
    (js/document.body.removeChild link)
    (js/URL.revokeObjectURL url)))

(defn build-random-sound [length-scale volume randomness]
  (let [R #(js/Math.random)
        C #(if (< (R) 0.5) (R) 0)
        S #(if (C) 1 -1)
        attack (* (js/Math.pow (R) 3) 0.5 length-scale)
        decay (* (js/Math.pow (R) 3) 0.5 length-scale)
        sustain (* (js/Math.pow (R) 3) 0.5 length-scale)
        release (* (js/Math.pow (R) 3) 0.5 length-scale)
        length (+ attack decay sustain release)
        filter (if (C)
                 0
                 (if (< (R) 0.5)
                   (+ 99 (* (js/Math.pow (R) 2) 900))
                   (- (* (js/Math.pow (R) 2) 1000) 1500)))]
    (clj->js
      [volume
       randomness
       (+ 9 (* (js/Math.pow (R) 2) 1000))
       attack
       sustain
       release
       (js/Math.floor (* (R) 5))
       (* (R) 5)
       (* (js/Math.pow (C) 3) 99 (S))
       (* (js/Math.pow (C) 3) 99 (S))
       (* (js/Math.pow (C) 2) 500 (S))
       (* (js/Math.pow (R) 2) length)
       (* (C) (/ length 4))
       (js/Math.pow (C) 4)
       (* (R) (js/Math.pow (C) 2) 500)
       (js/Math.pow (C) 4)
       (* (js/Math.pow (C) 3) 0.5)
       (- 1 (* (R) 0.5))
       decay
       (* (js/Math.pow (C) 2) 0.5)
       filter])))

(defn generate-impulse-tracker-file
  []
  (p/catch
    (p/let [;beat (:beat *state)
            ;samples (:samples beat)
            sound-params (build-random-sound 1 0.5 0.05)
            sample (.apply ZZFX.buildSamples ZZFX sound-params)
      it-struct (clj->js
                  {:title "zzfxloop"
                   :samples [{:name "random A"
                              :channels [sample]}]})
      it-file-buffer (itwriter (clj->js it-struct))]
      (js/File. #js [it-file-buffer]
                (str "rando" ".it")
                #js {:type "audio/x-it"}))
    (fn [err] (js/console.error err) (throw err))))

(defn generate-and-play-sound []
  (let [sound-params (build-random-sound 1 0.5 0.05)
        samples (.apply ZZFX.buildSamples ZZFX sound-params)]
    (js/console.log "sound-params" sound-params)
    (js/console.log "samples" samples)
    (.playSamples ZZFX samples)))

(defn sound-generator []
  [:div {:style {:text-align "center" :padding "20px"}}
   [:h1 "Random Sound Generator"]
   [:button {:on-click generate-and-play-sound
             :style {:font-size "1.2em" :padding "10px 20px"}}
    "Generate Sound!"]
   [:button {:on-click #(p/let [it-file (generate-impulse-tracker-file)]
                          (js/console.log it-file)
                          (download-file it-file))}
    "Generate IT file"]])

(defn start {:dev/after-load true} []
  (rdom/render [sound-generator]
               (.getElementById js/document "app")))

(defn init []
  (start))
