(ns zzfxloop.core
  (:require
    [reagent.core :as r]
    [reagent.dom :as rdom]
    [promesa.core :as p]
    ["itwriter$default" :as itwriter]
    ["zzfx/ZzFX" :refer [ZZFX]]))

(def rnd js/Math.random)

(defn rnd-el [array]
  (nth array (js/Math.floor (* (rnd) (count array)))))

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
  (let [R #(rnd)
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

(def probability-tables
  [[0.1 0.3 0.1 0.2  ; snare like
    0.9 0.1 0.1 0.3]
   [0.9 0.1 0.5 0.2  ; bassdrum style
    0.1 0.2 0.2 0.2]
   [0.7 0.3 0.7 0.3  ; hat style
    0.7 0.3 0.7 0.3]
   [0.5 0.1 0.1 0.5  ; triplets style
    0.1 0.1 0.5 0.1]])

(defn build-rhythm []
  (let [table (rnd-el probability-tables)]
    (map
      #(if (< (rnd) (nth table (mod % (count table))))
         1 0)
      (range 32))))

#_ (defn build-rhythm []
  (let [new-rate #(inc (js/Math.floor (* (rnd) 4)))
        lookup-table
        (repeatedly
          4
          #(if (> (rnd) 0.5) 0.95 0.05))
        initial-rate (new-rate)]
    (loop [rate initial-rate rhythm []]
      (let [pos (count rhythm)
            lookup (mod (js/Math.floor (/ pos rate)) 4)
            prob (nth lookup-table lookup)
            rhythm (concat rhythm [(if (> (rnd) prob) 1 0)] (repeat (dec rate) 0))
            rate (if (> (rnd) 0.5) (new-rate) rate)]
        (if (< (count rhythm) 16)
          (recur rate rhythm)
          (take 16 rhythm))))))

(defn generate-impulse-tracker-file
  [*state]
  (p/catch
    (p/let [;beat (:beat *state)
            ;samples (:samples beat)
            samples (map (fn [sound]
                           (let [sample-data
                                 (.apply ZZFX.buildSamples
                                         ZZFX (:sound-params sound))
                                 sound-name (str "sound-"
                                                 (rnd-el "ABCDEFGHIJKLMNOPQR"))]
                             {:name sound-name
                              :filename (str sound-name ".wav")
                              :samplerate 44100
                              :channels [sample-data]}))
                         (:sounds *state))
            sequences (map :sequence (:sounds *state))
            pattern (->> (map
                           (fn [i]
                             (let [note
                                   ; find the last channel that matches
                                   (->>
                                     (map
                                       #(when (= (nth (nth sequences %) i) 1)
                                          {:note "C-5"
                                           :instrument %
                                           :vol "v64"})
                                       (range 4))
                                     (remove nil?)
                                     last)]
                               (when note
                                 {i note})))
                           (range 32))
                         (apply merge))
            it-struct (clj->js
                        {:title "zzfxloop"
                         :samples samples
                         :order [0]
                         :message "Generated with zzfxloop"
                         :patterns [{:rows 32
                                     :channels [pattern]}]})
            it-file-buffer (itwriter (clj->js it-struct))]
      (js/File. #js [it-file-buffer]
                (str "zzfxloop" ".it")
                #js {:type "audio/x-it"}))
    (fn [err] (js/console.error err) (throw err))))

(defn play-sound [sound-params]
  (->> (.apply ZZFX.buildSamples ZZFX sound-params)
       (.playSamples ZZFX)))

(defn sound-generator [state]
  [:div {:style {:text-align "center" :padding "20px"}}
   [:h1 "blipseq."]
   [:ul
    (for [si (range (count (:sounds @state)))]
      (let [s (nth (:sounds @state) si)]
        [:li {:key si}
         [:button {:on-click #(swap! state assoc-in [:sounds si :sound-params]
                                     (build-random-sound 1 0.5 0.05))} "gen"]
         [:button {:on-click #(play-sound (:sound-params s))} "play"]
         (for [r (:sequence s)]
           (if (> r 0.5) "X" "0"))]))]
   [:button {:on-click #(p/let [it-file (generate-impulse-tracker-file @state)]
                          (download-file it-file))}
    "Download IT file"]])

(defonce state
  (r/atom
    {:sounds
     (vec
       (repeatedly
         4
         (fn []
           {:sound-params (build-random-sound 1 0.5 0.05)
            :sequence (build-rhythm)})))}))

(defn start {:dev/after-load true} []
  (rdom/render [sound-generator state]
               (.getElementById js/document "app")))

(defn init []
  (start))
