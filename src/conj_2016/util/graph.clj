(ns conj-2016.util.graph
  (:require [dali.io :as d.io]))

(def circle-radius 1)
(def circle-stroke-width 1)
(def label-font-size 5)

(defn label
  [title]
  [:dali/place {:relative-to [(keyword title) :bottom]
                :anchor :bottom
                :offset [0 -2]}
   [:text
    {:font-size label-font-size :stroke :black :fill :black}
    title]])

(defn labelled-circle
  [title x y]
  [[:circle {:id (keyword title)
             :cx x :cy y
             :r circle-radius
             :stroke {:width circle-stroke-width}}]
   (label title)])

(defn page
  [coords titles]
  [:dali/page
   (reduce concat
           []
           (map (fn [title [x y]]
                  (labelled-circle title x y))
                titles
                coords))])
