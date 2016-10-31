(ns conj-2016.util.graph
  (:require [dali.io :as d.io]
            [taoensso.timbre :as log]))

(def label-font-size 5)
(def label-percentage-size 0.1)
(def page-border 10)

(defn label
  [title x y]
  [:text {:font-size label-font-size
          :x x
          :y y
          :fill :black}
   title])

(defn page
  [coords titles]
  (let [xs (map first coords)
        min-x (apply min xs)
        max-x (apply max xs)
        ys (map second coords)
        min-y (apply min ys)
        max-y (apply max ys)
        min-diff (min (Math/abs (- max-x min-x)) (Math/abs (- max-y min-y)))
        scale (/ (max min-diff (/ label-font-size label-percentage-size))
                 min-diff)
        adjust-coord (fn [c min-c]
                       (-> c
                           (- min-c)
                           (* scale)
                           (+ page-border)))
        adjusted-coords (map (fn [[x y]]
                               [(adjust-coord x min-x)
                                (adjust-coord y min-y)])
                             coords)]
    (apply vector
           :dali/page
           (map (fn [title [x y]]
                  (label title x y))
                titles
                adjusted-coords))))

(comment
  (try
    (d.io/render-svg (page [[1 2] [3 4] [-1.5 3.1]] ["a" "b" "c"]) "/tmp/dali.demo.svg")
    (catch Exception e
      (log/error e))))
