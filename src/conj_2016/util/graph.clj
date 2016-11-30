(ns conj-2016.util.graph
  (:require [dali.io :as d.io]
            [taoensso.timbre :as log]))

(def label-font-size 5)
(def min-page-dimension 100)
(def label-percentage-size 0.1)
(def page-border 10)

(defn label
  [title x y fill-options]
  [:text {:font-size label-font-size
          :x x
          :y y
          :fill (get fill-options title :black)}
   (str title)])

(defn page
  "For colour options see http://www.december.com/html/spec/colorsvg.html"
  [coords titles fill-options]
  (let [xs (map first coords)
        min-x (apply min xs)
        max-x (apply max xs)
        ys (map second coords)
        min-y (apply min ys)
        max-y (apply max ys)
        max-label-length (->> titles
                              (map (comp count str))
                              (apply max))
        min-diff (min (Math/abs (- max-x min-x))
                      (Math/abs (- max-y min-y)))
        scale (/ (max min-diff
                      (/ (* max-label-length label-font-size)
                         label-percentage-size))
                 min-diff)
        adjust-coord (fn [c min-c]
                       (-> c
                           (- min-c)
                           (* scale)
                           (+ page-border)))
        adjusted-coords (->> coords
                             (map vec)
                             (map (fn [[x y]]
                                    [(adjust-coord x min-x)
                                     (adjust-coord y min-y)])))]
    (apply vector
           :dali/page
           (map (fn [title [x y]]
                  (label title x y fill-options))
                titles
                adjusted-coords))))

(defn svg-spit
  "Example:
  (svg-spit '/tmp/dali.demo.svg' [[1 2] [3 4] [-1.5 3.1]] [:a :b :c] {:a :black :b :blue})"
  ([file coords titles fill-options]
   (svg-spit file (page coords titles fill-options)))
  ([file svg]
   (d.io/render-svg svg file)))
