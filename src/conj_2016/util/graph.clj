(ns conj-2016.util.graph
  "A simple namespace which produces svg/png renderings of coords
  and titles."
  (:require [dali.io :as d.io]
            [taoensso.timbre :as log]))

(def label-font-size 10)
(def min-page-dimension 100)
(def label-percentage-size 0.07)
(def page-border 10)
(def min-opacity 0.0)

(defn- label
  [title x y fill-options]
  [:text {:font-size label-font-size
          :x x
          :y y
          :fill (or (fill-options title)
                    :black)}
   (str title)])

(defn- page
  "For colour options see http://www.december.com/html/spec/colorsvg.html

  fill-options should be something that takes a title and returns what
  colour to fill the title with, or nil if you wish to default to black."
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

(defn graph-spit
  "Example:
  (svg-spit '/tmp/dali.demo' [[1 2] [3 4] [-1.5 3.1]] [:a :b :c] {:a :black :b :blue})

  Extension options are 'svg 'png"
  ([filename extension coords titles fill-options]
   (graph-spit filename extension (page coords titles fill-options)))
  ([filename extension svg]
   (log/info "spitting graph" :filename filename :extension extension)
   ((cond
      (= "svg" extension) d.io/render-svg
      (= "png" extension) d.io/render-png)
     svg (str filename "." extension))))

(defn- bound-0-1
  [^Double d]
  (cond
    (< 1.0 d) 1.0
    (> 0.0 d) 0.0
    :else d))

(defn- rgb-to-gradient
  [rgb1 rgb2 percentage]
  (let [scale (fn [p1 p2]
                (-> percentage
                    (* (Math/abs (- p1 p2)))
                    (+ p2)
                    Math/round))]
    (apply format "rgb(%d, %d, %d)"
           (map scale rgb1 rgb2))))

(defn- gradient-fill-fn
  "A simple helper which returns a fn which when given a title
  returns a string rgb(%d, %d, %d) value.

  This satisfies the graph-spit and page fill-options requirement."
  [coord-lookup gradient-dimension rgb1 rgb2]
  (let [target-coll (->> (vals coord-lookup)
                         (map #(nth % gradient-dimension)))
        min-val (apply min target-coll)
        adjusted-max-val (- (apply max target-coll)
                            min-val)]
    (fn [title]
      (let [alpha (-> title
                      coord-lookup
                      (nth gradient-dimension)
                      (- min-val)
                      (/ adjusted-max-val)
                      bound-0-1)]
        (rgb-to-gradient rgb1 rgb2 alpha)))))

(defn three-dimensional-graph-spit
  "Spit out three images based on the 3 dimensional coords passed in.

  If each coord is assumed to be [x y z],
  the first image is the x and y axes, the next xz, the last yz.

  The other dimension is used as a gradient value for some point between
  two colors (blue and green, pink and blue, etc.)

  Extension options are 'svg 'png"
  ([file-prefix coords titles]
   (three-dimensional-graph-spit
     file-prefix "svg" coords titles))
  ([file-prefix extension coords titles]
   (let [vcoords (map vec coords)
         coord-lookup (zipmap titles vcoords)
         xy_-fill-options (gradient-fill-fn coord-lookup 2 [255 0 0] [0 255 0])
         x_z-fill-options (gradient-fill-fn coord-lookup 1 [255 0 0] [0 0 255])
         _yz-fill-options (gradient-fill-fn coord-lookup 0 [0 255 0] [0 0 255])]
     (graph-spit (str file-prefix "--xy_")
                 extension
                 (map (fn [[x y _]]
                      [x y])
                    vcoords)
                 titles xy_-fill-options)
     (graph-spit (str file-prefix "--x_z")
                 extension
                 (map (fn [[x _ z]]
                      [x z])
                    vcoords)
                 titles x_z-fill-options)
     (graph-spit (str file-prefix "--_yz")
                 extension
                 (map (fn [[_ y z]]
                      [y z])
                    vcoords)
                 titles _yz-fill-options))))
