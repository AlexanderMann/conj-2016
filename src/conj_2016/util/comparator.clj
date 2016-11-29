(ns conj-2016.util.comparator
  (:require [clojure.core.matrix :as m]))

(defn dimensions-match?
  [matrix num-rows num-cols]
  (and (= num-rows (m/row-count matrix))
       (= num-cols (m/column-count matrix))))

(defn non-numeric-number?
  "Returns true when nil, or is a double which is NaN/+-Inf"
  [obj]
  (when (or (nil? obj)
            (double? obj))
    (or (nil? obj)
        (.isNaN obj)
        (.isInfinite obj))))

(defn range-intersect?
  [[r0 r1] [r2 r3] num-tolerance]
  (and (<= r0 r3)
       (or (<= r2 r1)
           (< (Math/abs (- r2 r1)) num-tolerance))))

(def fuzzy=-implementation-issue (atom nil))

(defn fuzzy=
  "Diff doesn't do a great job at helping us with numbers that are realllllyyy close.
  When sending values through the shell to Python and diffing them against Matrix
  multiplication values in the JVM...numerical percision becomes signicant."
  [a b & {:keys [num-tolerance
                 relative?
                 bracket-by-ulp?
                 ulp-modifier]
          :or {num-tolerance 0
               relative? false
               bracket-by-ulp? false
               ulp-modifier identity}
          :as opts}]
  (cond
    (= a b) true

    (and (non-numeric-number? a)
         (non-numeric-number? b))
    true

    (and (number? a)
         (number? b))
    (or (and bracket-by-ulp?
             (let [a-ulp (ulp-modifier (Math/ulp a))
                   b-ulp (ulp-modifier (Math/ulp b))
                   a-range [(- a a-ulp) (+ a a-ulp)]
                   b-range [(- b b-ulp) (+ b b-ulp)]]
               (or (range-intersect? a-range b-range num-tolerance)
                   (range-intersect? b-range a-range num-tolerance))))
        (and (let [d (Math/abs (- a b))]
               (if relative?
                 (< (->> (min a b)
                         Math/abs
                         (max Double/MIN_VALUE)
                         (/ d))
                    num-tolerance)
                 (< d num-tolerance)))))

    (and (seqable? a)
         (not (map? a))
         (seqable? b)
         (not (map? b)))
    (every? identity
            (map #(fuzzy= %1 %2
                          :num-tolerance num-tolerance
                          :relative? relative?
                          :bracket-by-ulp? bracket-by-ulp?
                          :ulp-modifier ulp-modifier)
                 a
                 b))

    :else (do (reset! fuzzy=-implementation-issue
                      {:a a
                       :b b
                       :opts opts})
              false)))
