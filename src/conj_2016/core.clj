(ns conj-2016.core
  (:require [clojure.core.matrix :as m]
            [clojure.java.io :as io]
            [clojure.spec :as s]
            [clojure.string :as string]
            [conj-2016.util.matrix :as u.m]
            [incanter.core :as i.core])
  (:import [java.util.zip GZIPInputStream]
           [mikera.matrixx Matrix]
           [mikera.vectorz AVector]))

;; super helpful incanter cheat sheet
;; http://incanter.org/docs/incanter-cheat-sheet.pdf

(def dimensions 2)

(defn gunzip
  [path]
  (with-open [in (GZIPInputStream.
                  (clojure.java.io/input-stream
                   path))]
    (slurp in)))

(defn parse-embeddings
  [path]
  (let [unzipped (gunzip path)
        lines (string/split-lines unzipped)
        line-parser (fn [v l]
                      (let [[c & nums] (string/split l #" ")]
                        (assert (not (contains? v c))
                                (format "%s already in parsed embeddings. Duplicates are not allowed"
                                        c))
                        (assert (or (empty? v)
                                    (= (-> v vals first count)
                                       (count nums)))
                                (format "Width of %d is different from required width of %d for %s"
                                        (count nums) (-> v vals first count) c))
                        (assoc v
                          c (map read-string nums))))]
    (reduce line-parser {} lines)))

(def inp (parse-embeddings (io/resource "english-embeddings.turian.txt.gz")))

(defn init-matrix
  "Given the output of parse-embeddings, initialize a matrix. Output is an array, where
  the first element is a list of strings and the second is a matrix. The list of strings
  correlate to the rows in the matrix, where each string is a key from parsed-embeddings"
  [parsed-embeddings]
  (let [vecs (map identity parsed-embeddings)]
    [(map first vecs)
     (i.core/matrix (map second inp))]))

(defn matrices
  [^Matrix matrix]
  (let [nr (m/row-count matrix)
        rand-matrix (u.m/rand-matrix nr dimensions)
        dy (m/zero-matrix nr dimensions)
        iy dy
        gains (u.m/one-matrix nr dimensions)]))

(defn hbeta
  "Compute the perplexity and the P-row for a specific value of the precision of a Gaussian distribution."
  [^AVector vector_n beta]
  ;; Note: using hungarian notation for dimensions

  ;32 def Hbeta(D = Math.array([]), beta = 1.0):
  ;33     """Compute the perplexity and the P-row for a specific value of the precision of a Gaussian distribution."""
  ;34
  ;35     # Compute P-row and corresponding perplexity
  ;36     P = Math.exp(-D.copy() * beta);
  ;37     sumP = sum(P);
  ;38     H = Math.log(sumP) + beta * Math.sum(D * P) / sumP;
  ;39     P = P / sumP;
  ;40     return H, P;
  (let [p_n (-> vector_n
                m/negate
                (m/mul beta)
                u.m/exp)
        sum-p (m/esum p_n)
        sum-dp (m/esum (m/mul vector_n p_n))
        h (+ (i.core/log sum-p)
             (/ (.doubleValue (* beta sum-dp))
                (.doubleValue sum-p)))]
    [h
     (m/div p_n sum-p)]))

(defn- update-beta
  [^Double betamin ^Double betamax ^Double beta ^Double diff]
  ;73 # If not, increase or decrease precision
  ;74 if Hdiff > 0:
  ;75     betamin = beta[i];
  ;76     if betamax == Math.inf or betamax == -Math.inf:
  ;77         beta[i] = beta[i] * 2;
  ;78     else:
  ;79         beta[i] = (beta[i] + betamax) / 2;
  ;80 else:
  ;81     betamax = beta[i];
  ;82     if betamin == Math.inf or betamin == -Math.inf:
  ;83         beta[i] = beta[i] / 2;
  ;84     else:
  ;85         beta[i] = (beta[i] + betamin) / 2;
  (if (pos? diff)
    [beta
     betamax
     (if (.isInfinite betamax)
       (* 2.0 beta)
       (/ (+ beta betamax) 2.0))]
    [betamin
     beta
     (if (.isInfinite betamin)
       (/ beta 2.0)
       (/ (+ beta betamin) 2.0))]))

(defn- x2p-iter
  [log-u tolerance idx ^AVector di_n]
  ;62 # Compute the Gaussian kernel and entropy for the current precision
  ;63 betamin = -Math.inf;
  ;64 betamax =  Math.inf;
  ;65 Di = D[i, Math.concatenate((Math.r_[0:i], Math.r_[i+1:n]))];
  (let [di_n-1 (u.m/but-n di_n idx)]
    ;68 # Evaluate whether the perplexity is within tolerance
    ;69 Hdiff = H - logU;
    ;52 beta = Math.ones((n, 1));
    ;70 tries = 0;
    ;71 while Math.abs(Hdiff) > tol and tries < 50:
    (loop [betamin Double/NEGATIVE_INFINITY
           betamax Double/POSITIVE_INFINITY
           beta 1
           n 0]
      ;66 (H, thisP) = Hbeta(Di, beta[i]);
      ;...
      ;87   # Recompute the values
      ;88   (H, thisP) = Hbeta(Di, beta[i]);
      ;89   Hdiff = H - logU;
      ;90   tries = tries + 1;
      ;(println idx n)
      ;(println "hbeta" di_n-1 beta)
      ;(println betamin betamax)
      (let [[h this-p_n-1] (hbeta di_n-1 beta)
            h-logu (- h log-u)]
        ;(println h-logu tolerance)
        ;(println "---" h this-p_n-1)
        (if (not (and (not= n 50)
                      (< tolerance (Math/abs ^Double h-logu))))
          ;51 P = Math.zeros((n, n));
          ;93 P[i, Math.concatenate((Math.r_[0:i], Math.r_[i+1:n]))] = thisP;
          (u.m/conj-idx this-p_n-1 0 idx)
          (let [[r-betamin
                 r-betamax
                 r-beta]
                (update-beta betamin betamax beta h-logu)]
            (recur r-betamin
                   r-betamax
                   r-beta
                   (inc n))))))))

(defn x2p
  "Performs a binary search to get P-values in such a way that each conditional Gaussian has the same perplexity."
  [^Matrix matrix_n_m tolerance perplexity]
  ;46     # Initialize some variables
  ;47     print "Computing pairwise distances..."
  ;48     (n, d) = X.shape;
  ;49     sum_X = Math.sum(Math.square(X), 1);
  ;50     D = Math.add(Math.add(-2 * Math.dot(X, X.T), sum_X).T, sum_X);
  ;53     logU = Math.log(perplexity);
  (let [sum-x_n (m/slice-map (comp m/esum m/square)
                             matrix_n_m)
        d_n_n (m/add (m/transpose (m/add (m/mul (m/mmul matrix_n_m
                                                        (m/transpose matrix_n_m))
                                                -2)
                                         sum-x_n))
                     sum-x_n)
        log-u (i.core/log perplexity)]
    ;55     # Loop over all datapoints
    ;56     for i in range(n):
    ; ...
    ;95     # Return final P-matrix
    ;96     print "Mean value of sigma: ", Math.mean(Math.sqrt(1 / beta))
    ;97     return P;
    (i.core/matrix
      (pmap (partial x2p-iter log-u tolerance)
            (range)
            d_n_n))))

(defn p-values
  [^Matrix matrix perplexity]
  ;137     # Compute P-values
  ;138     P = x2p(X, 1e-5, perplexity);
  ;139     P = P + Math.transpose(P);
  ;140     P = P / Math.sum(P);
  ;141     P = P * 4;                                    # early exaggeration
  ;142     P = Math.maximum(P, 1e-12);
  (as-> (x2p matrix 1e-5 perplexity) p
        (m/add p (m/transpose p))
        (m/div p (m/esum p))
        (m/mul p 4)
        (m/clamp p 1e-12 Double/POSITIVE_INFINITY)))

(defn calc--values
  [^Matrix matrix]

  )
