(ns conj-2016.core
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.random :as m.rnd]
            [clojure.java.io :as io]
            [clojure.spec :as s]
            [clojure.string :as string]
            [incanter.core :as i.core])
  (:import [java.util.zip GZIPInputStream]
           [mikera.matrixx Matrix]))

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

(defn rand-matrix
  [num-rows num-cols]
  (i.core/matrix (repeatedly num-rows (partial m.rnd/sample-normal num-cols))))

(defn one-matrix
  [num-rows num-cols]
  (m/add (m/zero-matrix num-rows num-cols) 1))

(defn matrices
  [^Matrix matrix]
  (let [nr (m/row-count matrix)
        rand-matrix (rand-matrix nr dimensions)
        dy (m/zero-matrix nr dimensions)
        iy dy
        gains (one-matrix nr dimensions)]))

(defn hbeta
  "Compute the perplexity and the P-row for a specific value of the precision of a Gaussian distribution."
  [^Matrix matrix beta]
  ;32 def Hbeta(D = Math.array([]), beta = 1.0):
  ;33     """Compute the perplexity and the P-row for a specific value of the precision of a Gaussian distribution."""
  ;34
  ;35     # Compute P-row and corresponding perplexity
  ;36     P = Math.exp(-D.copy() * beta);
  ;37     sumP = sum(P);
  ;38     H = Math.log(sumP) + beta * Math.sum(D * P) / sumP;
  ;39     P = P / sumP;
  ;40     return H, P;
  (let [p (-> matrix
              m/negate
              (m/mul beta)
              (m/pow Math/E))
        sum-p (m/esum p)
        h (+ (Math/log sum-p)
             (* beta
                (m/esum (m/mmul matrix p))
                (/ sum-p)))]
    [h (m/div p sum-p)]))

(defn row-but-diag
  "Return the nth row of the matrix except for the nth element"
  [^Matrix matrix n]
  ;>>> D = Math.array([[1, 2, 3],[4, 5, 6],[7, 8, 9]]);
  ;>>> D
  ;array ([[1, 2, 3],
  ;  [4, 5, 6],
  ;  [7, 8, 9]])
  ;>>> for i in range(0,3):
  ;...   print D[i, Math.concatenate((Math.r_[0:i], Math.r_[i+1:3]))];
  ;...
  ;[2 3]
  ;[4 6]
  ;[7 8]
  (let [r (m/get-row matrix n)]
    (i.core/matrix
     (concat (subvec r 0 n)
             (subvec r (inc n) (m/column-count matrix)))))
  (m/select matrix
            n
            (remove (partial = n)
                    (range (m/column-count matrix)))))

;43 def x2p(X = Math.array([]), tol = 1e-5, perplexity = 30.0):
;44     """Performs a binary search to get P-values in such a way that each conditional Gaussian has the same perplexity."""
;45

;54
;55     # Loop over all datapoints
;56     for i in range(n):
;57
;58         # Print progress
;59         if i % 500 == 0:
;60             print "Computing P-values for point ", i, " of ", n, "..."
;61
;62         # Compute the Gaussian kernel and entropy for the current precision
;63         betamin = -Math.inf;
;64         betamax =  Math.inf;
;65         Di = D[i, Math.concatenate((Math.r_[0:i], Math.r_[i+1:n]))];
;66         (H, thisP) = Hbeta(Di, beta[i]);
;67
;68         # Evaluate whether the perplexity is within tolerance
;69         Hdiff = H - logU;
;70         tries = 0;

;91
;92         # Set the final row of P
;93         P[i, Math.concatenate((Math.r_[0:i], Math.r_[i+1:n]))] = thisP;
;94
;95     # Return final P-matrix
;96     print "Mean value of sigma: ", Math.mean(Math.sqrt(1 / beta))
;97     return P;

(defn- calc-beta
  [])

(defn- while-iter
  [h-diff tol log-u]
  ;72
  ;73             # If not, increase or decrease precision
  ;74             if Hdiff > 0:
  ;75                 betamin = beta[i];
  ;76                 if betamax == Math.inf or betamax == -Math.inf:
  ;77                     beta[i] = beta[i] * 2;
  ;78                 else:
  ;79                     beta[i] = (beta[i] + betamax) / 2;
  ;80             else:
  ;81                 betamax = beta[i];
  ;82                 if betamin == Math.inf or betamin == -Math.inf:
  ;83                     beta[i] = beta[i] / 2;
  ;84                 else:
  ;85                     beta[i] = (beta[i] + betamin) / 2;
  ;86
  ;87             # Recompute the values
  ;88             (H, thisP) = Hbeta(Di, beta[i]);
  ;89             Hdiff = H - logU;
  ;90             tries = tries + 1;
  (loop [d-i nil
         h-diff h-diff
         this-p (i.core/matrix [] [])
         tries 0]
    ;71         while Math.abs(Hdiff) > tol and tries < 50:
    (if-not (and (< tol (i.core/abs h-diff))
                 (< tries 50))
      this-p
      (let [[h this-p] (hbeta d-i (calc-beta))]
        (recur nil
               (m/sub h log-u)
               this-p
               (inc tries))))))

(defn x2p
  "Performs a binary search to get P-values in such a way that each conditional Gaussian has the same perplexity."
  [^Matrix matrix tol perplexity]
  ;46     # Initialize some variables
  ;47     print "Computing pairwise distances..."
  ;48     (n, d) = X.shape;
  ;49     sum_X = Math.sum(Math.square(X), 1);
  ;50     D = Math.add(Math.add(-2 * Math.dot(X, X.T), sum_X).T, sum_X);
  ;51     P = Math.zeros((n, n));
  ;52     beta = Math.ones((n, 1));
  ;53     logU = Math.log(perplexity);
  (let [nr (m/row-count matrix)
        nc (m/column-count matrix)
        sum-x (->> matrix
                   m/square
                   (map m/esum))
        d (m/add (m/transpose (m/add (m/mul (m/dot matrix (m/transpose matrix)) -2)
                                     sum-x))
                 sum-x)
        p (m/zero-matrix nr nr)
        beta (one-matrix nr 1)
        log-u (i.core/log perplexity)]))

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
