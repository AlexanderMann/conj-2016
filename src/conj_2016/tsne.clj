(ns conj-2016.tsne
  (:require [clojure.core.matrix :as m]
            [clojure.core.memoize :as memo]
            [conj-2016.util.matrix :as u.m]
            [incanter.core :as i.core]
            [taoensso.timbre :as log]
            [taoensso.tufte :refer [p]])
  (:import [mikera.matrixx Matrix]
           [mikera.vectorz AVector]))

(set! *warn-on-reflection* true)

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
  (p :hbeta
     (let [p_n (-> vector_n
                   m/negate
                   (m/mul beta)
                   u.m/exp)
           sum-p (m/esum p_n)
           sum-dp (m/esum (m/mul vector_n p_n))
           h (+ (i.core/log sum-p)
                (/ (double (* beta sum-dp))
                   (double sum-p)))]
       ;(log/info p_n)
       ;(log/info sum-p)
       ;(log/info sum-dp)
       [h
        (m/div p_n sum-p)])))

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
  ;(log/info {:min betamin :max betamax :beta beta :diff diff})
  (p :update-beta
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
          (/ (+ beta betamin) 2.0))])))

(defn- x2p-iter
  [log-u tolerance idx ^AVector di_n]
  ;62 # Compute the Gaussian kernel and entropy for the current precision
  ;63 betamin = -Math.inf;
  ;64 betamax =  Math.inf;
  ;65 Di = D[i, Math.concatenate((Math.r_[0:i], Math.r_[i+1:n]))];
  (p :x2p-iter
     (let [di_n-1 (u.m/but-n di_n idx)]
       ;68 # Evaluate whether the perplexity is within tolerance
       ;69 Hdiff = H - logU;
       ;52 beta = Math.ones((n, 1));
       ;70 tries = 0;
       ;71 while Math.abs(Hdiff) > tol and tries < 50:
       (loop [betamin Double/NEGATIVE_INFINITY
              betamax Double/POSITIVE_INFINITY
              beta 1.0
              n 0]
         ;66 (H, thisP) = Hbeta(Di, beta[i]);
         ;...
         ;87   # Recompute the values
         ;88   (H, thisP) = Hbeta(Di, beta[i]);
         ;89   Hdiff = H - logU;
         ;90   tries = tries + 1;
         (let [[h this-p_n-1] (hbeta di_n-1 beta)
               h-logu (- h log-u)]
           (if (not (and (not= n 50)
                         (< tolerance (Math/abs ^Double h-logu))))
             ;51 P = Math.zeros((n, n));
             ;93 P[i, Math.concatenate((Math.r_[0:i], Math.r_[i+1:n]))] = thisP;
             (u.m/conj-idx this-p_n-1 0 idx)
             (let [[r-betamin
                    r-betamax
                    r-beta]
                   (update-beta betamin betamax beta h-logu)]
               (recur (double r-betamin)
                      (double r-betamax)
                      (double r-beta)
                      (inc n)))))))))

(defn x2p
  "Performs a binary search to get P-values in such a way that each conditional Gaussian has the same perplexity."
  [^Matrix matrix_n_m tolerance perplexity]
  ;46     # Initialize some variables
  ; ...
  ;53     logU = Math.log(perplexity);
  (p :x2p
     (let [d_n_n (u.m/pairwise-distances matrix_n_m)
           log-u (i.core/log perplexity)]
       ;55     # Loop over all datapoints
       ;56     for i in range(n):
       ; ...
       ;95     # Return final P-matrix
       ;96     print "Mean value of sigma: ", Math.mean(Math.sqrt(1 / beta))
       ;97     return P;
       (i.core/matrix
         (map-indexed (partial x2p-iter log-u tolerance)
                      d_n_n)))))

(defn- p-values*
  [^Matrix matrix_n_m perplexity exaggeration]
  ;137 # Compute P-values
  ;138 P = x2p(X, 1e-5, perplexity);
  ;139 P = P + Math.transpose(P);
  ;140 P = P / Math.sum(P);
  ;141 P = P * 4;                    # early exaggeration
  ;142 P = Math.maximum(P, 1e-12);
  ; ...
  ;175 # Stop lying about P-values
  ;176 if iter == 100:
  ;177     P = P / 4;
  (p :p-values*
     (doto (let [r-matrix_n_n (x2p matrix_n_m 1E-5 perplexity)]
             (m/add r-matrix_n_n (m/transpose r-matrix_n_n)))
       u.m/normalize!
       (u.m/clamp! 2.5E-13)
       (m/mul! exaggeration))))

(def p-values (memo/fifo p-values*
                         ;; It _should_ be the case that there will only
                         ;; ever be _two_ calls to this fn from within
                         ;; tsne which vary. One with an exaggeration,
                         ;; and one without.
                         :fifo/threshold 2))

(defn- gradient-iter
  [^Matrix matrix_n_m ^AVector matrix-row_m ^AVector col_n]
  ;157 dY[i,:] = Math.sum(Math.tile( ... , (no_dims, 1)).T * (Y[i,:] - Y), 0);
  (p :gradient-iter
     (let [matrix-diff_m_n
           (p :gradient-iter/matrix-diff_n-dims_n
                (m/transpose (m/sub matrix-row_m matrix_n_m)))]
       (p :gradient-iter/gradient-row_n-dims
          (m/slice-map m/esum
                       (m/mul! matrix-diff_m_n col_n))))))

(defn gradient
  [^Matrix matrix_n_m ^Matrix pvalues_n_n]
  ;147 # Compute pairwise affinities
  ;149 num = 1 / (1 + ...pairwise...);
  ;151 Q = num / Math.sum(num);
  ;152 Q = Math.maximum(Q, 1e-12);
  ;154 # Compute gradient
  ;155 PQ = P - Q;
  (p :gradient
     (let [num_n_n (p :gradient/num_n_n
                      (doto (u.m/pairwise-distances matrix_n_m)
                        (m/add! 1.0)
                        m/div!
                        ;150 num[range(n), range(n)] = 0;
                        (u.m/set!-diag 0.0)))
           pv_n_n (p :gradient/pv_n_n
                     (doto (m/clone num_n_n)
                       u.m/normalize!
                       (u.m/clamp! 1E-12)
                       m/negate!
                       (m/add! pvalues_n_n)
                       ;157 ... PQ[:,i] * num[:,i] ...
                       (m/mul! num_n_n)))]
       ;156 for i in range(n):
       ;157   dY[i,:] = ...
       (i.core/matrix
         (map (partial gradient-iter matrix_n_m)
              matrix_n_m
              (m/columns pv_n_n))))))

(defn update!-gains
  [^Matrix gains_n_m ^Matrix gradient_n_m ^Matrix intermediate_n_m]
  ;164 gains = (gains + 0.2) * ((dY > 0) != (iY > 0)) + (gains * 0.8) * ((dY > 0) == (iY > 0));
  (p :update!-gains
     (doseq [n (range (m/row-count gains_n_m))
             m (range (m/column-count gains_n_m))]
       (let [gain (m/mget gains_n_m n m)
             grad (m/mget gradient_n_m n m)
             intr (m/mget intermediate_n_m n m)]
         (m/mset! gains_n_m n m
                  (max (if (= (pos? grad)
                              (pos? intr))
                         (* 0.8 gain)
                         (+ 0.2 gain))
                       ;131 min_gain = 0.01;
                       ;165 gains[gains < min_gain] = min_gain;
                       0.01))))
     gains_n_m))

(defn tsne
  "Runs t-SNE on the dataset in the NxD array X to reduce its
  dimensionality to no_dims dimensions.

  NOTE: the variadic call here is purely for testability. The
  recommened call is 3 or 4 arg form."
  ([^Matrix matrix_n_m n-dims perplexity]
    ;; 3 arg
   (tsne matrix_n_m n-dims perplexity (u.m/rand-matrix (m/row-count matrix_n_m) n-dims) 1000))
  ([^Matrix matrix_n_m n-dims perplexity iterations]
    ;; 4 arg
   (tsne matrix_n_m n-dims perplexity (u.m/rand-matrix (m/row-count matrix_n_m) n-dims) iterations))
  ([^Matrix matrix_n_m n-dims _ perplexity _ ^Matrix rand-matrix_n_n-dims]
    ;; 6 arg, necessary for testing parity with Python
   (tsne matrix_n_m n-dims perplexity rand-matrix_n_n-dims 1000))
  ([^Matrix matrix_n_m n-dims perplexity ^Matrix rand_n_n-dims iterations]
    ;; 5 arg
    ;126 (n, d) = X.shape;
    ;127 max_iter = 1000;
    ;128 initial_momentum = 0.5;
    ;129 final_momentum = 0.8;
    ;130 eta = 500;
    ;132 Y = Math.random.randn(n, no_dims);
    ;133 dY = Math.zeros((n, no_dims));
    ;134 iY = Math.zeros((n, no_dims));
    ;135 gains = Math.ones((n, no_dims));
   (p :tsne
      (let [n (m/row-count matrix_n_m)
            reduced_n_n-dims (m/clone rand_n_n-dims)
            intermediate-reduced_n_n-dims (m/zero-matrix n n-dims)
            gains_n_n-dims (u.m/one-matrix n n-dims)
            eta 500]
        ;144 # Run iterations
        ;145 for iter in range(max_iter):
        ;160   if iter < 20:
        ;161       momentum = initial_momentum
        ;162   else:
        ;163       momentum = final_momentum
        (dotimes [iter iterations]
          (when (zero? (mod iter 50))
            (log/infof "Iteration %s" iter))
          (let [exaggeration (if (> 101 iter)
                               4.0
                               1.0)
                p_n_n (p-values matrix_n_m perplexity exaggeration)
                gradient_n_n-dims (gradient reduced_n_n-dims p_n_n)
                momentum (if (> 20 iter)
                           0.5
                           0.8)]
            (update!-gains gains_n_n-dims
                           gradient_n_n-dims
                           intermediate-reduced_n_n-dims)
            ;166 iY = momentum * iY - eta * (gains * dY);
            (doto intermediate-reduced_n_n-dims
              (m/mul! momentum)
              (m/sub! (m/mul! gradient_n_n-dims gains_n_n-dims eta)))
            ;167 Y = Y + iY;
            (m/add! reduced_n_n-dims intermediate-reduced_n_n-dims)
            ;168 Y = Y - Math.tile(Math.mean(Y, 0), (n, 1));
            (m/sub! reduced_n_n-dims
                    (as-> reduced_n_n-dims $
                          (m/transpose $)
                          (m/slice-map m/esum $)
                          (m/div! $ n)
                          (repeat n $)
                          (i.core/matrix $)))))
        reduced_n_n-dims))))
