(ns org.parkerici.multitool.math)

;;; ⩇⩆⩇ Scaling and interpolation ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn interpolate
  "Return a number between a and b, s is from [0,1]"
  [a b s]
  (+ (* a (- 1 s)) (* b s)))

(defn interpolated
  "Return a sequence of n equally-spaced numbers beteen and b"
  [a b n]
  (map #(interpolate a b (/ % (float n))) (range n)))

(defn rescale
  "Val is a number from [from-lower, from-upper], scale it to [to-loer, to-upper]"
  [val from-lower from-upper to-lower to-upper]
  (+ to-lower
     (* (- val from-lower)
        (/ (- to-upper to-lower)
           (- from-upper from-lower)))))


;;; ⩇⩆⩇ Number theory ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; An infinite sequence of primes. 
(def primes
  (cons 2
        (lazy-seq
         (filter
          (fn [n] (not (some (fn [i] (= 0 (rem n i)))
                             (take-while #(<= % (Math/sqrt n)) primes))))
          (iterate #(+ % 2) 3)
          ))))

(defn prime-factors
  "Prime factors of n (this is a slow and simpleminded method, not recommended for large n)"
  [n]
  (loop [nn n
         [prime & rest-primes] primes
         acc ()]
    (cond (= nn 1) acc
          (zero? (mod nn prime))
          (recur (/ nn prime) primes (cons prime acc))
          true
          (recur nn rest-primes acc))))

;;; ⩇⩆⩇ Geometry ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn distance
  "Euclidean distance between points. Either 2D points supplied as separate arguments, or between 2  n-dimensional points with vector coordinates"
  ([x0 y0 x1 y1]
   (Math/sqrt (+ (Math/pow (- x0 x1) 2) (Math/pow (- y0 y1) 2))))
  ([p0 p1]
   (Math/sqrt (apply + (map (fn [v0 v1]
                              (Math/pow (- v0 v1) 2))
                            p0 p1)))))

(defn r2d [r]
  (* r (/ 180 Math/PI)))

(defn d2r [d]
  (* d (/ Math/PI 180)))

;;; ⩇⩆⩇ Naive statistics ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn mean
  "Return the arithmetic mean of the elements of `seq`"
  [seq]
  (/ (reduce + seq)
     (count seq)))

(defn standard-deviation
  "Return standard deviation of the elements of `seq`"
  [seq]
  (let [mean0 (mean seq)]
    (Math/sqrt
     (/ (reduce + (map #(Math/pow (- % mean0) 2) seq))
        (- (count seq) 1)))))

(defn coefficent-of-variation
  "Return coefficent of variation of the elements of `seq`"
  [seq]
  (/ (standard-deviation seq)
     (mean seq)))

(defn geometric-mean
  "Return the geometric mean of the elements of `seq`"
  [seq]
  (Math/pow (reduce * (map double seq))
            (/ 1 (count seq))))

(defn score-b
  "Return a list of [elt score] pairs, in descending score order."
  [keyfn seq]
  (reverse
   (sort-by second
            (map (fn [elt] [elt (keyfn elt)]) seq))))

(defn outliers-by
  "Return elements of `seq` on whom `scorefn` is more than `factor` standard-deviations away from the mean."
  [scorefn seq factor]
  (let [scores (map scorefn seq)
        threshold (+ (mean scores) (* factor (standard-deviation scores)))]
    (filter identity (map (fn [elt score] (when (>= score threshold) elt)) seq scores))))

(defn iles
  "Return the boundaries of deciles (n-iles) of seq"
  [seq n]
  (map first (partition n (sort seq))))
    



