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

;;; An infinite sequence of factorials
(def factorials
  (map * (rest (range)) (cons 1 (lazy-seq factorials))))

(defn prime-factors
  "Prime factors of n (this is a slow and simpleminded method, not recommended for large n)"
  [n]
  (loop [nn n
         [prime & rest-primes] primes
         acc ()]
    (cond (= nn 1) acc
          (zero? (mod nn prime))
          (recur (/ nn prime) primes (cons prime acc))
          :else
          (recur nn rest-primes acc))))

;;; ⩇⩆⩇ Geometry ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn euclidean-distance
  "Euclidean distance between points. Either 2D points supplied as separate arguments, or between 2 n-dimensional points with vector coordinates"
  ([x0 y0 x1 y1]
   (euclidean-distance [x0 y0] [x1 y1]))
  ([p0 p1]
   (Math/sqrt (reduce + (map (fn [v0 v1]
                              (Math/pow (- v0 v1) 2))
                            p0 p1)))))

(defn manhattan-distance
  "Manhattan distance between points. Either 2D points supplied as separate arguments, or between 2 n-dimensional points with vector coordinates"
  ([x0 y0 x1 y1]
   (manhattan-distance [x0 y0] [x1 y1]))
  ([p0 p1]
   (reduce + (map (fn [v0 v1]
                   (Math/abs (- v0 v1)))
                  p0 p1))))

(defn r2d
  "Convert radians to degrees"
  [r]
  (* r (/ 180 Math/PI)))

(defn d2r
  "Convert radians to degrees"
  [d]
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

;;; https://en.wikipedia.org/wiki/Bessel%27s_correction
(defn standard-deviation-sample
  "Return standard deviation with Bessel correction of the elements of `seq`"
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
        mean (double (mean scores))
        threshold (* factor (standard-deviation scores))]
    (remove nil? (map (fn [elt score]
                        (when (> (Math/abs (- score mean)) threshold)
                          elt))
                      seq scores))))

(defn iles
  "Return the boundaries of deciles (n-iles) of seq"
  [seq n]
  (map first (partition n (sort seq))))
    



