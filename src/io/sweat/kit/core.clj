(ns io.sweat.kit.core
  (:require [clojure.set :as st]
            [clj-time.core :as time]
            [clj-time.coerce :as tc]))

(declare interval-ctor sseq-ctor reduce-pvseq reduce-mseq mseq sma-lin)

;; -----------------------------------------------------------------------------
;; Vars
;; =============================================================================

(def metric-types
  #{:hr :power :cadence :steps :speed :distance :position :altitude :calories})

(def trigger-types (st/union #{:manual :time} metric-types))

(def sport-types #{:running :cycling})

;; -----------------------------------------------------------------------------
;; Abstractions
;; =============================================================================

(defprotocol ISports
  "Something with a sequence of Sports"
  (sports [this]))

(defprotocol IInterval
  "A time interval has an initial instant and a duration"
  (dtstart [this])
  (duration [this]))

(defprotocol IMeasured
  "A set of sports metrics taken over a time interval. Depending on the source
   device, metrics may be tracked over time or just stored as global values"
  (interval [this])
  (metrics [this])
  (tracked? [this metric])
  (track [this metric])
  (mget [this metric rfn]))

(defprotocol IPointValue
  "A point value for some metric"
  (inst [this])
  (value [this])
  (metric [this]))

;; -----------------------------------------------------------------------------
;; Public API
;; =============================================================================

(defn measured?
  "Tests if something implements the IMeasured interface"
  [x]
  (satisfies? IMeasured x))

(defn point-val?
  "Tests if something implements the IPointValue interface"
  [x]
  (satisfies? IPointValue x))

(defn acc-metric? 
  "Returns true for metrics whose point value is an accumulator 
   instead of a single reading (e.g. :distance and :calories vs :speed)"
  [m]
  (contains? #{:distance :calories :steps} m))

(defn splits
  "Takes a measured object, an accumulator metric and the split value.
   
   If the metric is not an accumulator, or there's no track for it, this fn
   returns nil.

   Otherwise, returns a seq of IMeasured objects, representing the split
   intervals. Each interval will have a start/end point interpolated from 
   the surrounding points. All other metrics are also split at this instant"
  [md m v]
  (letfn []
    (when (and (measured? md) (acc-metric? m) (tracked? md m))
      (loop [mtk (track md m)
             acc v
             sp []]
        (if (empty? mtk)
          sp
          (let [cut (split-with #(<= (value %) acc) mtk)
                x1 (last (first cut))
                x3 (first (rest cut))
                x2 (interpolate x1 x3 acc)]
            (recur (cons x2 (rest cut))
                   (+ acc v)
                   (conj sp (cons x2 (first cut))))))))))

(defn speed
  "Takes a measured object and an optional reducing fn. When the fn is not
   given, yields the whole Speed track, otherwise it returns a single value.
   rfn can be any fn reducing point-vals or one of these keywords for
   standard behavior: :avg, :max, :min"
  ([md] (track md :speed))
  ([md rfn] (mget md :speed rfn)))

(defn pace
  "Takes a measured object and an optional reducing fn. When the fn is not
   given, yields the whole Pace track, otherwise it returns a single value.
   rfn can be any fn reducing point-vals or one of these keywords for
   standard behavior: :avg, :max, :min"
  ([md] (track md :pace))
  ([md rfn] (mget md :pace rfn)))

(defn calories
  "Takes a measured object and an optional reducing fn. When the fn is not
   given, yields the whole Calories track, otherwise it returns a single value.
   rfn can be any fn reducing point-vals or the :total keyword to get the
   accumulated value"
  ([md] (track md :calories))
  ([md rfn] (mget md :calories rfn)))

(defn altitude
  "Takes a measured object and an optional reducing fn. When the fn is not
   given, yields the whole Altitude track, otherwise it returns a single value.
   rfn can be any fn reducing point-vals or one of these keywords for
   standard behavior: :avg, :max, :min"
  ([md] (track md :altitude))
  ([md rfn] (mget md :altitude rfn)))

(defn hr
  "Takes a measured object and an optional reducing fn. When the fn is not
   given, yields the whole Heart Rate track, otherwise it returns a single value.
   rfn can be any fn reducing point-vals or one of these keywords for
   standard behavior: :avg, :max, :min"
  ([md] (track md :hr))
  ([md rfn] (mget md :hr rfn)))

(defn power
  "Takes a measured object and an optional reducing fn. When the fn is not
   given, yields the whole Power track, otherwise it returns a single value.
   rfn can be any fn reducing point-vals or one of these keywords for
   standard behavior: :avg, :max, :min"
  ([md] (track md :power))
  ([md rfn] (mget md :power rfn)))

(defn cadence
  "Takes a measured object and an optional reducing fn. When the fn is not
   given, yields the whole Cadence track, otherwise it returns a single value.
   rfn can be any fn reducing point-vals or one of these keywords for
   standard behavior: :avg, :max, :min"
  ([md] (track md :cadence))
  ([md rfn] (mget md :cadence rfn)))

(defn steps
  "Takes a measured object and an optional reducing fn. When the fn is not
   given, yields the whole Steps track, otherwise, it returns a single value.
   rfn can be any fn reducing point-vals or the :total keyword to get the
   accumulated value"
  ([md] (track md :steps))
  ([md rfn] (mget md :steps rfn)))

(defn distance
  "Takes a measured object and an optional reducing fn. When the fn is not
   given, yields the whole Distance track, otherwise it returns a single value.
   rfn can be any fn reducing point-vals or the :total keyword to get the
   accumulated value"
  ([md] (track md :distance))
  ([md rfn] (mget md :distance rfn)))

(defmulti mseq
  "Takes a collection of measured or point-val elements and returns a seq
   implementing the IMeasured protocol, or 'mseq' as shorthand."
  (fn [coll] 
    (cond
     (every? measured? coll) :measured
     (every? point-val? coll) :point-val)))

(defmethod mseq :default [_])

(defmethod mseq :measured [coll]
  (let [dtval #(dtstart (interval %))
        cs (sseq-ctor coll dtval)]
    (reify
      clojure.lang.Seqable
      (seq [this] cs)
      IMeasured
      (metrics [this] (->> this (mapcat metrics) distinct))
      (track [this m] (mapcat #(track % m) this))
      (tracked? [this m] (not (empty? (track this m))))
      (interval [_] (interval-ctor cs dtval))
      (mget [this m rfn] (reduce-mseq m rfn this)))))

(defmethod mseq :point-val [coll]
  (let [dtval #(inst %)
        cs (sseq-ctor coll dtval)]
    (reify
      clojure.lang.Seqable
      (seq [this] cs)
      IMeasured
      (metrics [this] (->> this (map metric) distinct))
      (track [this m] (filter #(= (metric %) m) this))
      (tracked? [this m] (not (empty? (track this m))))
      (interval [this] (interval-ctor cs dtval))
      (mget [this m rfn] (reduce-pvseq m rfn this)))))

(defn interpolate
  "Takes two point-vals and an intermediate value. Yields a new point-val
   with an interpolated instant for the given value"
  [pv1 pv2 v]
  (when (and (acc-metric? (metric pv1))
             (acc-metric? (metric pv2)))
    (->PointValue 
     (tc/from-long
      (Math/round
       (float (+ (tc/to-long (inst pv1))
                 (* (- (tc/to-long (inst pv2)) (tc/to-long (inst pv1)))
                    (/ (- v (value pv1)) (- (value pv2) (value pv1)))))))
      v)
     (metric pv1))))

;; -----------------------------------------------------------------------------
;; Datatypes
;; =============================================================================
                                        
(defrecord Activity
    [dtstart annotations segments]
  ISports
  (sports [this]
    (->> segments (map :sport) (remove nil?) distinct))
  IInterval
  (dtstart [this] dtstart)
  (duration [this] (->> segments (map duration) (reduce +)))
  IMeasured
  (metrics [_] (metrics (mseq segments)))
  (tracked? [_ m] (tracked? (mseq segments) m))
  (track [_ m] (track (mseq segments) m))
  (interval [this] this)
  (mget [_  m rfn] (mget (mseq segments) m rfn)))

(defrecord Segment
    [dtstart duration sport annotations metrics]
  IInterval
  (dtstart [_] dtstart)
  (duration [_] duration)
  IMeasured
  (metrics [this] (keys metrics))
  (tracked? [this m] (not (empty? (track this m))))
  (track [this m] (:track (m metrics)))
  (interval [this] this)
  (mget [this m rfn]
    (if (tracked? this m)
      (reduce-pvseq m rfn (track this m))
      (when (keyword? rfn) (rfn (m metrics))))))

(defrecord PointValue
    [instant value metric]
  IPointValue
  (inst [this] instant)
  (value [this] value)
  (metric [this] metric))

;; -----------------------------------------------------------------------------
;; Private API
;; =============================================================================

(defmulti ^:private reduce-pvseq
  "IPointValue seqs can be reduced to extract useful global values.
   The reduce-pvseq multimethod provides a way to apply some default
   reducers represented by keys, as well as using some other reducing fn.
   The default reducers are: :avg, :savg, :max, :min, :total"
  (fn [m rfn pvseq]
    (when (every? #(= m (metric %)) pvseq)
      (if (keyword? rfn) rfn :fn))))

(defmethod reduce-pvseq :fn [m rfn pvseq]
  "Reduces the pvseq with rfn"
  (reduce rfn pvseq))                                  

(defmethod reduce-pvseq :avg [m rfn pvseq]
  "Applies a Simple Moving Average with linear interpolation, that works 
   with unevenly spaced time series (which is most likely the case).
   It then averages the SMA values over time and returns that"
  (let [window-size 60000.0 ; (60 seconds)
        roll-vals (sma-lin pvseq window-size)]
    (/ (reduce + roll-vals) (count roll-vals))))

(defmethod reduce-pvseq :min [m _ pvseq]
  "Returns the minimum value in the pvseq"
  (apply min (map value pvseq)))

(defmethod reduce-pvseq :max [m _ pvseq]
  "Returns the max value in the pvseq"
  (apply max (map value pvseq)))

(defmethod reduce-pvseq :total [m _ pvseq]
  "Returns the final (total) value for the pvseq.
   Useful for accumulating metrics"
  (apply max (map value pvseq)))

(defmethod reduce-pvseq :default [m rfn pvseq])

(defn- reduce-mseq 
  "IMeasured seqs can be reduced. This fn takes a metric, a reducing fn and
   the mseq. The thing to take note is that any metric that doesn't exist for
   one elem will be treated as if the item did not exist in the seq"
  [m rfn mseq]
  (reduce-pvseq
   m rfn (remove nil?
                 (map #(when-let [v (mget % m rfn)]
                         (->PointValue (dtstart (interval %)) v m))
                      mseq))))

(defn- sseq-ctor
  "Creates a sorted seq based on the given coll and the dtval fn to call on
   each item for comparison"
  [coll dtval]
  (sort #(compare (dtval %1) (dtval %2)) (seq coll)))

(defn- interval-ctor
  "Builds an interval for an IMeasured seq (mseq), using the dtval fn to
   get an element's DateTime. This function assumes the seq is already
   sorted"
  [mseq dtval]
  (reify IInterval
    (dtstart [this]
      (when-let [f (first mseq)]
        (dtval f)))
    (duration [this]
      (when-let [f (first mseq)]
        (time/in-seconds
         (time/interval (dtval f) (dtval (last mseq))))))))

(defn- sma-lin
  "Simple Moving Average with linear interpolation.
   Takes a PointValue seq and a parameter for window size in milliseconds.
   Returns a seq of values, corresponding to the SMAlin over time.

   It's implemented based on Andreas Eckner's work describing how to
   calculate rolling time series operators for unevenly spaced samples.
   See: \"Algorithms for Unevenly Spaced Time Series: Moving Averages and
   Other Rolling Operators\", at http://www.eckner.com/papers/ts_alg.pdf"
  [pvseq tau]
  (letfn [(trapezoid [x1 x2 x3 y1 y3]
            (if (or (= x2 x3) (< x2 x1))
              (* (- x3 x2) y1)
              (let [w (/ (- x3 x2) (- x3 x1))
                    y2 (+ (* y1 w) (* y3 (- 1 w)))]
                (/ (* (- x3 x2) (+ y2 y3)) 2))))

          (expand-itv-right [pnts roll-area right]
            (+ roll-area (* (/ (+ (value (pnts (dec right)))
                                  (value (pnts right)))
                               2)
                            (- (tc/to-long (inst (pnts right)))
                               (tc/to-long (inst (pnts (dec right))))))))
          
          (shrink-itv-left [pnts roll-area right left tau]
            (let [t-left-new (- (tc/to-long (inst (pnts right))) tau)]
              (loop [ra-new roll-area
                     left-new left]
                (if (> (tc/to-long (inst (pnts left-new))) t-left-new)
                  [t-left-new ra-new left-new]
                  (recur (- ra-new (* (/ (+ (value (pnts left-new))
                                            (value (pnts (inc left-new))))
                                         2)
                                      (- (tc/to-long (inst (pnts (inc left-new))))
                                         (tc/to-long (inst (pnts left-new))))))
                         (inc left-new))))))

          (inc-truncated-left [pnts t-left-new left]
            (trapezoid (tc/to-long (inst (pnts (max 0 (dec left)))))
                       t-left-new
                       (tc/to-long (inst (pnts left)))
                       (value (pnts (max 0 (dec left))))
                       (value (pnts left))))]

    ; Main body
    (let [pnts (vec pvseq)
          n (count pnts)]
      (if (< n 2)
        (map value (seq pnts))
        (loop [left 0
               left-area (* (value (first pnts)) tau)
               right 1
               roll-area left-area
               out (vector (value (first pnts)))]
          (if (< right n)
            (let [ra-tmp1 (- (expand-itv-right pnts roll-area right) left-area)
                  [t-left-new ra-tmp2 left-new] (shrink-itv-left
                                                 pnts ra-tmp1 right left tau)
                  left-area-new (inc-truncated-left pnts t-left-new left-new)
                  roll-area-new (+ ra-tmp2 left-area-new)
                  out-new (conj out (/ roll-area-new tau))]
              (recur left-new left-area-new (inc right) roll-area-new out-new))
            out))))))

;; -----------------------------------------------------------------------------

(comment
  (def r (clojure.java.io/file "test-resources/FitnessHistoryDetail.tcx"))
  (def p (io.sweat.kit.parse.tcx/parse (.getPath r)))
  (def a (first (:activities p)))
  (def s (first (:segments a)))
  (sports a)
  (metrics a)
  (def sgmts (:segments a))
  (mget s :distance :total)
  (mget s :altitude :max)
  (mget s :altitude :min)
  (mget s :altitude :avg)

  (def vls  [{:time "2014-01-01T00:00:01.000Z" :value 0.0}
             {:time "2014-01-01T00:00:01.540Z" :value 0.2}
             {:time "2014-01-01T00:00:01.580Z" :value 0.4}
             {:time "2014-01-01T00:00:02.010Z" :value 0.6}
             {:time "2014-01-01T00:00:03.350Z" :value 0.8}
             {:time "2014-01-01T00:00:05.280Z" :value 1.0}])

  (def pvs  (for [v vls]
              (->PointValue (:time v) (:value v) :speed)))
  
  )
