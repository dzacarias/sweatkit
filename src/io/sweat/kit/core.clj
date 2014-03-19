(ns io.sweat.kit.core
  (:require [clojure.set :as st]
            [clj-time.core :as time]
            [clj-time.coerce :as tc]))

(declare itv-ctor sseq-ctor reduce-pvseq reduce-mseq mseq sma-lin)

;; =============================================================================
;; Vars

(def metric-types
  #{:hr :power :cadence :steps :speed :distance :position :altitude :calories})

(def trigger-types (st/union #{:manual :time} metric-types))

(def sport-types #{:running :cycling})

(def window-size 10000.0) ; Rolling operators' window size (10 seconds)

;; =============================================================================
;; Abstractions

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

;; =============================================================================
;; Public API

(defn measured? [x] (satisfies? IMeasured x))

(defn point-val? [x] (satisfies? IPointValue x))

(defn acc-metric? 
  "Returns true for metrics whose point value is an accumulator 
   instead of a single reading (e.g. :distance and :calories vs :speed)"
  [m]
  (contains? #{:distance :calories :steps}))


(defn splits [md & {:keys []}])
;  (when (and  (tracked? md metric))
;    (split-with #(< (value %) v) (track md metric))))

(defn speed
  ([md] (track md :speed))
  ([md rfn] (mget md :speed rfn)))

(defn pace
  ([md] (track md :pace))
  ([md rfn] (mget md :pace rfn)))

(defn calories
  ([md] (track md :calories))
  ([md rfn] (mget md :calories rfn)))

(defn altitude
  ([md] (track md :altitude))
  ([md rfn] (mget md :altitude rfn)))

(defn hr
  ([md] (track md :hr))
  ([md rfn] (mget md :hr rfn)))

(defn power
  ([md] (track md :power))
  ([md rfn] (mget md :power rfn)))

(defn cadence
  ([md] (track md :cadence))
  ([md rfn] (mget md :cadence rfn)))

(defn steps
  ([md] (track md :steps))
  ([md rfn] (mget md :steps rfn)))

(defn distance
  ([md] (track md :distance))
  ([md rfn] (mget md :distance rfn)))

(defmulti mseq
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
      (interval [_] (itv-ctor cs dtval))
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
      (interval [this] (itv-ctor cs dtval))
      (mget [this m rfn] (reduce-pvseq m rfn this)))))

;; -----------------------------------------------------------------------------
;; Datatypes

(defrecord PointValue [instant value metric]
  IPointValue
  (inst [this] instant)
  (value [this] value)
  (metric [this] metric))

(defrecord Segment [dtstart duration sport active trigger annotations metrics]
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
                                        
(defrecord Activity [dtstart annotations segments]
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

;; =============================================================================
;; Private API

(defmulti ^:private reduce-pvseq
  "IPointValue seqs can be reduced to extract useful global values.
   The reduce-pvseq multimethod provides a way to apply some default
   reducers (:avg, :max, :min, :total), as well as using some other reducing fn"
  (fn [m rfn pvseq]
    (when (every? #(= m (metric %)) pvseq)
      (if (keyword? rfn) rfn :fn))))

(defmethod reduce-pvseq :fn [m rfn pvseq]
  (reduce rfn pvseq))
                                        
(defmethod reduce-pvseq :avg [m rfn pvseq]
  (/ (reduce + (map value pvseq)) (count pvseq)))

(defmethod reduce-pvseq :mavg [m rfn pvseq]
  (let [roll-vals (sma-lin pvseq window-size)]
    (/ (reduce + roll-vals) (count roll-vals))))

(defmethod reduce-pvseq :min [m _ pvseq]
  (apply min (map value pvseq)))

(defmethod reduce-pvseq :max [m _ pvseq]
  (apply max (map value pvseq)))

(defmethod reduce-pvseq :total [m _ pvseq]
  (apply max (map value pvseq)))

(defmethod reduce-pvseq :default [m rfn pvseq])

(defn- reduce-mseq [m rfn mseq]
  "IMeasured seqs can be reduced. This fn takes a metric, a reducing fn and
   the mseq. The thing to take note is that any metric that don't exist for
   one elem will be treated as if the item did not exist"
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

(defn- itv-ctor
  "Builds a interval for an IMeasured seq (mseq), using the dtval fn to apply
   on each element"
  [mseq dtval]
  (reify IInterval
    (dtstart [this]
      (when-let [f (first mseq)]
        (dtval f)))
    (duration [this]
      (when-let [f (first mseq)]
        (time/in-seconds
         (time/interval (dtval f) (dtval (last mseq))))))))


;; =============================================================================
;; Simple Moving Average
;;
;; These fns are implemented based on Andreas Eckner's work describing
;; how to calculate rolling time series operators for unevenly spaced samples.
;; See "Algorithms for Unevenly Spaced Time Series: Moving Averages and
;; Other Rolling Operators", at http://www.eckner.com/papers/ts_alg.pdf

(defn- trapezoid
  "Helper function that calculates the area of the trapezoid with coordinates
   of the corners (x2, 0), (x2, y2), (x3, 0), and (x3, y3), where y2 is 
   obtained by linear interpolation of (x1, y1) and (x3, y3) evaluated at x2"
  [x1 x2 x3 y1 y3]
  (if (or (= x2 x3) (< x2 x1))
    (* (- x3 x2) y1)
    (let [w (/ (- x3 x2) (- x3 x1))
          y2 (+ (* y1 w) (* y3 (- 1 w)))]
      (/ (* (- x3 x2) (+ y2 y3)) 2))))

(defn- expand-itv-right
  ""
  [pnts roll-area right]
  (+ roll-area (* (/ (+ (value (pnts (dec right)))
                        (value (pnts right)))
                     2)
                  (- (tc/to-long (inst (pnts right)))
                     (tc/to-long (inst (pnts (dec right))))))))

(defn- shrink-itv-left
  ""
  [pnts roll-area right left tau]
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

(defn- inc-truncated-left
  ""
  [pnts roll-area t-left-new left]
  (trapezoid (tc/to-long (inst (pnts (max 0 (dec left)))))
             t-left-new
             (tc/to-long (inst (pnts left)))
             (value (pnts (max 0 (dec left))))
             (value (pnts left))))

(defn- sma-lin
  ""
  [pvseq tau]
  (let [pnts (vec pvseq)
        n (count pnts)]
    (if (<= n 1)
      (map value (seq pnts))
      (loop [left 0
             left-area (* (value (pnts 1)) tau)
             right 1
             roll-area left-area
             out (vector (value (pnts 1)))]
;        (println left left-area right roll-area)
        (if (< right n)
          (let [roll-area-tmp (- (expand-itv-right pnts roll-area right)
                                 left-area)
                [t-left-new ra-new left-new] (shrink-itv-left
                                              pnts roll-area-tmp right left tau)
                left-area-new (inc-truncated-left 
                               pnts roll-area-tmp t-left-new left-new)
                roll-area-new (+ roll-area-tmp left-area-new)
                out-new (conj out (/ roll-area-new tau))]
            (recur left-new left-area-new (inc right) roll-area-new out-new))
          out)))))



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
  (mget s :altitude :avg))
