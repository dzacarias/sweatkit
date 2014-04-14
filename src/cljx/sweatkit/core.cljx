(ns sweatkit.core
  (:require  [clojure.set :as st]
      #+clj  [clj-time.core :as time]
      #+cljs [cljs-time.core :as time]
      #+clj  [clj-time.coerce :as tc]
      #+cljs [cljs-time.coerce :as tc]
             [schema.core :as s]))

;; -----------------------------------------------------------------------------
;; Vars
;; =============================================================================

(def metric-types
  #{:hr :power :cadence :steps :speed :distance
    :pace :position :altitude :calories})

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
  (mreduce [this metric rfn]))

(defprotocol IPointValue
  "A point value for some metric"
  (inst [this])
  (value [this])
  (metric [this]))

;; -----------------------------------------------------------------------------
;; Datatypes
;; =============================================================================

(declare reduce-pvseq)

(defrecord ^:private Activity
    [dtstart annotations segments]
  ISports
  (sports [this]
    (->> segments (map :sport) (remove nil?) distinct))
  IInterval
  (dtstart [this] dtstart)
  (duration [this] (->> segments (map duration) (reduce +)))
  IMeasured
  (metrics [_] (metrics segments))
  (tracked? [_ m] (tracked? segments m))
  (track [_ m] (track segments m))
  (interval [this] this)
  (mreduce [_  m rfn] (mreduce segments m rfn)))

(defrecord ^:private Segment
    [dtstart duration sport annotations metrics]
  IInterval
  (dtstart [_] dtstart)
  (duration [_] duration)
  IMeasured
  (metrics [this] (keys metrics))
  (tracked? [this m] (not (empty? (track this m))))
  (track [this m] (:track (m metrics)))
  (interval [this] this)
  (mreduce [this m rfn]
    (if (tracked? this m)
      (reduce-pvseq m rfn (track this m))
      (when (keyword? rfn) (rfn (m metrics))))))

(defrecord ^:private PointValue
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
  (let [window-size 60000.0 ; (60 second)
        roll-vals (sma-lin pvseq window-size)]
    (/ (reduce + roll-vals) (count roll-vals))))

(defmethod reduce-pvseq :min [m _ pvseq]
  "Returns the minimum value in the pvseq"
  (let [vs (map value pvseq)]
    (when (> (count vs) 0)
      (apply min vs))))

(defmethod reduce-pvseq :max [m _ pvseq]
  "Returns the max value in the pvseq"
  (let [vs (map value pvseq)]
    (when (> (count vs) 0)
      (apply max vs))))

(defmethod reduce-pvseq :total [m _ pvseq]
  "Returns the final (total) value for the pvseq.
   Useful for accumulating metrics"
  (let [vs (map value pvseq)]
    (when (> (count vs) 0)
      (apply max vs))))

(defmethod reduce-pvseq :default [m rfn pvseq])

(defn- reduce-mseq 
  "IMeasured seqs can be reduced. This fn takes a metric, a reducing fn and
   the mseq. The thing to take note is that any metric that doesn't exist for
   one elem will be treated as if the item did not exist in the seq"
  [m rfn mseq]
  (reduce-pvseq
   m rfn (remove nil?
                 (map #(when-let [v (mreduce % m rfn)]
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
    (let [pnts (vec (seq pvseq))
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
;; Public API
;; =============================================================================

(defn measured?
  "Tests if x implements the IMeasured interface"
  [x]
  (satisfies? IMeasured x))

(defn point-val?
  "Tests if x implements the IPointValue interface"
  [x]
  (satisfies? IPointValue x))

(defn acc-metric? 
  "Returns true for metrics whose point value is an accumulator 
   instead of a single reading (e.g. :distance and :calories vs :speed)"
  [m]
  (contains? #{:distance :calories :steps} m))


(defmulti mseq
  "Takes a coll of measured or point-val elements and returns a sorted
   seq (by time), implementing the IMeasured protocol, or 'mseq' as shorthand.
   If coll is not at a coll or it's empty, it returns nil"
  (fn [coll]
    (when (and (coll? coll) (not (empty? coll)))
      (cond
       (every? measured? coll) :measured
       (every? point-val? coll) :point-val))))

(defmethod mseq :default [_])

(defmethod mseq :measured [coll]
  (let [dtval #(dtstart (interval %))
        cs (sseq-ctor coll dtval)]
    (reify
      #+clj  clojure.lang.Seqable
      #+clj  (seq [this] cs)
      #+cljs ISeqable
      #+cljs (-seq [this] cs)
      IMeasured
      (metrics [this] (->> this (mapcat metrics) distinct))
      (track [this m] (mseq (mapcat #(track % m) this)))
      (tracked? [this m] (not (empty? (track this m))))
      (interval [_] (interval-ctor cs dtval))
      (mreduce [this m rfn] (reduce-mseq m rfn this)))))

(defmethod mseq :point-val [coll]
  (let [dtval #(inst %)
        cs (sseq-ctor coll dtval)]
    (reify
      #+clj  clojure.lang.Seqable
      #+clj  (seq [this] cs)
      #+cljs ISeqable
      #+cljs (-seq [this] cs)
      IMeasured
      (metrics [this] (->> this (map metric) distinct))
      (track [this m] (mseq (filter #(= (metric %) m) cs)))
      (tracked? [this m] (not (empty? (track this m))))
      (interval [this] (interval-ctor cs dtval))
      (mreduce [this m rfn] (reduce-pvseq m rfn this)))))


(defn interpolate
  "Takes two point-vals and an intermediate value. Yields a new
   point-val with an interpolated instant for the given value, if both
   points are non-nil."
  [pv1 pv2 v]
  (when (and (not (nil? pv1)) (not (nil? pv2))
             (acc-metric? (metric pv1))
             (acc-metric? (metric pv2)))
    (->PointValue 
     (tc/from-long
      (Math/round
       (double (+ (tc/to-long (inst pv1))
                  (* (- (tc/to-long (inst pv2))
                        (tc/to-long (inst pv1)))
                     (/ (- v (value pv1))
                        (- (value pv2) (value pv1))))))))
     v
     (metric pv1))))

(defn splits
  "Takes a measured object, an accumulator metric and the split value.
   
   If the metric is not an accumulator, or there's no track for it, this fn
   returns nil.

   Otherwise, returns a seq of IMeasured objects, representing the split
   intervals. Each interval will have a start/end point interpolated from 
   the surrounding points. All other metrics are also split at this instant"
  [md m v]
  (when (and (measured? md) (acc-metric? m) (tracked? md m))
    (loop [mtk (vec (track md m)), acc v, sp []]
      (let [[under over] (split-with #(<= (value %) acc) mtk)
            x (interpolate (last under) (first over) acc)]
          (if (empty? over)
            (map mseq (conj sp under))
            (recur (cons x over) (+ acc v) (conj sp (cons x under))))))))

(defn speed
  "Takes a measured object and an optional reducing fn. When the fn is not
   given, yields the whole Speed track, otherwise it returns a single value.
   rfn can be any fn reducing point-vals or one of these keywords for
   standard behavior: :avg, :max, :min"
  ([md] (track md :speed))
  ([md rfn] (mreduce md :speed rfn)))

(defn pace
  "Takes a measured object and an optional reducing fn. When the fn is not
   given, yields the whole Pace track, otherwise it returns a single value.
   rfn can be any fn reducing point-vals or one of these keywords for
   standard behavior: :avg, :max, :min.

   If the measured obj doesn't have this metric itself, this fn tries to get
   it via the :speed and :distance metrics (in that order), in case they exist.
   In these cases, the returned values will be limited to the data that is
   available for :speed or :distance."
  ([md] 
     (cond
      (tracked? md :pace)    (track md :pace)
      (tracked? md :speed)   (map #(->PointValue (inst %)
                                                 (/ 1.0 (value %))
                                                 :pace)
                                  (track md :speed)))
     (tracked? md :distance) (loop [dist-trk (track md :distance)
                                    pace-trk []
                                    prev-inst (inst (first dist-trk))
                                    prev-dist (value (first dist-trk))]
                               (if (empty? dist-trk)
                                 (mseq pace-trk)
                                 (let [v (first dist-trk)
                                       secs (time/in-seconds
                                             (time/interval prev-inst (inst v)))
                                       dist (- (value v) prev-dist)
                                       p (double (if-not (zero? dist)
                                                   (/ secs dist)
                                                   0))]
                                   (recur (next dist-trk)
                                          (conj pace-trk
                                                (->PointValue (inst v) p :pace))
                                          (inst v)
                                          (value v))))))
  ([md rfn] 
     (cond
      (contains? (metrics md) :pace)     (mreduce md :pace rfn)
      (contains? (metrics md) :speed)    (let [s (or (mreduce md :speed rfn) 0)]
                                           (when-not (zero? s)
                                             (double (/ 1.0 s))))
      (contains? (metrics md) :distance) (let [secs (or (duration (interval md)) 0)
                                               dist (or (mreduce md :distance rfn) 0)]
                                           (when-not (zero? dist)
                                             (double (/ secs dist)))))))

(defn calories
  "Takes a measured object and an optional reducing fn. When the fn is not
   given, yields the whole Calories track, otherwise it returns a single value.
   rfn can be any fn reducing point-vals or the :total keyword to get the
   accumulated value"
  ([md] (track md :calories))
  ([md rfn] (mreduce md :calories rfn)))

(defn altitude
  "Takes a measured object and an optional reducing fn. When the fn is not
   given, yields the whole Altitude track, otherwise it returns a single value.
   rfn can be any fn reducing point-vals or one of these keywords for
   standard behavior: :avg, :max, :min"
  ([md] (track md :altitude))
  ([md rfn] (mreduce md :altitude rfn)))

(defn hr
  "Takes a measured object and an optional reducing fn. When the fn is not
   given, yields the whole Heart Rate track, otherwise it returns a single value.
   rfn can be any fn reducing point-vals or one of these keywords for
   standard behavior: :avg, :max, :min"
  ([md] (track md :hr))
  ([md rfn] (mreduce md :hr rfn)))

(defn power
  "Takes a measured object and an optional reducing fn. When the fn is not
   given, yields the whole Power track, otherwise it returns a single value.
   rfn can be any fn reducing point-vals or one of these keywords for
   standard behavior: :avg, :max, :min"
  ([md] (track md :power))
  ([md rfn] (mreduce md :power rfn)))

(defn cadence
  "Takes a measured object and an optional reducing fn. When the fn is not
   given, yields the whole Cadence track, otherwise it returns a single value.
   rfn can be any fn reducing point-vals or one of these keywords for
   standard behavior: :avg, :max, :min"
  ([md] (track md :cadence))
  ([md rfn] (mreduce md :cadence rfn)))

(defn steps
  "Takes a measured object and an optional reducing fn. When the fn is not
   given, yields the whole Steps track, otherwise, it returns a single value.
   rfn can be any fn reducing point-vals or the :total keyword to get the
   accumulated value"
  ([md] (track md :steps))
  ([md rfn] (mreduce md :steps rfn)))

(defn distance
  "Takes a measured object and an optional reducing fn. When the fn is not
   given, yields the whole Distance track, otherwise it returns a single value.
   rfn can be any fn reducing point-vals or the :total keyword to get the
   accumulated value.
   N.B: As of now, this metric will not use the geo track to calculate distance.
        Only tracks that provide this metric directly are considered (this might
        change in the future)"
  ([md] (track md :distance))
  ([md rfn] (mreduce md :distance rfn)))

(defn valid-sweat?
  "Takes a data structure and checks if it conforms to sweatkit's format:
   A map with these keys and values:
    :activities => collection of maps like this:
        :dtstart => DateTime, starting instant
        :annotations => Map with relevant annotations
        :segments => collection of maps like this:
           :dtstart => Starting instant (DateTime)
           :duration => Number of milliseconds (Integer)
           :active => Was this an active period? (Boolean)
           :trigger => What caused this segment? (An element from trigger-types)
           :annotations => Map with relevant annotations
           :metrics => Map with keys corresponding to metric-types,
                       each of its values a map like this:
             :avg => Average value for this metric
             :max => Max value for this metric
             :min => Min value for this metric
             :total => Total value for this metric
             :track => Seq of individual readings, each like this:
                 :instant => Reading instant (DateTime)
                 :<metric> (key corresponding to the metric-type) => <value>"
  [in]
  (let [datetime #+clj org.joda.time.DateTime #+cljs goog.date.UtcDateTime
        segment {:dtstart datetime
                 :duration s/Num
                 :sport (apply s/enum (seq sport-types))
                 :active s/Bool
                 :trigger (apply s/enum (seq trigger-types))
                 :metrics {(s/optional-key :avg) [s/Num]
                           (s/optional-key :max) [s/Num]
                           (s/optional-key :min) [s/Num]
                           (s/optional-key :total) [s/Num]
                           (s/optional-key :track)
                           [{:instant datetime
                             ; Metric 
                             (s/enum metric-types)
                             (s/either
                              s/Num
                              {(s/required-key :lat) s/Num
                               (s/required-key :lng) s/Num})}]
                           s/Keyword s/Any}
                 (s/optional-key :annotations) {s/Keyword s/Any}
                 s/Keyword s/Any}
        activity {:dtstart datetime
                  (s/optional-key :annotations) {s/Keyword s/Any}
                  :segments [segment]
                  s/Keyword s/Any}
        db {(s/optional-key :activities) [activity]}]
    (s/validate db in)))

(defn build
  "Takes a data structure as checked by valid-sweat? and yields a mostly
   identical new one, with the following features:
   * The :activities coll is turned into an mseq (sorted IMeasured) and its
     elements are also IMeasured (via Activity record)
   * The :segments coll in each Activity is also made an mseq, with each of 
     elements also becoming IMeasured (via Segment record) 
   * All metric tracks are also made mseqs and their items made IPointValues
     record"
  [in]
  (letfn [(point-val [pv m]
            (map->PointValue
             {:instant (:instant pv), :value (m pv), :metric m}))
          (metric [m v]
            {m 
             (merge v (when-let [t (mseq (map #(point-val % m) (:track v)))]
                        {:track t}))})
          (segment [s]
            (map->Segment
             (merge s {:metrics (apply merge (for [[m v] (:metrics s)]
                                               (metric m v)))})))
          (activity [a]
            (map->Activity
             (merge a {:segments (mseq (map segment (:segments a)))})))]

    (when (valid-sweat? in)
      {:activities (mseq (map activity (:activities in)))})))
