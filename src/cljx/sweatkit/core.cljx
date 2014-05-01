(ns sweatkit.core
  (:require  [clojure.set :as st]
      #+clj  [clj-time.core :as time]
      #+cljs [cljs-time.core :as time]
      #+clj  [clj-time.coerce :as tc]
      #+cljs [cljs-time.coerce :as tc]
             [schema.core :as s]))

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
   device, metrics may be tracked over time or just stored as global values;
   this abstraction lets you access the whole track and/or the reduced value"
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
  (duration [this] (-> segments interval duration))
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

(defmulti ^:private reduce-pvseq
  "IPointValue seqs can be reduced to extract useful global values.
   The reduce-pvseq multimethod provides a way to apply some default
   reducers represented by keys, as well as using some other reducing fn.
   The default reducers are: :avg, :max, :min and :total"
  (fn [m rfn mseq]
    (cond
     (keyword? rfn) rfn
     (fn? rfn) :fn)))

(defmethod reduce-pvseq :fn [m rfn pvseq]
  "Reduces the pvseq with rfn"
  (reduce rfn (filter #(= m (metric %)) pvseq)))                                  

(defmethod reduce-pvseq :avg [m rfn pvseq]
  "Applies a Simple Moving Average with linear interpolation, that works 
   with unevenly spaced time series (which is most likely the case).
   It then averages the SMA values over time and returns that"
  (let [window-size 60000.0 ; (60 second)
        roll-vals (sma-lin (filter #(= m (metric %)) pvseq)
                           window-size)]
    (/ (reduce + roll-vals) (count roll-vals))))

(defmethod reduce-pvseq :min [m _ pvseq]
  "Returns the minimum value in the pvseq"
  (let [vs (map value (filter #(= m (metric %)) pvseq))]
    (when-not (empty? vs) (apply min vs))))

(defmethod reduce-pvseq :max [m _ pvseq]
  "Returns the max value in the pvseq"
  (let [vs (map value (filter #(= m (metric %)) pvseq))]
    (when-not (empty? vs) (apply max vs))))

(defmethod reduce-pvseq :total [m _ pvseq]
  "Returns the final (total) value for the pvseq.
   Useful for accumulating metrics"
  (let [vs (map value (filter #(= m (metric %)) pvseq))]
    (when-not (empty? vs) (apply max vs))))

(defmulti ^:private reduce-mseq
  "IMeasured seqs can be reduced to extract useful global values.
   The reduce-mseq multimethod provides a way to apply some default
   reducers represented by keys, as well as using some other reducing fn.
   The default reducers are: :avg, :max, :min, :total. Only seq elements
   that contain the given metric/reduced value will be considered"
  (fn [m rfn mseq]
    (cond
     (keyword? rfn) rfn
     (fn? rfn) :fn)))

(defmethod reduce-mseq :fn [m rfn mseq]
  "Reduces the mseq with rfn"
  (reduce rfn mseq))

(defmethod reduce-mseq :avg [m rfn mseq]
  "Returns the avg value for the given metric in mseq's elements. Each
   element's average value is weighted by its interval duration"
  (let [vals (remove nil? (map #(let [avg (mreduce % m :avg)
                                      dur (-> % interval duration)]
                                  (when avg
                                    (hash-map :val (* dur avg) :itv dur)))
                               mseq))]
    (float (/ (apply + (map :val vals))
              (apply + (map :itv vals))))))

(defmethod reduce-mseq :max [m rfn mseq]
  "Returns the max value for the given metric in mseq's elements"
  (let [vs (remove nil? (map #(mreduce % m :max) mseq))]
    (when-not (empty? vs) (apply max vs))))

(defmethod reduce-mseq :min [m rfn mseq]
  "Returns the min value for the given metric in mseq's elements"
  (let [vs (remove nil? (map #(mreduce % m :min) mseq))]
    (when-not (empty? vs) (apply min vs))))

(defmethod reduce-mseq :total [m rfn mseq]
  "Returns the total value for the given metric in mseq's elements"
  (let [vs (remove nil? (map #(mreduce % m :total) mseq))]
    (when-not (empty? vs) (apply + vs))))

(defn- sseq-ctor
  "Creates a sorted seq based on the given coll and the dtval fn to call on
   each item for comparison"
  [coll dtval]
  (sort #(compare (dtval %1) (dtval %2)) (seq coll)))
                                        
;; -----------------------------------------------------------------------------
;; Public API
;; =============================================================================

(def metric-types
  "The set of supported metric types"
  #{:hr :power :cadence :steps :speed :distance
    :pace :position :altitude :calories})

(def trigger-types
  "The set of supported segment triggers (what causes an activity segment
   to be created)"
  (st/union #{:manual :time} metric-types))

(def sport-types
  "The set of supported sport types"
  #{:running :cycling})

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
      (track [this m] (mapcat #(track % m) this))
      (tracked? [this m] (not (empty? (track this m))))
      (interval [_]
        (reify IInterval
          (dtstart [this]
            (some-> cs first dtval))
          (duration [this]
            (when-let [f (first cs)]
              (time/in-millis
               (time/interval (dtval f)
                              (tc/from-long
                               (+ (tc/to-long (dtval (last cs)))
                                  (-> (last cs) interval duration)))))))))
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
      (track [this m] (filter #(= (metric %) m) cs))
      (tracked? [this m] (not (empty? (track this m))))
      (interval [this]
        (reify IInterval
          (dtstart [this]
            (some-> cs first dtval))
          (duration [this]
            (when-let [f (first cs)]
              (time/in-millis (time/interval (dtval f) (dtval (last cs))))))))
      (mreduce [this m rfn] (reduce-pvseq m rfn this)))))

(defmethod mseq :default [_])

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
  (letfn [(between [pvseq dtstart dtend]
            (filter #(and (>= (tc/to-long (inst %))
                              (tc/to-long dtstart))
                          (<= (tc/to-long (inst %))
                              (tc/to-long dtend)))
                    pvseq))
          (make-split [mtk]
            (let [other (map (fn [x] {x {:track (between (track md x)
                                                         (-> mtk first inst)
                                                         (-> mtk last inst))}})
                             (->> (metrics md)
                                  (remove #(= m %))
                                  (filter #(tracked? md %))))
                  mets (apply merge {m {:track mtk}} other)]
              (reify IMeasured
                (metrics [this] (keys mets))
                (track [this m] (:track (m mets)))
                (tracked? [this m] (contains? mets m))
                (interval [this]
                  (reify IInterval
                    (dtstart [_] (inst (first mtk)))
                    (duration [_]
                      (time/in-millis (time/interval (-> mtk first inst)
                                                     (-> mtk last inst))))))
                (mreduce [this m rfn] (reduce-pvseq m rfn (track this m))))))]

    (when (and (measured? md) (acc-metric? m) (tracked? md m))
      (loop [mtk (track md m), acc v, sp []]
        (let [[under over] (split-with #(<= (value %) acc) mtk)
              x (interpolate (last under) (first over) acc)]
          (if (empty? over)
            (mseq (conj sp (make-split under)))
            (recur (cons x over) 
                   (+ acc v)
                   (conj sp (make-split (conj (vec under) x))))))))))

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
                                 pace-trk
                                 (let [v (first dist-trk)
                                       secs (time/in-millis
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
      (contains? (metrics md) :distance) (let [millis (or (duration (interval md)) 0)
                                               dist (or (mreduce md :distance rfn) 0)]
                                           (when-not (zero? dist)
                                             (double (/ (/ millis 1000) dist)))))))

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
    :activities => Collection of maps like this:
        :dtstart => DateTime - starting instant
        :annotations => Map - Relevant annotations
        :segments => Collection of maps like this:
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
    (nil? (s/check db in))))

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
             (merge v (when-let [t (map #(point-val % m) (:track v))]
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
