(ns sweatkit.core
  "Provides the basic abstractions and fns to polymorphically work with sports
   activity data: i.e. extracting the usual fitness metrics (like Speed, HR,
   Altitude, Distance) out of collections of activities, individual activities
   or small time segments within them."
  (:require  [clojure.set :as st]
             [clj-time.core :as time]
             [clj-time.coerce :as tc]
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

(declare reduce-pvseq reduce-mseq mkind)

(defrecord Activity
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
  (mreduce [this m rfn]
    (if (and (keyword? rfn) (rfn (m metrics)))
      (rfn (m metrics))
      (when (tracked? this m) (reduce-pvseq m rfn (track this m))))))

(defrecord PointValue
    [instant value metric]
  IPointValue
  (inst [this] instant)
  (value [this] value)
  (metric [this] metric))

(alter-meta! #'->Activity assoc :no-doc true)
(alter-meta! #'map->Activity assoc :no-doc true)
(alter-meta! #'->Segment assoc :no-doc true)
(alter-meta! #'map->Segment assoc :no-doc true)
(alter-meta! #'->PointValue assoc :no-doc true)
(alter-meta! #'map->PointValue assoc :no-doc true)

(declare seq-metrics seq-track seq-tracked? seq-interval seq-mreduce)

(extend-protocol IMeasured

  clojure.lang.LazySeq
  (metrics [this] (seq-metrics this))
  (track [this m] (seq-track this m))
  (tracked? [this m] (seq-tracked? this m))
  (interval [this] (seq-interval this))
  (mreduce [this m rfn] (seq-mreduce this m rfn))

  clojure.lang.APersistentVector
  (metrics [this] (seq-metrics this))
  (track [this m] (seq-track this m))
  (tracked? [this m] (seq-tracked? this m))
  (interval [this] (seq-interval this))
  (mreduce [this m rfn] (seq-mreduce this m rfn))

  clojure.lang.PersistentList
  (metrics [this] (seq-metrics this))
  (track [this m] (seq-track this m))
  (tracked? [this m] (seq-tracked? this m))
  (interval [this] (seq-interval this))
  (mreduce [this m rfn] (seq-mreduce this m rfn)))

;; -----------------------------------------------------------------------------
;; Private API
;; =============================================================================

(defn- seq-metrics [s]
  (condp = (mkind s)
    :measured (->> s (mapcat metrics) distinct)
    :point-val (->> s (map metric) distinct)
    (throw (ex-info "Object is not Measurable" {:type (type s)}))))

(defn- seq-track [s m]
  (condp = (mkind s)
    :measured (mapcat #(track % m) s)
    :point-val (filter #(= (metric %) m) s)
    (throw (ex-info "Object is not Measurable" {:type (type s)}))))

(defn- seq-tracked? [s m]
  (if-not (nil? (mkind s))
    (not (empty? (track s m)))
    (throw (ex-info "Object is not Measurable" {:type (type s)}))))

(defn- seq-interval [s]
  (let [k (mkind s)
        dtval (condp = k
                :measured  #(dtstart (interval %))
                :point-val #(inst %)
                :default)
        cs (sort #(compare (dtval %1) (dtval %2)) s)]
    (if-not (nil? k)
      (reify IInterval
        (dtstart [this]
          (some-> cs first dtval))
        (duration [this]
          (when-let [f (first cs)]
            (time/in-seconds
             (time/interval (dtval f)
                            (tc/from-long
                             (+ (tc/to-long (dtval (last cs)))
                                (if (= k :measured)
                                  (-> (last cs) interval duration (* 1000))
                                  0))))))))
      (throw (ex-info "Object is not Measurable" {:type (type s)})))))
          
(defn- seq-mreduce [s m rfn]
  (condp = (mkind s)
    :measured (reduce-mseq m rfn s)
    :point-val (reduce-pvseq m rfn s)
    (throw (ex-info "Object is not Measurable" {:type (type s)}))))

(defn- sma-lin
  "Simple Moving Average with linear interpolation.
   Takes a PointValue seq and a parameter for window size in seconds (can
   be fractional).
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

(declare measured? point-val? acc-metric?)

(defn- mkind [coll]
  "Takes a collection and returns its measurable kind:
     :measured if every elem is an IMeasured
     :point-val if every elem is an IPointValue
     nil otherwise"
  (when-not (empty? coll) 
    (cond
     (every? measured? coll) :measured
     (every? point-val? coll) :point-val)))

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
    (when-not (empty? roll-vals)
      (/ (reduce + roll-vals) (count roll-vals)))))

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
    (when-not (empty? vs)
      (if (acc-metric? m)
        (- (apply max vs) (apply min vs))
        (apply max vs)))))

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
                               mseq))
        denominators (map :itv vals)]
    (when-not (empty? denominators)
      (float (/ (apply + (map :val vals))
                (apply + denominators))))))

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

(defmulti ^:private linear-itp
  (fn [_ _ _ y1 y3]
    (cond
     (and (number? y1) (number? y3)) :number
     (and (map? y1) (map? y3)) :geo)))

(defmethod linear-itp :number [x1 x2 x3 y1 y3]
  (double (+ y1 (* (- y3 y1) (/ (- x2 x1) (- x3 x1))))))

(defmethod linear-itp :geo [x1 x2 x3 y1 y3]
  {:lat (linear-itp x1 x2 x3 (:lat y1) (:lat y3))
   :lng (linear-itp x1 x2 x3 (:lng y1) (:lng y3))})

(defn- interpolate
  "Takes two point-vals and an intermediate numeric value or instant.
   If given a numeric value, it yields a new point-val with an interpolated
   instant.
   If given an instant, it yields a new point-val with an interpolated
   numeric value."
  [pv1 pv2 v]
  (when-not (or (nil? pv1) (nil? pv2) (nil? v))
      (cond
       (number? v) (->PointValue 
                    (tc/from-long
                     (Math/round
                      (linear-itp (value pv1)
                                  v
                                  (value pv2)
                                  (tc/to-long (inst pv1))
                                  (tc/to-long (inst pv2)))))
                    v
                    (metric pv1))
       (satisfies?
         time/DateTimeProtocol v) (->PointValue
                                   v
                                   (linear-itp (tc/to-long (inst pv1))
                                               (tc/to-long v)
                                               (tc/to-long (inst pv2))
                                               (value pv1)
                                               (value pv2))
                                   (metric pv1)))))

(defn- split-between
  "Takes a point-val seq and splits it between given dtstart and dtend. It creates
   new point-vals at the given instants, interpolating their metric values through
   their surrounding point-vals"
  [pvseq dtstart dtend]
  (let [under (filter #(< (tc/to-long (inst %)) (tc/to-long dtstart)) pvseq)
        over (filter #(> (tc/to-long (inst %)) (tc/to-long dtend)) pvseq)
        between (filter #(and (>= (tc/to-long (inst %))
                                  (tc/to-long dtstart))
                              (<= (tc/to-long (inst %))
                                  (tc/to-long dtend)))
                        pvseq)]
    (remove nil?
            (conj (vec (cons (interpolate (last under) (first between) dtstart)
                             between))
                  (interpolate (last between) (first over) dtend)))))

(defn- rebase-track
  "Takes a pvseq and in case it holds an acc-metric, maps over the seq and
   produces new point vals rebased by subtracting from each the initial value"
  [mtk]
  (if (and (not (empty? mtk)) (acc-metric? (-> mtk first metric)))
    (let [b (-> mtk first value)]
      (map #(->PointValue (inst %) (- (value %) b) (metric %)) mtk))
    mtk))

(defn- make-split
  "Helper fn to produce a Split for some IMeasured. It takes a measured obj
   and a metric track. It walks over all of the other metric tracks and splits
   them at the same start/end instants as the given metric track. All of these
   are then used for the reified IMeasured obj that this fn returns"
  [md mtk]
  (when-not (empty? mtk)
    (let [m (-> mtk first metric)
          dtstart (-> mtk first inst)
          dtend (-> mtk last inst)
          other (map #(let [s (split-between (track md %) dtstart dtend)]
                        (when-not (empty? s)
                          {(metric (first s))
                           {:track (rebase-track s)}}))
                     (->> (metrics md)
                          (remove #(= m %))
                          (filter #(tracked? md %))))
          mets (apply merge {m {:track (rebase-track mtk)}} other)]
      (reify IMeasured
        (metrics [this] (keys mets))
        (track [this m] (:track (m mets)))
        (tracked? [this m] (some #{m} mets))
        (interval [this]
          (reify IInterval
            (dtstart [_] dtstart)
            (duration [_] (time/in-seconds (time/interval dtstart dtend)))))
        (mreduce [this m rfn] (reduce-pvseq m rfn (track this m)))))))

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
  (if (sequential? x)
    (not (nil? (mkind x)))
    (satisfies? IMeasured x)))

(defn point-val?
  "Tests if x implements the IPointValue interface"
  [x]
  (satisfies? IPointValue x))

(defn acc-metric? 
  "Returns true for metrics whose point value is an accumulator 
   instead of a single reading (e.g. :distance and :calories vs :speed)"
  [m]
  (contains? #{:distance :calories :steps} m))

(defn splits
  "Takes a measured object, an accumulator metric and a split value and
   returns a seq of IMeasured objects, representing the split intervals.
   Each interval will have a start/end point corresponding to the accumulator
   metric/value steps, and instants calculated by interpolation from 
   the surrounding points.
   All other metrics are also split between these instants and are included
   in the IMeasured object, if they have metric tracks. If they only have
   reduced values (such as max or avg), they are ignored.
   
   If the given metric is not an accumulator, or there's no track for it,
   this fn will return nil."
  [md m v]
  (when (and (measured? md) (acc-metric? m) (tracked? md m))
    (loop [mtk (track md m), acc v, sp []]
      (let [[under over] (split-with #(<= (value %) acc) mtk)
            x (interpolate (last under) (first over) acc)
            s (if x (conj (vec under) x) (vec under))]
        (if (empty? over)
          (conj sp (make-split md s))
          (recur (cons x over) 
                 (+ acc v)
                 (conj sp (make-split md s))))))))

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
   it via the :speed metric, in case it exists."
  ([md] 
     (cond
      (tracked? md :pace)    (track md :pace)
      (tracked? md :speed)   (map #(->PointValue (inst %)
                                                 (/ 1.0 (value %))
                                                 :pace)
                                  (track md :speed))))
  ([md rfn] 
     (cond
      (some #{:pace} (metrics md)) (mreduce md :pace rfn)
      (some #{:speed} (metrics md)) (let [s (or (mreduce md :speed rfn) 0)]
                                           (when-not (zero? s)
                                             (double (/ 1.0 s)))))))

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

(defn position
  "Takes a measured object and yields the whole Position track, if it exists"
  [md] (track md :position))

(declare MeasurementSchema SegmentSchema ActivitySchema)

(defn measurement?
  "Takes a map and validates if it has the necessary keys and vals
   to be considered a measurement point, like so:
     :instant => Reading instant (DateTime)
     :<metric> (key corresponding to the metric-type) => <value>

     The metric value may be a number or a map with :lan and :lng keys,
     each with numeric values"
  [m] (and (nil? (s/check MeasurementSchema m))
           (not (empty? (filter metric-types (keys m))))))

(defn measurement
  "Takes a map, as validated by the measurement-point? fn and yields
   a new one, implementing the IPointValue protocol. If there is more than
   one metric entry, one will be chose as random, for this measurement

   If the input is already a point-val, it will be returned as-is.
   If the input is not a valid measurement map, the function returns nil"
  [m]
  (if-not (point-val? m)
    (when (measurement? m)
      (let [k (first (filter metric-types (keys m)))]
        (map->PointValue
         {:instant (:instant m), :value (k m), :metric k})))
    m))

(defn segment?
  "Takes a map and validates if it has the proper structure in order
   for it to be considered a segment, like so:
     :dtstart => Starting instant (DateTime)
     :duration => Number of seconds (Double)
     :active => Was this an active period? (Boolean)
     :trigger => What caused this segment? (An element from trigger-types)
     :annotations => Map with relevant annotations
     :metrics => Map with keys corresponding to metric-types,
                       each of its values a map like this:
        :avg => Average value for this metric
        :max => Max value for this metric
        :min => Min value for this metric
        :total => Total value for this metric
        :track => Seq of individual readings, as per the measurement? fn"
  [s] (nil? (s/check SegmentSchema s)))

(defn segment
  "Takes a map, as validated by the segment? fn  and yields a mostly identical
   new one, implementing the IMeasured interface

   If the input is already an IMeasured, it will be returned as-is.
   If the input is not a valid segment map, the function returns nil"
  [s]
  (letfn [(metric [m v]
            {m 
             (merge v (when-let [t (vec (map measurement (:track v)))]
                        (when-not (empty? t) {:track t})))})]
    (if-not (measured? s)
      (when (segment? s)
        (map->Segment
         (merge s {:metrics (apply merge (for [[m v] (:metrics s)]
                                           (metric m v)))})))
      s)))

(defn activity?
  "Takes a map and validates if it has the proper structure in order for it
   to be considered an activity, like so:
        :activities => Collection of maps like this:
            :dtstart => DateTime - starting instant
            :annotations => Map - Relevant annotations
            :segments => Collection of maps, as per the segment? fn"
  [a] (nil? (s/check ActivitySchema a)))


(defn activity 
  "Takes a map, as validated by the activity? fn and yields a mostly identical
   new one, but where each element in :segments is made an IMeasured (as per
   the segment fn)

   If the input is already an IMeasured, it will be returned as-is.
   If the input is not a valid activity map, the function returns nil"
  [a]
  (if-not (measured? a) 
    (when (activity? a)
      (map->Activity
       (merge a {:segments (vec (map segment (:segments a)))})))
    a))

(defn db?
  "Takes a data structure and checks if it conforms to sweatkit's db format:
   A map with these keys and values:
    :activities => Collection of maps, as per the activity? fn"
  [in]
  (let [db {(s/optional-key :activities) [ActivitySchema]}]
    (nil? (s/check db in))))

(defn db
  "Takes a data structure as checked by db? and yields a mostly identical
   new one, but where each element in the :activities vector is made to be
   an IMeasured. Since IMeasured colls are also IMeasured themselves, you
   also get that behavior"
  [in]
  (when (db? in)
    {:activities (vec (map activity (:activities in)))}))


; -----------------------------------------------------------------------------
; Schemas

(def ^:private DateTimeSchema org.joda.time.DateTime)

(def ^:private MeasurementSchema {:instant DateTimeSchema
                                  (apply s/enum metric-types)
                                  (s/either
                                   s/Num
                                   {(s/required-key :lat) s/Num
                                    (s/required-key :lng) s/Num})})

(def ^:private SegmentSchema {:dtstart DateTimeSchema
                              :duration s/Num
                              :sport (apply s/enum sport-types)
                              :active s/Bool
                              :trigger (apply s/enum trigger-types)
                              :metrics {(s/optional-key :avg) [s/Num]
                                        (s/optional-key :max) [s/Num]
                                        (s/optional-key :min) [s/Num]
                                        (s/optional-key :total) [s/Num]
                                        (s/optional-key :track)
                                        [MeasurementSchema]
                                        s/Keyword s/Any}
                              (s/optional-key :annotations) {s/Keyword s/Any}
                              s/Keyword s/Any})

(def ^:private ActivitySchema {:dtstart DateTimeSchema
                               (s/optional-key :annotations) {s/Keyword s/Any}
                               :segments [SegmentSchema]
                               s/Keyword s/Any})
