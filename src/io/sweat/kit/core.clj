(ns io.sweat.kit.core
  (:require [clojure.set :as st]
            [clj-time.core :as time]))

;; ==============================================================================
;; Vars

(def metric-types
  #{:hr :power :cadence :steps :speed :distance :position :altitude :calories})

(def trigger-types (st/union #{:manual :time} metric-types))

(def sport-types #{:running :cycling})


;; ==============================================================================
;; Abstractions

(defprotocol ISports
  "Something that has a sequence of Sports"
  (sports [this]))

(defprotocol IInterval
  "A time interval has a initial instant and a duration."
  (dtstart [this])
  (duration [this]))

(defprotocol IMeasured
  "Sports metrics taken over a time interval."
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

(declare itv-ctor mseq-ctor reduce-pvseq mseq)

(defn measured? [x] (satisfies? IMeasured x))

(defn point-val? [x] (satisfies? IPointValue x))

(defn splits [m])

(defn pace
  ([m] (track m :pace))
  ([m rfn] (mget m :pace rfn)))

(defn speed
  ([m] (track m :speed))
  ([m rfn] (mget m :speed rfn)))

(defn cals
  ([m] (track m :calories))
  ([m rfn] (mget m :calories rfn)))

(defn altitude
  ([m] (track m :altitude))
  ([m rfn] (mget m :altitude rfn)))

(defn hr
  ([m] (track m :hr))
  ([m rfn] (mget m :hr rfn)))

(defn power
  ([m] (track m :power))
  ([m rfn] (mget m :power rfn)))

(defn cadence
  ([m] (track m :cadence))
  ([m rfn] (mget m :cadence rfn)))

(defn steps
  ([m] (track m :steps))
  ([m rfn] (mget m :steps rfn)))

(defn distance
  ([m] (track m :distance))
  ([m rfn] (mget m :distance rfn)))

(defmulti mseq
  (fn [coll] 
    (cond
     (every? measured? coll) :measured
     (every? point-val? coll) :point-val)))

(defmethod mseq :default [_])

(defmethod mseq :measured [coll]
  (let [dtval #(dtstart (interval %))
        cs (mseq-ctor coll dtval)]
    (reify
      clojure.lang.Seqable
      (seq [this] cs)
      IMeasured
      (metrics [this] (->> this (mapcat metrics) distinct))
      (track [this m] (mapcat #(track % m) this))
      (tracked? [this m] (not (empty? (track this m))))
      (interval [_] (itv-ctor cs dtval))
      (mget [this m rfn]
        (reduce-pvseq
         m rfn (remove nil?
                       (map #(when-let [v (mget % m rfn)]
                               (->PointValue (dtstart (interval %)) v m))
                            this)))))))

(defmethod mseq :point-val [coll]
  (let [dtval #(inst %)
        cs (mseq-ctor coll dtval)]
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


;; IPointValue seqs can be reduced to extract useful global values.
;; The reduce-pvseq multimethod provides a way to apply some default
;; reducers (:avg, :max, :min, :total), as well as using some other reducing fn. 

(defmulti ^:private reduce-pvseq (fn [m rfn pvseq] rfn))

(defmethod reduce-pvseq :default [m rfn pvseq]
  (when (every? #(= m %) pvseq)
    (reduce rfn pvseq)))
                                        
(defmethod reduce-pvseq :avg [m _ pvseq]
  (when (every? #(= m %) pvseq)
    (/ (reduce + (map value pvseq)) (count pvseq))))

(defmethod reduce-pvseq :min [m _ pvseq]
  (when (every? #(= m %) pvseq)
    (apply min (map value pvseq))))

(defmethod reduce-pvseq :max [m _ pvseq]
  (when (every? #(= m %) pvseq)
    (apply max (map value pvseq))))

(defmethod reduce-pvseq :total [m _ pvseq]
  (when (every? #(= m %) pvseq)
    (apply max (map value pvseq))))

(defn- mseq-ctor [coll dtval]
  (sort #(compare (dtval %1) (dtval %2)) (seq coll)))

(defn- itv-ctor [mseq dtval]
  (reify IInterval
    (dtstart [this]
      (when-let [f (first mseq)]
        (dtval f)))
    (duration [this]
      (when-let [f (first mseq)]
        (time/in-seconds
         (time/interval (dtval f) (dtval (last mseq))))))))


;; =============================================================================

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
