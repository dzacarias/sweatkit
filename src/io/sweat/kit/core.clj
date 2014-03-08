(ns io.sweat.kit.core
  (:require [clojure.set :as st]))

;; ==============================================================================
;; Vars

(def metric-types #{:hr :power :cadence :steps :speed :distance :position :altitude})
(def trigger-types (st/union #{:manual :time} metric-types))
(def sport-types #{:running :cycling})

;; ==============================================================================
;; Protocols

(defprotocol ISports
  "Something that has a sequence of Sports"
  (sports [this]))

(defprotocol IInterval
  "A time interval has a initial instant and a duration."
  (dtstart [this])
  (duration [this]))

(defprotocol IMeasured
  "Sports metrics associated with something."
  (metrics [this])
  (tracked? [this metric])
  (track [this metric])
  (mget [this metric rfn]))

(defprotocol IMeasurement
  "A point measurement for a given metric"
  (inst [this])
  (value [this])
  (metric [this]))

;; ==============================================================================
;; Public fns 
 
(defn measurement? [x] (satisfies? IMeasurement x))

(defn acc-metric? [m]
  (contains? #{:steps :distance} m))

(defn seq-metric? [m]
  (contains? #{:hr :power :cadence :speed :altitude} m))

(defmulti reduce-metric
  (fn [m rfn mseq]
    (when (every? #(and (measurement? %) (= m (metric %))) mseq)
      rfn)))

(defmethod reduce-metric :default [_ rfn mseq]
  (reduce rfn mseq))
                                        
(defmethod reduce-metric :avg [m _ mseq]
  (when (seq-metric? m)
    (/ (reduce + (map value mseq)) (count mseq))))

(defmethod reduce-metric :min [m _ mseq]
  (when (seq-metric? m)
    (apply min (map value mseq))))

(defmethod reduce-metric :max [m _ mseq]
  (when (seq-metric? m)
    (apply max (map value mseq))))

(defmethod reduce-metric :total [m _ mseq]
  (when (acc-metric? m)
    (apply max (map value mseq))))

;; ==============================================================================
;; Datatypes

(defrecord Measurement [instant value metric]
  IMeasurement
  (inst [this] instant)
  (value [this] value)
  (metric [this] metric))

(defrecord Segment [dtstart duration sport active trigger annotations metrics]
  IInterval
  (dtstart [_] dtstart)
  (duration [_] duration)
  IMeasured
  (metrics [this]
    (keys metrics))
  (tracked? [this m]
    (not (empty? (track this m))))
  (track [this m]
    (:track (m metrics)))
  (mget [this m rfn]
    (if (tracked? this m)
      (reduce-metric m rfn (track this m))
      (when (keyword? rfn) (rfn (m metrics))))))
                                        
(defrecord Activity [dtstart annotations segments]
  ISports
  (sports [this]
    (->> segments (map :sport) (remove nil?) distinct))
  IInterval
  (dtstart [this]
    (or dtstart (->> segments first dtstart)))
  (duration [this]
    (->> segments (map duration) (reduce +)))
  IMeasured
  (metrics [this]
    (->> segments (mapcat metrics) distinct))
  (track [this m]
    (mapcat #(track % m) segments))
  (tracked? [this m]
    (not (empty? (track this m))))
  (mget [this m rfn] 
    (reduce-metric m rfn (track this m))))

;; ====================================================================

(comment
  (def r (clojure.java.io/file "test-resources/FitnessHistoryDetail.tcx"))
  (def p (io.sweat.kit.parse.tcx/parse (.getPath r)))
  (def a (first (:activities p)))
  (def s (first (:segments a)))
  
  (sports a)
  (metrics a)
  (mget s :distance :total)
  (mget s :altitude :max)
  (mget s :altitude :min)
  (mget s :altitude :avg))
