(ns io.sweat.kit.core
  (:require [clojure.set :as st]
            [clj-time.core :as time]))

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
  "Sports metrics taken over a time interval."
  (metrics [this])
  (tracked? [this metric])
  (track [this metric])
  (interval [this])
  (mget [this metric rfn]))

(defprotocol IMeasurement
  "A point measurement for a given metric"
  (inst [this])
  (value [this])
  (metric [this]))

;; ==============================================================================
;; Fns
 
(defn measurement? [x] (satisfies? IMeasurement x))

(defn measured? [x] (satisfies? IMeasured x))

(defn acc-metric? [m]
  (contains? #{:steps :distance} m))

(defn seq-metric? [m]
  (contains? #{:hr :power :cadence :speed :altitude} m))

(defmulti ^:private reduce-mseq
  (fn [m rfn mseq]
    (when (every? #(and (measurement? %) (= m (metric %))) mseq)
      rfn)))

(defmethod reduce-mseq :default [_ rfn mseq]
  (reduce rfn mseq))
                                        
(defmethod reduce-mseq :avg [m _ mseq]
  (when (seq-metric? m)
    (/ (reduce + (map value mseq)) (count mseq))))

(defmethod reduce-mseq :min [m _ mseq]
  (when (seq-metric? m)
    (apply min (map value mseq))))

(defmethod reduce-mseq :max [m _ mseq]
  (when (seq-metric? m)
    (apply max (map value mseq))))

(defmethod reduce-mseq :total [m _ mseq]
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
  (interval [this] this)
  (mget [this m rfn]
    (if (tracked? this m)
      (reduce-mseq m rfn (track this m))
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
  (metrics [_] (metrics segments))
  (tracked? [_ m] (tracked? segments m))
  (track [_ m] (track segments m))
  (interval [this] this)
  (mget [_  m rfn] (mget segments m rfn)))

(extend-type clojure.lang.ISeq
  IMeasured
  (metrics [this]
    (cond 
     (every? measured? this)    (->> this (mapcat metrics) distinct)
     (every? measurement? this) (->> this (mapcat metric) distinct)))
  (track [this m]
    (cond
     (every? measured? this) (mapcat #(track % m) this)
     (every? measurement? this) (filter #(= (metric %) m) this)))
  (tracked? [this m]
    (not (empty? (track this m))))
  (interval [this]
    (let [sval (cond
                (every? measurement? this) inst
                (every? measured? this) dtstart)
          sthis (sort #(compare (sval %1) (sval %2)) this)]
      (reify IInterval
        (dtstart [this]
          (when-let [f (first sthis)]
            (sval f)))
        (duration [this]
          (when-let [f (first sthis)]
            (time/in-seconds
             (time/interval (sval f) (sval (last sthis)))))))))
  (mget [this m rfn]
    (reduce-mseq
     m rfn
     (if (every? measured? this)
       (remove nil?
               (map
                #(when-let [v (mget % m rfn)]
                   (->Measurement (dtstart %) v m))
                (track this m)))
       this))))

;; ====================================================================

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
