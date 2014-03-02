(ns io.sweat.kit.core)

;; ============================================================
;; Vars

(def sport-metrics #{:hr :power :cadence :speed :position :distance})

;; ============================================================
;; Protocols

(defprotocol IInterval
  (dtstart [this])
  (duration [this])
  (trigger [this])
  (active? [this]))

(defprotocol IAnnotation
  (title [this])
  (notes [this]))

(defprotocol ISegmented
  (segments [this]))

(defprotocol IMetric
  (m [this k & preds]))

(defprotocol IMetricTrack
  (metric [this])
  (points [this]))

(defprotocol IMultiTrack
  (tracks [this])
  (metrics [this]))

(defprotocol ITrackPoint
  (inst [this])
  (value [this]))

(defprotocol ISport
  (sport [this]))

(defprotocol IMultiSport
  (sports [this]))

;; ============================================================
;; Instance constructors

(defn point
  "Point ctor"
  [p metric]
  (reify 
    
    ITrackPoint
    (inst [this] (:instant p))
    (value [this] (metric p))))

(defn track
  "Track ctor"
  [trk mtc]
  (reify 

    IInterval
    (dtstart [this])
    (duration [this])
    (trigger [this])
    (active? [this])
    
    IMetric
    (m [this k & preds])

    IMetricTrack
    (metric [this] mtc)
    (points [this] (map #(point % mtc) trk))))

(defn segment
  "Segment ctor"
  [s]
  (reify
    
    ISport
    (sport [_] (:sport s))

    IInterval
    (trigger [_] (:trigger s))
    (dtstart [_] (:start s))
    (duration [_] (:duration s))
    (active? [_] (= :active s))

    IAnnotation
    (title [_] (:title s))
    (notes [_] (:notes s))
    
    IMetric
    (m [this k & preds]
      (k (tracks this)))
    
    IMultiTrack
    (metrics [this]
      (keys (tracks this)))
    (tracks [this]
      (let [tpnts (for [t (:tracks s) tp t [k v] tp :when (not (nil? v))]
                    (select-keys tp [:instant k]))]
        (into {}
              (for [sm sport-metrics
                    :let [tk (filter sm tpnts)]
                    :when (not (zero? (count tk)))]
                {sm tk}))))))

(defn activity
  "Activity ctor"
  [a]
  (reify 

    IAnnotation
    (title [_] (:title a))
    (notes [_] (:notes a))
    
    ISegmented
    (segments [_] (->> (:segments a)
                       (map segment)
                       (sort #(compare (dtstart %1) (dtstart %2)))))

    IInterval
    (trigger [_] :none)
    (dtstart [this]  (->> (segments this)
                          first
                          dtstart))
    (duration [this] (->> (segments this)
                          (map duration)
                          (reduce +)))
    (active? [this] (->> (segments this)
                         (every? active?)))

    IMultiSport
    (sports [this] (->> (segments this) (map sport)))))

(comment
  (def r (clojure.java.io/file "test-resources/FitnessHistoryDetail.tcx"))
  (def p (io.sweat.kit.parse.tcx/parse (.getPath r)))
  (def a (first (:activities p)))
  (def s (first (:segments a)))
  (def act (activity a))
  

  
  )