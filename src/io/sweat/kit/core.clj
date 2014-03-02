(ns io.sweat.kit.core)

;; ==============================================================================
;; Vars

(def sport-metrics #{:hr :power :cadence :speed :position :distance})

;; ==============================================================================
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
  (points [this]))

(defprotocol ITrackPoint
  (inst [this])
  (value [this]))

(defprotocol ITracked
  (tracks [this])
  (metrics [this]))

(defprotocol ISports
  (sports [this]))

;; ==============================================================================
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
    (points [this] (map #(point % mtc) trk))))

(defn segment
  "Segment ctor"
  [s]
  (let [tpnts (for [t (:tracks s) tp t [k v] tp :when (not (nil? v))]
                (select-keys tp [:instant k]))
        trks (into {}
                   (for [sm sport-metrics
                         :let [tk (filter sm tpnts)]
                         :when (not (zero? (count tk)))]
                     {sm tk}))]
    (reify
      
      ISports
      (sports [_] [(:sport s)])

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
      
      ITracked
      (metrics [this] (keys (tracks this)))
      (tracks [this] trks))))

(defn activity
  "Activity ctor"
  [a]
  (let [sgmts (->> (:segments a)
                   (map segment)
                   (sort #(compare (dtstart %1) (dtstart %2))))]
    (reify 

      IAnnotation
      (title [_] (:title a))
      (notes [_] (:notes a))
      
      ISegmented
      (segments [_] sgmts)

      IInterval
      (trigger [_] :none)
      (dtstart [this]  (->> (segments this) first dtstart))
      (duration [this] (->> (segments this) (map duration) (reduce +)))
      (active? [this] (->> (segments this) (every? active?)))

      ISports
      (sports [this] (->> (segments this)
                          (mapcat sports)
                          (into [(:sport a)])
                          (remove nil?)
                          distinct)))))

(comment
  (def r (clojure.java.io/file "test-resources/FitnessHistoryDetail.tcx"))
  (def p (io.sweat.kit.parse.tcx/parse (.getPath r)))
  (def a (first (:activities p)))
  (def s (first (:segments a)))
  (def act (activity a))

  (sports act)
  (m (first (segments act)) :position)
  
  )
