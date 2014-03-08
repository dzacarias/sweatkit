(ns io.sweat.kit.parse.tcx
  "TCX to SweatKit format parser. Currently, it only supports simple activities (e.g no multisport nor workouts)"
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.data.zip.xml :as zip-xml :refer [xml-> xml1-> attr]]
            [clojure.java.io :as io]
            [clj-time.format :as time]
            [io.sweat.kit.core :as sk]))

;; ==============================================================================
;; Helpers

(defn- tcx-zip [tcx]
  (-> tcx io/file xml/parse zip/xml-zip))

(defn- parse-double [txt] (Double/parseDouble txt))

(defn- parse-int [txt] (Integer/parseInt txt))

(defn- xml1->text [loc & preds] 
  (some-> (apply xml1-> loc preds) zip-xml/text))

(defn- xml1->double [loc & preds]
  (some-> (apply xml1->text loc preds) parse-double))

(defn- xml1->int [loc & preds]
  (some-> (apply xml1->text loc preds) parse-int))

(defn- xml1->inst [loc & preds]
  (some-> (apply xml1->text loc preds) time/parse))

;; ==============================================================================
;; Main 

(def ^:private sports
  {"Running" :running
   "Biking"  :cycling
   "Other"   :other})

(def ^:private lap-triggers
  {"Manual"    :manual
   "Distance"  :distance
   "Location"  :location
   "Time"      :time
   "HeartRate" :hr})

(def ^:private intensities
  {"Active" :active
   "Resting" :resting})

(defn- get-track [tks metric]
  (map #(sk/->Measurement (:instant %) (metric %) metric)
       (filter metric tks)))

(defn- get-metric [metric tks loc]
  (when-let
      [m (merge
          (condp = metric
            :distance (when-let [t (xml1->double loc :DistanceMeters)] {:total t})
            :calories (when-let [t (xml1->int loc :Calories)] {:total t})
            :steps    (when-let [t (xml1->int loc :Extensions :LX :Steps)] {:total t})
            :hr       (merge (when-let [a (xml1->int loc :AverageHeartRateBpm :Value)]
                               {:avg a})
                             (when-let [m (xml1->int loc :MaximumHeartRateBpm :Value)]
                               {:max m}))
            :speed    (merge (when-let [a (xml1->double loc :Extensions :LX :AvgSpeed)]
                               {:avg a})
                             (when-let [m (xml1->double loc :MaximumSpeed)]
                               {:max m}))
            :cadence  (merge (when-let [a (or (xml1->int loc :Cadence)
                                              (xml1->int loc :Extensions :LX :AvgRunCadence))]
                               {:avg a})
                             (when-let [m (or (xml1->int loc :Extensions :LX :MaxBikeCadence)
                                              (xml1->int loc :Extensions :LX :MaxRunCadence))]
                               {:max m}))
            nil)
          (when-let [tk (get-track tks metric)]
            (when-not (empty? tk) {:track tk})))]
    {metric m}))

(defmulti ^:private parse-loc #(-> % zip/node :tag))

(defmethod parse-loc :default [loc])

(defmethod parse-loc :Activity [act]
  {:dtstart (xml1->inst act :Id)
   :annotations {:notes (xml1->text act :Notes)}
   :segments (for [lap (xml-> act :Lap)]
               (sk/map->Segment
                (merge {:sport (get sports (attr act :Sport))}
                       (parse-loc lap))))})

(defmethod parse-loc :Lap [lap]
  (let [tks (for [tpnt (xml-> lap :Track :Trackpoint) :let [tp (parse-loc tpnt)]
                  [k v] tp :when (not (nil? v))]
              (select-keys tp [:instant k]))]
    {:dtstart     (time/parse (attr lap :StartTime))
     :duration    (xml1->double lap :TotalTimeSeconds)
     :active      (= :active (get intensities (xml1->text lap :Intensity))) 
     :trigger     (get lap-triggers (xml1->text lap :TriggerMethod)) 
     :annotations {:notes (xml1->text lap :Notes)}
     :metrics     (apply merge (map #(get-metric % tks lap) sk/metric-types))}))

(defmethod parse-loc :Trackpoint [tpnt]
  (merge {:instant  (xml1->inst tpnt :Time)}
         (when-let [lat (xml1->double tpnt :Position :LatitudeDegrees)]
           {:position {:lat lat
                       :lng (xml1->double tpnt :Position :LongitudeDegrees)}})
         (when-let [alt (xml1->double tpnt :AltitudeMeters)]
           {:altitude alt})
         (when-let [dst (xml1->double tpnt :DistanceMeters)]
           {:distance dst})
         (when-let [hr (xml1->int tpnt :HeartRateBpm :Value)]
           {:hr hr})
         (when-let [cad (or (xml1->int tpnt :Cadence)
                            (xml1->int tpnt :Extensions :TPX :RunCadence))]
           {:cadence cad})
         (when-let [pow (xml1->double tpnt :Extensions :TPX :Watts)]
           {:power pow})
         (when-let [spd (xml1->double tpnt :Extensions :TPX :Speed)]
           {:speed spd})))

(defn parse [tcx]
  (let [z (tcx-zip tcx)]
    {:activities (for [act (xml-> z :Activities :Activity)] 
                   (sk/map->Activity (parse-loc act)))}))

(comment 
  (def r (io/file "test-resources/FitnessHistoryDetail.tcx"))
  (def p (parse (.getPath r))))
