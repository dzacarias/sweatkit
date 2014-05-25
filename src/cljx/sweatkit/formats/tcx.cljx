(ns sweatkit.formats.tcx
  "TCX to sweatkit format parser. There's only support for Activity nodes (with
   MultiSportSessions coming on future release). Workouts and Courses will only
   be supported if/when sweatkit.core is extended to those concepts"
  (:require [clojure.zip :as zip]
            [sweatkit.formats.impl.xml :as xml
             :refer (xml1->double xml1->int xml1->inst xml1->text attr xml->)]
            [sweatkit.core :as sk]
            #+clj  [clojure.java.io :as io]
            #+clj  [clj-time.format :as time]
            #+cljs [cljs-time.format :as time]))

;; ==============================================================================
;; Private API

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

(defn- tcx-zip [tcx]
  #+clj
  (with-open [i (io/input-stream tcx)]
    (-> i xml/parse zip/xml-zip))
  #+cljs
  (-> tcx xml/parse zip/xml-zip))

(defn- get-track [tks metric]
  (filter metric tks))

(defn- get-metric [metric tks loc]
  "Takes a metric, a seq of trackpoints and a starting loc (as per c.zip).
   Returns a map with any global values (:max, :min, :avg, :total) for said
   metric and/or its track of point values over time"
  (when-let
      [m (merge
          (condp = metric
            :distance
            (when-let [t (xml1->double loc :DistanceMeters)] {:total t})
            :calories
            (when-let [t (xml1->int loc :Calories)] {:total t})
            :steps
            (when-let [t (xml1->int loc :Extensions :LX :Steps)] {:total t})
            :hr
            (merge (when-let [a (xml1->int loc :AverageHeartRateBpm :Value)]
                     {:avg a})
                   (when-let [m (xml1->int loc :MaximumHeartRateBpm :Value)]
                     {:max m}))
            :speed
            (merge (when-let [a (xml1->double loc :Extensions :LX :AvgSpeed)]
                     {:avg a})
                   (when-let [m (xml1->double loc :MaximumSpeed)]
                     {:max m}))
            :cadence
            (merge (when-let [a (or (xml1->int loc
                                               :Cadence)
                                    (xml1->int loc
                                               :Extensions :LX :AvgRunCadence))]
                     {:avg a})
                   (when-let [m (or (xml1->int loc
                                               :Extensions :LX :MaxBikeCadence)
                                    (xml1->int loc
                                               :Extensions :LX :MaxRunCadence))]
                     {:max m}))
            :power
            (merge (when-let [a (xml1->int loc :Extensions :p1:AverageWatts)]
                     {:avg a})
                   (when-let [m (xml1->int loc :Extensions :p1:MaxWatts)]
                     {:max m})) 
            nil)

          (when-let [tk (get-track tks metric)]
            (when-not (empty? tk) {:track tk})))]

    {metric m}))

(defmulti ^:private parse-loc
  "Parses loc elements (as from zip). Returns maps representing those elements
   in sweatkit format"
  #(-> % zip/node :tag))

(defmethod parse-loc :default [loc])

(defmethod parse-loc :Activity [act]
  "Parses Activity elements, returning a map for a sweatkit Activity"
  {:dtstart (xml1->inst act :Id)
   :annotations {:notes (xml1->text act :Notes)}
   :segments (for [lap (xml-> act :Lap)]
               (merge {:sport (get sports (attr act :Sport))}
                      (parse-loc lap)))})

(defmethod parse-loc :Lap [lap]
  "Parses Lap elements, returning a map representing a sweatkit Segment"
  (let [tks (for [tpnt (xml-> lap :Track :Trackpoint) :let [tp (parse-loc tpnt)]
                  [k v] tp :when (not (nil? v))]
              (select-keys tp [:instant k]))]
    {:dtstart     (time/parse (attr lap :StartTime))
     :duration    (xml1->double lap :TotalTimeSeconds)
     :active      (= :active (get intensities (xml1->text lap :Intensity))) 
     :trigger     (get lap-triggers (xml1->text lap :TriggerMethod)) 
     :annotations {:notes (xml1->text lap :Notes)}
     :metrics     (apply merge (map #(get-metric % tks lap)
                                    sk/metric-types))}))

(defmethod parse-loc :Trackpoint [tpnt]
  "Parses Trackpoint elements, returning a map with an :instant key
   and all metrics/values contained in it"
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
                            (xml1->int tpnt :Extensions :TPX :RunCadence)
                            (xml1->int tpnt :Extensions
                                       :ActivityTrackpointExtension 
                                       :RunCadence))]
           {:cadence cad})
         (when-let [pow (or (xml1->double tpnt :Extensions :TPX :Watts)
                            (xml1->double tpnt :Extensions :p1:Watts))]
           {:power pow})
         (when-let [spd (or (xml1->double tpnt :Extensions :TPX :Speed)
                            (xml1->double tpnt :Extensions
                                          :ActivityTrackpointExtension
                                          :Speed))]
           {:speed spd})))

;; =============================================================================
;; Public API

(defn parse [tcx]
  "Takes a param representing a TCX input, in any of the following types:
     - Clojure: File, URI, URL, Socket, byte array, or String.
     - ClojureScript: String
   Returns a map in sweatkit format representing the parsed input"
  (let [z (tcx-zip tcx)]
    {:activities (for [act (xml-> z :Activities :Activity)] 
                   (parse-loc act))}))
