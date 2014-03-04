(ns io.sweat.kit.parse.tcx
  "TCX to SweatKit format parser. Currently, it only supports simple activities (e.g no multisport nor workouts)"
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.data.zip.xml :as zip-xml :refer [xml-> xml1-> attr]]
            [clojure.java.io :as io]
            [clj-time.format :as time]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defmulti ^:private parse-loc #(-> % zip/node :tag))

(defmethod parse-loc :default [loc])

(defmethod parse-loc :Activity [act]
  {:dtstart (xml1->inst act :Id)
   :title (xml1->text act :Notes)
   :sports [(get sports (attr act :Sport))]
   :segments (for [lap (xml-> act :Lap)]
               (parse-loc lap))
   :notes (xml1->text act :Notes)})

(defmethod parse-loc :Lap [lap]
  {:dtstart (time/parse (attr lap :StartTime))
   :duration (xml1->double lap :TotalTimeSeconds)
   :distance (xml1->double lap :DistanceMeters)
   :calories (xml1->int lap :Calories)
   :active (= :active (get intensities (xml1->text lap :Intensity))) 
   :steps (xml1->int lap :Extensions :LX :Steps)
   :trigger (get lap-triggers (xml1->text lap :TriggerMethod)) 
   :notes (xml1->text lap :Notes)
   :hr {:avg (xml1->int lap :AverageHeartRateBpm :Value)
        :max (xml1->int lap :MaximumHeartRateBpm :Value)}
   :speed {:avg (xml1->double lap :Extensions :LX :AvgSpeed)
           :max (xml1->double lap :MaximumSpeed)}
   :cadence {:avg (or (xml1->int lap :Cadence)
                      (xml1->int lap :Extensions :LX :AvgRunCadence))
             :max (or (xml1->int lap :Extensions :LX :MaxBikeCadence)
                    (xml1->int lap :Extensions :LX :MaxRunCadence))}
   :tracks [(for [tpnt (xml-> lap :Track :Trackpoint)]
              (parse-loc tpnt))]})

(defmethod parse-loc :Trackpoint [tpnt]
  {:instant (xml1->inst tpnt :Time)
   :position {:lat (xml1->double tpnt :Position :LatitudeDegrees)
              :lng (xml1->double tpnt :Position :LongitudeDegrees)}
   :altitude (xml1->double tpnt :AltitudeMeters)
   :distance (xml1->double tpnt :DistanceMeters)
   :hr (xml1->int tpnt :HeartRateBpm :Value)
   :cadence (or (xml1->int tpnt :Cadence)
                (xml1->int tpnt :Extensions :TPX :RunCadence))
   :power (xml1->double tpnt :Extensions :TPX :Watts)
   :speed (xml1->double tpnt :Extensions :TPX :Speed)})
   
(defn parse [tcx]
  (let [z (tcx-zip tcx)]
    {:activities (for [act (xml-> z :Activities :Activity)] 
                   (parse-loc act))}))                                        
(comment 
  (def r (io/file "test-resources/FitnessHistoryDetail.tcx"))
  (time (let [p (parse (.getPath r))] p))
  (clojure.pprint/pprint p))
