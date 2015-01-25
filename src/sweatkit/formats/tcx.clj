(ns sweatkit.formats.tcx
  "TCX to sweatkit format parser and TCX emitter. Currently, the supported TCX nodes
   are: Activity and MultiSportSessions
   Workouts and Courses will only be supported if/when sweatkit.core is
   extended to those concepts"
  (:require [clojure.zip :as zip]
            [clojure.data.xml :as xml]
            [clojure.data.zip.xml :as dzip]
            [sweatkit.core :as sk]
            [clojure.java.io :as io]
            [clj-time.format :as time]))

;; =============================================================================
;; Private API

; ------------------------------------------------------------------------------
; XML helpers

(defn- xml1->text [loc & preds] 
  (some-> (apply dzip/xml1-> loc preds) zip/node :content first))

(defn- xml1->double [loc & preds]
  (some-> (apply xml1->text loc preds) Double/parseDouble))

(defn- xml1->int [loc & preds]
  (some-> (apply xml1->text loc preds) Integer/parseInt))

(defn- xml1->inst [loc & preds]
  (some-> (apply xml1->text loc preds) time/parse))

; ------------------------------------------------------------------------------
; TCX / sweatkit

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

; ------------------------------------------------------------------------------
; Parse TCX

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
            (merge (when-let [a (or (xml1->int loc :Extensions :AverageWatts)
                                    (xml1->int loc :Extensions :LX :AvgWatts))]
                     {:avg a})
                   (when-let [m (or (xml1->int loc :Extensions :MaxWatts)
                                    (xml1->int loc :Extensions :LX :MaxWatts))]
                     {:max m})) 
            nil)

          (when-let [tk (vec (map sk/measurement (get-track tks metric)))]
            (when-not (empty? tk) {:track tk})))]

    {metric m}))

(defmulti ^:private parse-loc
  "Parses loc elements (as from zip). Returns maps representing those elements
   in sweatkit format"
  #(-> % zip/node :tag))

(defmethod parse-loc :default [loc])

(defmethod parse-loc :Activity [act]
  "Parses Activity elements, returning a map for a sweatkit Activity"
  (sk/activity
   {:dtstart (xml1->inst act :Id)
    :annotations {:notes (xml1->text act :Notes)}
    :segments (vec (for [lap (dzip/xml-> act :Lap)]
                     (sk/segment
                      (merge {:sport (get sports (dzip/attr act :Sport))}
                             (parse-loc lap)))))}))

(defmethod parse-loc :Lap [lap]
  "Parses Lap elements, returning a map representing a sweatkit Segment"
  (let [tks (for [tpnt (dzip/xml-> lap :Track :Trackpoint)
                  :let [tp (parse-loc tpnt)]
                  [k v] tp :when (not (nil? v))]
              (select-keys tp [:instant k]))]
    {:dtstart     (time/parse (dzip/attr lap :StartTime))
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
                            (xml1->double tpnt :Extensions :Watts))]
           {:power pow})
         (when-let [spd (or (xml1->double tpnt :Extensions :TPX :Speed)
                            (xml1->double tpnt :Extensions
                                          :ActivityTrackpointExtension
                                          :Speed))]
           {:speed spd})))

; ------------------------------------------------------------------------------
; Emit TCX

(defn- key-name
  "Converts a sweatkit sport name keyword into TCX's string"
  [k container]
  (first (keep #(when (= k (val %)) (key %)) container)))

(defn- emit-datetime
  "Takes a clj-time datetime object and returns a formatted string as 
   date-time-no-ms"
  [dtime]
  (time/unparse (time/formatters :date-time-no-ms) dtime))

(defn- emit-trackpoints
  "Takes a sweatkit IMeasured object and a sport. Returns the data sexp
   for data.xml's emit corresponding to a single sequence of Trackpoints.
   The objective is to associate them to a single Track, thus producing a 
   smaller XML file"
  [m sport]
  (let [tks (->> (for [met (sk/metrics m)]
                   (sk/track m met))
                 flatten
                 (remove nil?)
                 (group-by #(sk/inst %)))]
    (for [[k pcoll] tks]
      [:Trackpoint
       [:Time (emit-datetime k)]
       (remove
        nil?
        (for [pval pcoll :let [v (sk/value pval)]]
          (condp = (sk/metric pval)
            :hr       [:HeartRateBpm {:xsi:type "HeartRateInBeatsPerMinute_t"}
                       [:Value v]]
            :cadence  (if (= sport :running)
                        [:Extensions [:TPX [:RunCadence v]]]
                        [:Cadence v])
            :power    [:Extensions [:TPX [:Watts v]]]
            :speed    [:Extensions [:TPX [:Speed v]]]
            :distance [:DistanceMeters v]
            :position [:Position
                       [:LatitudeDegrees (:lat v)]
                       [:LongitudeDegrees (:lng v)]]
            :altitude [:AltitudeMeters v]
            nil)))])))


(defn- emit-segment
  "Takes a sweatkit segment and returns the data sexp for data.xml's emit"
  [s]
  [:Lap {:StartTime (emit-datetime (sk/dtstart s))}
   (when-let [secs (sk/duration s)] [:TotalTimeSeconds secs]) 
   (when-let [dist (sk/distance s :total)] [:DistanceMeters dist])
   (when-let [spd (sk/speed s :max)] [:MaximumSpeed spd]) 
   (when-let [cals (sk/calories s :total)] [:Calories cals]) 
   (when-let [hr (sk/hr s :avg)]
     [:AverageHeartRateBpm {:xsi:type "HeartRateInBeatsPerMinute_t"} 
      [:Value hr]])
   (when-let [hr (sk/hr s :max)]
     [:MaximumHeartRateBpm {:xsi:type "HeartRateInBeatsPerMinute_t"}
      [:Value hr]])
   [:Intensity (if (:active s) "Active" "Resting")] 
   (when-let [trig (key-name (:trigger s) lap-triggers)]
     [:TriggerMethod trig])
   (when-let [notes (some-> s :annotations :notes)]
     [:Notes notes])
   (when-let [cad (sk/cadence s :avg)]
     (when (= :cycling (:sport s))
       [:Cadence cad]))
   (let [ext
         (remove
          nil?
          [(when-let [cad (sk/cadence s :avg)]
             (when (= :running (:sport s))
               [:AvgRunCadence {:xsi:type "CadenceValue_t"} cad]))
           (when-let [cad (sk/cadence s :max)]
             (condp = (:sport s)
               :running [:MaxRunCadence {:xsi:type "CadenceValue_t"} cad]
               :cycling [:MaxBikeCadence {:xsi:type "CadenceValue_t"} cad]
               nil))
           (when-let [pow (sk/power s :max)]
             [:MaxWatts pow])
           (when-let [pow (sk/power s :avg)]
             [:AvgWatts pow])
           (when-let [spd (sk/speed s :avg)]
             [:AvgSpeed spd])
           (when-let [steps (sk/steps s :total)]
             [:Steps steps])])]
     (when-not (empty? ext)
       [:Extensions
        [:LX {:xmlns "http://www.garmin.com/xmlschemas/ActivityExtension/v2"}
         ext]]))
   
   (when-let [tpts (emit-trackpoints s (:sport s))] [:Track tpts])])

(defn- emit-activity
  "Takes a sweatkit activity and returns the data sexp for data.xml's emit"
  [a]
  (if (= 1 (count (sk/sports a)))
    [:Activity {:Sport (key-name (first (sk/sports a)) sports)}
     [:Id (time/unparse (time/formatters :date-time-no-ms)
                        (:dtstart a))]
     (for [s (:segments a)] (emit-segment s))
     (when-let [notes (:annotations a)] [:Notes notes])]
    [:MultiSportSession {}]))

;; =============================================================================
;; Public API

(defn parse 
  "Takes a param representing a TCX input, in any of the following types:
   OutputStream, File, URI, URL, Socket or String.
  
   Returns a map in sweatkit format representing the parsed input"
  [tcx]
  (with-open [i (io/input-stream tcx)]
    (let [z (-> i xml/parse zip/xml-zip)]
      {:activities (vec (for [act (dzip/xml-> z :Activities :Activity)] 
                          (parse-loc act)))})))

(defn emit
  "Takes a sweatkit db and returns the TCX data as a clojure.data.xml
   Element or nil if is not in a valid format"
  [db]
  (when (sk/db? db)
    (xml/sexp-as-element
     [:TrainingCenterDatabase
      {:xmlns "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2"
       :xmlns:xsi "http://www.w3.org/2001/XMLSchema-instance"
       :xsi:schemaLocation "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2 http://www.garmin.com/xmlschemas/TrainingCenterDatabasev2.xsd"
       :xmlns:ns2 "http://www.garmin.com/xmlschemas/UserProfile/v2"
       :xmlns:ns5 "http://www.garmin.com/xmlschemas/ActivityGoals/v1"
       :xmlns:ns4 "http://www.garmin.com/xmlschemas/ProfileExtension/v1"
       :xmlns:tpx "http://www.garmin.com/xmlschemas/ActivityExtension/v2"}
      (for [a (:activities db)]
        [:Activities (emit-activity a)])
      [:Author {:xsi:type "Application_t"}
       [:Name "sweatkit"]
       [:Build 
        [:Version 
         [:VersionMajor 0]
         [:VersionMinor 0]
         [:BuildMajor 0]
         [:BuildMinor 0]]
        [:Type "Alpha"]
        [:Time (time/unparse (time/formatters :date-time-no-ms)
                             (clj-time.core/now))]
        [:Builder "sweatkit"]]
       [:LangID "EN"]]])))
