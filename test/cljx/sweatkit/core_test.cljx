(ns sweatkit.core-test
  (:require [sweatkit.core :as sk]
            [sweatkit.formats.tcx :as tcx]
            [sweatkit.test-util :as util]
            #+clj  [clojure.test :as t :refer :all]  
            #+cljs [cemerick.cljs.test :as t]
            #+clj  [clj-time.coerce :as tc]
            #+cljs [cljs-time.coerce :as tc]
            #+clj  [clj-time.core :as time]
            #+cljs [cljs-time.core :as time])
  #+cljs
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)]))

;; -----------------------------------------------------------------------------
;; Unit tests

(defn- rand-inst
  [i dtstart duration]
  (tc/from-long (+ (tc/to-long dtstart)
                   (min (* 1000 duration)
                        (* 1000 (rand-int 5) (int i))))))


(defn- make-metric [m dtstart duration]
  (map #(hash-map :instant (rand-inst % dtstart duration)
                  m (Math/abs (* 10 (Math/sin %))))
       (range 0 360 0.25)))

(defn- =f
  "Approximate comparison for floating point numbers, because of JS"
  [& vals]
  (empty? (remove #(< % 0.000001) (map #(Math/abs (- (first vals) %)) vals))))

(def segment-1
  (let [dtstart (tc/from-string "2013-08-07T03:10:21Z")
        duration 4200]
    {:dtstart dtstart
     :duration duration
     :annotations {:title "A segment"}
     :sport :cycling
     :active true
     :trigger :manual
     :metrics {:speed {:max 4.3
                       :track (make-metric :speed dtstart duration)}
               :distance {:total 1000}
               :cadence {:avg 50}
               :power {:max 100}
               :steps {:track (map #(hash-map :steps %
                                              :instant (rand-inst %
                                                                  dtstart
                                                                  duration))
                                   (range 1 1000)) }}
     :other :stuff}))

(def segment-2
  (let [dtstart (tc/from-string "2013-08-07T04:20:14Z")
        duration 300]
    {:dtstart dtstart
     :duration duration
     :sport :running
     :active false
     :trigger :manual
     :metrics {:speed {:track (make-metric :speed dtstart duration)}
               :distance {:total 500}
               :steps {:track (map #(hash-map :steps %
                                              :instant (rand-inst %
                                                                  dtstart
                                                                  duration))
                                   (range 1 200)) }}
     :power {:min 20}
     :some "other"}))

(def segment-3
  (let [dtstart (tc/from-string "2013-08-07T04:25:14Z")
        duration 6000]
    {:dtstart dtstart
     :duration duration
     :sport :running
     :active true
     :metrics {:speed {:track (make-metric :speed dtstart duration)}
               :distance {:total 1500}}
     :trigger :speed}))

(def activity
  {:dtstart (tc/from-string "2013-08-07T03:10:21Z")
   :annotations {:title "An activity" :some :other}
   :segments [segment-1 segment-2 segment-3]})

(def sweat-db
  {:activities [activity]})

(deftest db-test
  (testing "Valid format should be built"
    (is (sk/db sweat-db)))
  (testing "Invalid format should return empty db"
    (let [db-1 (assoc-in sweat-db [:activities 0 :segments 1 :metrics] nil)]
      (is (not (sk/db db-1)))))
  (testing "Should return an mseq of Activities and each should be measured"
    (let [b (sk/db sweat-db)]
      (is (sk/measured? (:activities b)))
      (is (empty? (filter false? (map sk/measured? (:activities b))))))))

(deftest format-test
  (testing "Valid format should be true"
    (is (sk/valid-sweat? sweat-db)))
  (testing "Invalid format should be false"
    (let [db-1 (assoc-in sweat-db [:activities 0 :segments 0 :sport] nil)]
      (is (not (sk/valid-sweat? db-1))))))

(deftest splits-test
  (testing "Should not split if there's no metric track"
    (is (zero? (count (sk/splits
                       (-> (sk/db sweat-db) :activities first)
                       :distance
                       1000)))))
  (testing "Should not split if it's not an acc metric"
    (is (zero? (count (sk/splits
                       (-> (sk/db sweat-db) :activities first)
                       :speed
                       1000)))))
  (let [a (-> (util/read-file "test-resources/tcx/FitnessHistoryDetail.tcx")
              tcx/parse sk/db :activities first)]
    (testing "Total splits should be correct for given metric split value"
      (is (= 9 (count (sk/splits a :distance 1000))))
      (is (= 84 (count (sk/splits a :distance 100)))))
    (testing "Each split should have the given size, except for the last one"
      (is (zero? (count (filter #(not= 100.0 (double %))
                                (drop-last (map #(sk/distance % :total)
                                                (sk/splits a :distance 100)))))))
      (is (zero? (count (filter #(not= 1000.0 (double %))
                                (drop-last (map #(sk/distance % :total)
                                                (sk/splits a :distance 1000))))))))
    (testing "Every metric should have the same dtstart/end as the split metric"
      (doseq [x (sk/splits a :distance 1000)]
        (let [st (sk/inst (first (sk/track x :distance)))
              ed (sk/inst (last (sk/track x :distance)))]
          (doseq [m (remove #(= % :distance) (sk/metrics x))]
            (is (and (= st (sk/inst (first (sk/track x m))))
                     (= ed (sk/inst (last (sk/track x m))))))))))
    (testing "At split frontiers, values should be interpolated"
      (let [sp (sk/splits a :distance 900)
            a1 (last (drop-last (sk/altitude (first sp))))
            i1 (last (sk/altitude (first sp)))
            a2 (second (sk/altitude (second sp)))
            i2 (first (sk/altitude (second sp)))]
        (testing "Split frontiers should be equal"
          (is (= i1 i2))
          (is (= (sk/value a1) (sk/value a2))))
        (testing "Frontier instant should come from split metric"
          (is (= (sk/inst i1) (sk/inst (last (sk/distance (first sp)))))))
        ; Linear interpolation:
        ; (double (+ y0 (* (- y1 y0) (/ (- x x0) (- x1 x0)))))
        (let [y0 (sk/value a1)
              y1 (sk/value a2)
              x (tc/to-long (sk/inst i1))
              x0 (tc/to-long (sk/inst a1))
              x1 (tc/to-long (sk/inst a2))]
          (is (= (sk/value i1)
                 (double (+ y0 (* (- y1 y0) (/ (- x x0) (- x1 x0))))))))))
    (testing "At every split, metric value should be <= input split value"
      (doseq [s (sk/splits a :distance 800)]
        (is (zero? (count (filter #(> (sk/value %) 800) (sk/distance s)))))))
    (testing "Only tracked metrics can be split"
      (let [ntm (set (remove #(sk/tracked? a %) (sk/metrics a)))]
        (doseq [sp (sk/splits a :distance 1000)
                m (sk/metrics sp)]
          (is (not (contains? ntm m))))))))

(deftest metrics-test
  (let [b (sk/db sweat-db)
        ref-act (-> b :activities first)
        acts [ref-act ref-act ref-act]]
    (testing "Already reduced value should be used instead of track"
      (is (= 4.3 (-> b :activities first :segments first (sk/speed :max)))))
    (testing "If track-only, return computed reduced value"
      (let [s (-> b :activities first :segments second)]
        (is (= (keys (-> s :metrics :speed)) '(:track)))
        (is (not (nil? (sk/speed s :avg))))))
    (testing "Using the helper fn should be the same as using mreduce"
      (is (= (sk/mreduce ref-act :speed :avg) (sk/speed ref-act :avg)))
      (is (= (sk/mreduce ref-act :speed :min) (sk/speed ref-act :min)))
      (is (= (sk/mreduce ref-act :speed :max) (sk/speed ref-act :max)))
      (is (= (sk/mreduce ref-act :distance :total) (sk/distance ref-act :total)))
      (is (= (sk/mreduce ref-act :cadence :avg) (sk/cadence ref-act :avg)))
      (is (= (sk/mreduce ref-act :cadence :min) (sk/cadence ref-act :min)))
      (is (= (sk/mreduce ref-act :cadence :max) (sk/cadence ref-act :max)))
      (is (= (sk/mreduce ref-act :power :avg) (sk/power ref-act :avg)))
      (is (= (sk/mreduce ref-act :power :min) (sk/power ref-act :min)))
      (is (= (sk/mreduce ref-act :power :max) (sk/power ref-act :max))))
    (testing "Min should return the smallest value"
      (is (= (sk/speed ref-act :min)
             (apply min (map #(sk/value %) (sk/speed ref-act))))))
    (testing "Max should return the largest value"
      (is (= (sk/speed ref-act :max)
             (apply max (map #(sk/value %) (sk/speed ref-act))))))
    (testing "Measured sequence with a repeating measured element"
      (testing "Duration is equal to to the reference activity (has the same dtstart/dtend)"
        (is (= (-> acts sk/interval sk/duration)
               (-> ref-act sk/interval sk/duration))))
      (testing "Total distance should be the sum of each measured element"
        (is (= (sk/mreduce acts :distance :total)
               (* (count (seq acts)) (sk/mreduce ref-act :distance :total)))))
      (testing "Max, Min and Avg values should be the same as the reference activity"
        (is (= (sk/mreduce acts :speed :max)
               (sk/mreduce ref-act :speed :max)))
        (is (=f (sk/mreduce acts :speed :avg)
                (sk/mreduce ref-act :speed :avg))))))
  (testing "Different measured elem sequence"
    (let [a-1 (assoc-in activity [:segments] [segment-1])
          a-2 (assoc-in activity [:segments] [segment-2 segment-3])
          a-3 (assoc-in activity [:segments] [segment-3])
          a-4 (assoc-in activity [:segments] [segment-1 segment-3])
          a-5 (assoc-in activity [:segments] [segment-2])
          b (sk/db {:activities [a-1 a-2 a-3 a-4 a-5]})
          acts (:activities b)]
      (testing "Max should be from the right activity"
        (is (= (sk/mreduce acts :power :max) 
               (sk/mreduce (nth (:activities b) 0) :power :max)))
        (is (= (sk/mreduce acts :power :max) 
               (sk/mreduce (nth (:activities b) 3) :power :max))))
      (testing "Min should be from the right activity"
        (is (= (sk/mreduce acts :power :min) 
               (sk/mreduce (nth (:activities b) 1) :power :min)))
        (is (= (sk/mreduce acts :power :min) 
               (sk/mreduce (nth (:activities b) 4) :power :min))))
      (testing "Total should be the sum of all activities"
        (is (= (reduce + (map #(get-in % [:metrics :distance :total])
                              (mapcat :segments acts)))
               (sk/mreduce acts :distance :total))))
      (testing "Average should be weighted by each activity's duration"
        (let [vals (map #(hash-map :val (* (-> % sk/interval sk/duration)
                                           (sk/mreduce % :speed :avg))
                                   
                                   :dur (-> % sk/interval sk/duration))
                        acts)]
          (is (=f (sk/mreduce acts :speed :avg)
                  (float (/ (apply + (map :val vals))
                            (apply + (map :dur vals)))))))))))

(deftest predicates-test
  (testing "Should be true for a measured"
    (let [v (reify sk/IMeasured)]
      (is (sk/measured? v))))
  (testing "Should be true for (Lazy)Seqs, Vectors and Lists"
    (let [act (-> (sk/db sweat-db) :activities first)]
      (is (sk/measured? [act act]))
      (is (sk/measured? (list act act act)))
      (is (sk/measured? (seq [act act])))
      (is (sk/measured? (lazy-seq [act act])))))
  (testing "Should be false for any non-measured"
    (is (not (sk/measured? nil)))
    (is (not (sk/measured? [])))
    (is (not (sk/measured? {})))
    (is (not (sk/measured? '())))
    (is (not (sk/measured? "abc")))
    (is (not (sk/measured? 123))))
  (testing "Should be true for a point-val"
    (let [v (reify sk/IPointValue)]
      (is (sk/point-val? v))))
  (testing "Should be false for any non point-val"
    (is (not (sk/point-val? nil)))
    (is (not (sk/point-val? [])))
    (is (not (sk/point-val? {})))
    (is (not (sk/point-val? '())))
    (is (not (sk/point-val? "abc")))
    (is (not (sk/point-val? 123)))))



