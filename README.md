# sweatkit [![Build Status](https://travis-ci.org/dzacarias/sweatkit.svg?branch=master)](https://travis-ci.org/dzacarias/sweatkit)

sweatkit is a Clojure library to work with sports activities data.

**This is alpha software. The API and code structure are subject to change.**

*Comments and pull requests are more than welcome. Due to the project's
stage, please open an issue before submitting a PR so an approach can be
discussed prior to your work*

## Goals and Motivation ##

In a nutshell, sweatkit is a set of:

- Composable abstractions and functions to work with sports activity data
- Facilities to get data from/to different sports activities formats

The library is motivated by the need to get the same kind of information from this data, on different time-scales (e.g., from a few seconds of a run to a collection of runs). Also, since this area is prolific in proprietary data silos, we need to support parsing/emitting from/to the most common data formats.

## Documentation

- [Wiki](https://github.com/dzacarias/sweatkit/wiki)
- [API Docs](http://dzacarias.github.io/sweatkit/)

## Usage

### Installation

Using [Leiningen](http://leiningen.org), add this to your project's `:dependencies`:

```clojure
[sweatkit "0.1.3"]
```

### Some examples

Let's start by parsing a TCX file into sweatkit's format:

```clojure
(require '[sweatkit.formats.tcx :as tcx])
(require '[sweatkit.core :as s])
(require '[clojure.java.io :as io])

; Provide a TCX File/InputStream/String 
(def p (tcx/parse (io/file "FitnessHistoryDetail.tcx")))

=> (clojure.pprint/pprint p)
{:activities
  [{
    ; -> Starting instant
    :dtstart #<DateTime 2007-08-07T02:42:41.000Z>,
    ; -> Any extra info
    :annotations {:notes nil},
    ; -> The activity's segments (usually called "laps")
    :segments
    [{:metrics
      {
       ; -> Some metrics may only have global values
       :speed {:max 18.6828499}, 
       :calories {:total 285},
       :altitude
       ; -> When there's a track of values for a metric, it appears like this:
       {:track 
        [{:metric :altitude,
          :value 3.982666,
          :instant #<DateTime 2007-08-07T02:42:41.000Z>}
          ; ...
          ]},
       :distance
       {:track
        [{:metric :distance,
          :value 0.0,
          :instant #<DateTime 2007-08-07T02:42:41.000Z>}
         {:metric :distance,
          :value 6.3073034,
          :instant #<DateTime 2007-08-07T02:42:48.000Z>}
         {:metric :distance,
          :value 7.5551758,
          :instant #<DateTime 2007-08-07T02:42:54.000Z>}
          ; ...
          }]}
      },
      :annotations {:notes nil},
      ; -> What caused this segment to be created (manually or some metric)
      :trigger :manual,
      ; -> It was an active (not resting) period
      :active true,
      ; -> How long it was, in seconds
      :duration 2325.02,
      ; -> When it started
      :dtstart #<DateTime 2007-08-07T02:42:41.000Z>,
      ; -> A segment may only refer to a single sport
      :sport :running}]}]}
```

A basic concept in sweatkit is the "measured" abstraction (through the IMeasured protocol). It represents something that has a set of sports metrics recorded over a time interval. Most of the public functions take "measured" objects. 

To start using sweatkit, you take a map like the one described above and feed it into the sweatkit.core/db function. It will return a mostly identical new map where each activity will implement the IMeasured protocol:

```clojure
; Get the first activity from the parsed structure
(def act (-> p :activities first))

; The activity is a measured object that may be fed into other fns
(s/measured? act)
;; => true

; Get the available metrics
(s/metrics act)
;; => (:calories :speed :position :distance :altitude)

; Get the average altitude
(s/altitude act :avg)
;; => 8.129691

; Get the max speed
(s/speed act :max)
;; => 18.6828499
;; These are meters/second, by the way (sweatkit uses metric units everywhere).

; Trying to a get a metric value that doesn't exist
(s/speed act :min)
;; => nil
```

Sequential collections containing IMeasured or IPointValue objects are extended to implement IMeasured, which allows you to do stuff like this:

```clojure
; Getting a full metric track (seq of point values)
(s/altitude act)
;; => (#sweatkit.core.PointValue{:instant #<DateTime 2007-08-07T02:42:41.000Z>,
;                                :value 3.982666,
;                                :metric :altitude}
;      #sweatkit.core.PointValue{:instant #<DateTime 2007-08-07T02:42:48.000Z>,
;                                :value 4.4632568,
;                                :metric :altitude}
;      ...)

; The seq implements IMeasured
(s/measured? (take 3 (s/altitude act)))
;; => true

; Get the average altitude for the activity's first 10 trackpoints (each an IPointValue)
(s/altitude (take 10 (s/altitude act)) :avg)

; Get the activity's splits every 1000 meters (returns a vector of IMeasured elems)
(def sp (s/splits act :distance 1000))

(count sp)
; => 9

; Since vectors implement IMeasured, you can query them using the standard fns:
(s/metrics sp)
; => (:altitude :position :distance)

; Notice that the set of metrics is not the same as the activity's. When splitting
; an IMeasured, you only get back the metrics with tracks, because already
; reduced values (like max or avg) can't be split.

; Since every split implements IMeasured, you can query it like anything else
; Let's get the average altitude for the first 1K split:
(s/altitude (first sp) :avg)
;; => 8.40672815097235
```
## Roadmap

To give you an idea of where we're headed, here's a list of features that we'd like to add over time:

- TCX format emitting
- Support for workout plans and courses
- Performance Analysis and Projections
- Distance & Altitude calculations from geo tracks
- More sports types (testing & possible adaptations)

## License

Copyright © 2014-2015 Daniel Zacarias

Distributed under the [Eclipse Public License 1.0](LICENSE). By using this software in any fashion, you are agreeing to be bound by the terms of this license.
