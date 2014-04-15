# sweatkit

sweatkit is a Clojure(Script) library to work with sports activities data.

**This is alpha software. The API and code structure are subject to change.**
*Comments and pull requests are more than welcome. Due to the project's
stage, please open an issue before submitting a PR so an approach can be
discussed prior to your work*

--

## Motivation and Goals

sweatkit is motivated by two unaddressed problems while trying to work with this kind of data:
- It is fundamentally _composable_
- It needs to be _portable_

By _composable_ we mean that sports activities data can/should be worked upon at different time scales in equivalent ways. That is, you should be able to use the same operations and get the same sort of information out of a few minutes of workout, an activity segment, an entire activity, or collections of these things. 

By _portable_ we mean that there's plenty to gain by using generic values for data representation, but in this subject area, where formats and silos abound, it's also necessary to have a simple and extensible representation that facilitates the transport to/from those other formats.

Thus, this project's goals are:
- To provide a set of abstractions that enable composable and reusable operations over sports activity data
- To provide facilities to get data from/to different sports activities formats
- To enable client-server applications working on this problem domain by targeting both Clojure and ClojureScript for most namespaces

## Usage

Leiningen dependency info and examples will be available here when there's an official release

## Documentation

- Concepts
    - Basic abstractions
    - measured
    - mseq
- Data Structures
    - Activity
    - Segment
    - Metrics
    - Point Value
- Examples
- API Docs

## Roadmap

sweatkit's development will be driven mostly by needs that arise while working on a related project (TBA soon). We have some ideas and 

- TCX format emitting
- FIT format parsing/emitting
- Distance & Altitude calculations from geo tracks
- Support for workout plans and courses
- Performance Analysis and Projections
- More sports types (testing & possible adaptations)

## License

Copyright © 2014 Daniel Zacarias

Distributed under the [Eclipse Public License 1.0](LICENSE). By using this software in any fashion, you are agreeing to be bound by the terms of this license.
