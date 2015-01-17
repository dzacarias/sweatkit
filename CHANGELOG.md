## 0.1.3
 * Moved to Clojure-only, due to lack of time to develop for both targets
 
## 0.1.2
 * TCX parsing now accepts Document objects, instead of just strings

## 0.1.1
 * Renamed the valid-sweat? fn to db?
 * Added activity?, segment? and measurement? predicates, to validate data structures for each concept
 * Added activity, segment and measurement fns, that take valid maps and return mostly identical new ones, implementing IMeasured or IPointValue. This allows for a finer grained usage of the API
 * TCX parser now uses sweatkit.core's API and returns an already reified data structure, as per the db, activity, segment and measurement fns

## 0.1.0
 * Initial release
