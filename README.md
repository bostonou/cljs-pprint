# cljs-pprint
port of clojure.pprint to clojurescript

Clojars: `[bostonou/cljs-pprint "0.0.4-SNAPSHOT"]`

Pretty raw, but works for Lists, Vectors, and Maps. Falls back to `pr` for everything else.

Prints via `*print-fn*` (use cljs's `(enable-console-print!)` for easy logging to the console).

`[cljs.contrib.pprint :refer [pprint]]`
