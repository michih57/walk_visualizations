(ns walk-visualizations.test-runner
  (:require
   [doo.runner :refer-macros [doo-tests]]
   [walk-visualizations.core-test]))

(enable-console-print!)

(doo-tests 'walk-visualizations.core-test)
