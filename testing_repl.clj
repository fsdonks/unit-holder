;;paste into marathon REPL
(load-file "/home/craig/workspace/unit-holder/unit-holder.clj")

(ns unit-holder)
;;location containing all of the excel test files
(def root "/home/craig/workspace/unit-holder/test-data/")

(load-file "/home/craig/workspace/unit-holder/tests.clj")

;;If tests fail, one way to compare in the REPL is like so:
(def generated (set (remove-index (make-demands-from (first (find-inputs 2 2)) ["Moke" "RC_NonBOG-War" "HLD"] 722 754))))
(def test-set (set (remove-index (enabled-demand (second (find-inputs 2 2))))))
;;Then get the differences
(clojure.set/difference generated test-set)
(clojure.set/difference test-set generated)
 
(def generated (set (remove-index (make-demands-from (get-file "2-input") ["Moke" "RC_NonBOG-War" "HLD"] 722 778))))
(def test-set (set (remove-index (enabled-demand (get-file "3-output")))))

;;in order to test parts of the code....
(require 'proc.demandanalysis)
(def wkbk-p "/home/craig/runs/peak_hold/test-recs.xlsx")
(proc.demandanalysis/peak-parts wkbk-p :periods [{:Name
                                                                "blah"
                                                                :FromDay
                                                                722
                                                                :ToDay
                                                                722}])

(require 'proc.supply)
(proc.supply/quants-by-compo wkbk-p)
