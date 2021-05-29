;;This file is intended to be loaded after unit-holder.clj,
;;and it will test unit-holder.clj

(ns unit-holder)
(use 'clojure.test)
;;location containing all of the excel test files
(def test-root "/home/craig/runs/peak_hold/")

(defn get-hold-demands
  "filters demand records for the hold demands."
  [demand-recs]
  (filter (fn [{:keys [DemandGroup]}] (= DemandGroup "peak_hold"))
          demand-recs))

(defn remove-index
  "remove DemandIndex from a collection of demand records since we
  don't care about it for testing purposes."
  [recs]
  (for [r recs] (dissoc r :DemandIndex (keyword "Title 10_32"))))

  ;;given a path to an input workbook with supply and demand, a path to
  ;;an output workbook with demand, and a vector of remaining arguments
  ;;to make-demand-from, check to see if the resulting records from make-demands-from called with
  ;;the input path and arguments equals the set of records in the output
  ;;workbook.

(def wkbk-p (str root "peak_hold_test-2-input.xlsx"))
(def out-p (str root "peak_hold_test-2-output.xlsx"))
(def demands ["Moke"])
(def forge-name "Moke")
(def start 722)
(def end 754)

(deftest one-hold
  (is (= (set (remove-index (make-demands-from wkbk-p demands forge-name start end)))
         (set (remove-index (enabled-demand out-p))))))

(run-tests 'unit-holder)
