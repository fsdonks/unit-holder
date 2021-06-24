;;This file is intended to be loaded after unit-holder.clj,
;;and it will test unit-holder.clj

(ns unit-holder)
(use 'clojure.test)
;;location containing all of the excel test files
(def root "/home/craig/runs/peak_hold/")

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

(defn find-inputs "Given test in string and out-string, make the paths for the in-file
  and out-files."
  [in-string out-string]
    [(str root "peak_hold_test-" in-string "-" "input.xlsx")
     (str root "peak_hold_test-" out-string "-" "output.xlsx")])

  ;;given a path to an input workbook with supply and demand, a path to
  ;;an output workbook with demand, and a vector of remaining arguments
  ;;to make-demand-from, check to see if the resulting records from make-demands-from called with
  ;;the input path and arguments equals the set of records in the output
  ;;workbook.
(with-test
  (defn outputs=? [[in-file out-file] {:keys [demands forge-name start end]}]
      (= (set (remove-index (make-demands-from in-file demands forge-name start end)))
         (set (remove-index (enabled-demand out-file)))))
  (let [args-0 {:demands ["Moke" "RC_NonBOG-War" "HLD"] :forge-name "Moke" :start 722 :end 754}]
    (is (outputs=? (find-inputs 2 2) args-0))
    (testing "Doesn't end on a FORGE start day."
      ;;here, maybe we should extend the end day on the hold demand, but we dont.
      ;;Unlikely to happen, so leave it alone for now.
      (is (outputs=? (find-inputs 2 2) (assoc args-0 :end 755)))
      ;;Here, we should shorten the hold demand, but we don't.  End
      ;;day is from the last FORGE record.
      (is (outputs=? (find-inputs 2 2) (assoc args-0 :end 753)))
      )
    (testing "Even if we add OffFORGE, the delta is still 3, so
initial demand should still be 3."
      (is (outputs=? (find-inputs 2 2)
                     (assoc args-0 :demands (conj (:demands args-0)
                                                  "OffFORGE"))))
          
      )
    )
  )

(run-tests 'unit-holder)
