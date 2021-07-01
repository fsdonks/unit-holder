;;This file is intended to be loaded after unit-holder.clj,
;;and it will test unit-holder.clj

(ns unit-holder)
(use 'clojure.test)

(defn get-hold-demands
  "filters demand records for the hold demands."
  [demand-recs]
  (filter (fn [{:keys [DemandGroup]}] (= DemandGroup "peak_hold"))
          demand-recs))

(defn remove-index
  "remove DemandIndex from a collection of demand records since we
  don't care about it for testing purposes."
  [recs]
  (for [r recs] (dissoc r :DemandIndex
                        (keyword "Title 10_32")
                        :Title10_32
                        :OriginalPriority)))

(defn get-file "Specify the name of a test to find with the standard
  extension and pre-text."
  [test-name]
  (str root "peak_hold_test-" test-name ".xlsx"))

(defn find-inputs "Given test in string and out-string, make the paths for the in-file
  and out-files."
  [in-string out-string]
  [(get-file (str in-string "-input"))
   (get-file (str out-string "-output"))])

  ;;given a path to an input workbook with supply and demand, a path to
  ;;an output workbook with demand, and a vector of remaining arguments
  ;;to make-demand-from, check to see if the resulting records from make-demands-from called with
  ;;the input path and arguments equals the set of records in the output
  ;;workbook.
(with-test
  (defn outputs=? [[in-file out-file] {:keys [demands start end]}]
      (= (set (remove-index (make-demands-from in-file demands start end)))
         (set (remove-index (enabled-demand out-file)))))
  (let [args-0 {:demands ["Moke" "RC_NonBOG-War" "HLD"] :start 722
                :end 754}
        args-4 {:demands ["Moke" "Hinny"] :start 722
                :end 785}]
    (testing "A simple, unchanging peak hold demand should be generated."
      (is (outputs=? (find-inputs 2 2) args-0)))
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
    (testing "Testing the first change in quantity, so we should have
    two peak hold records here."
      (is (outputs=? [(get-file "2-input") (get-file "3-output")]
                     (assoc args-0 :end 778))))
    (testing "Found a 0 quantity peak-hold test case that is passing
now. Testing start-day isn't on the first FORGE day, but it is on a
FORGE demand start day."
      (is (outputs=? [(get-file "4-input") (get-file "4-output")]
                     args-4)))
    (testing "Just another random visually-verified case with a
changing peak hold demand."
      (is (outputs=? (find-inputs 5 5)
                     args-4)))
    (testing "Just a large-sized run with many SRCs to see if anything
errors out since we got an error here previously."
      (is (make-demands-from (str root "base-testdata-v7.xlsx")
                              ["Hinny" "Moke"] 722 785)))
    ))

(run-tests 'unit-holder)
