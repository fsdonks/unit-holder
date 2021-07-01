;;This namespace is intended to be loaded from the MARATHON repl.
;;The purpose of this namespace is to set aside units at the beginning
;;of conflict so that they can deploy to a specified set of demands as
;;those demands build over time.
;;At this time, the simplest approach seems to be to just add hold
;;demands to DemandRecords.

;;example invocation from the MARATHON repl:
(comment
  (load-file "/home/craig/workspace/unit-holder/unit-holder.clj")
;;The rest of this is just to run tests if you want.
(ns unit-holder)
;;location containing all of the excel test files
(def root "/home/craig/workspace/unit-holder/test-data/")
(load-file "/home/craig/workspace/unit-holder/tests.clj")
)

(ns unit-holder
  (:require [proc.demandanalysis :as analyzer]
            [proc.supply :as supply]
            [proc.util :as util]
            [spork.util [io :as io] [table :as tbl]]
            [marathon.project.excel :as xl]))


(defn sum-demands
  "Given a sequence of demand names, add the quantity of all demands
  together, sampled at time t in demand-records"
  [demand-records demands t]
  (let [;;demand-group-key would go here (and in args)
        sample (analyzer/peak-parts demand-records
                                    :periods [{:Name "time t"
                                               :FromDay t
                                               :ToDay t}])
        _ (println "t: " t)
        _ (println "first demand: " (first demand-records))
        _ (println "sample: " (first sample))]
    ;;Get all demand quantities
    ;;from the sample and add them
    ;;together.
    (if (empty? sample)
      0       ;none of the demands are active at time t
     ;;at least one of the demands are active at time t
      (reduce + (filter identity (map (first sample) demands)))
      )))
   
(defn init-hold
  "Compute the number of initial units to hold."
  [demand-records supply-map demands start-day end-day]
  (let [;;priority demand combined quantity on the start day
        init-demands (sum-demands demand-records demands start-day)
                                        ;pull inventories from the map
        ;these inventories will be 0 if they don't exist.
        [total NG RC AC] (supply/return-inv supply-map)
        leftovers (- (+ (+ NG RC) AC)
                     ;;highest priority on the start day
                     init-demands)
        ;The sum of the quantity from all demands in demands at the
        ;end day.
        end-demands (sum-demands demand-records demands end-day)
        ]
    ;;leftovers might be negative or 0
      (min leftovers (- end-demands init-demands))
      ))

(def hold-record
{:DemandGroup "peak_hold"
 :SRC :text,
 :Operation "peak_hold"
 :DemandIndex 0
 :SourceFirst "Uniform"
 :Overlap 45,
 :Priority 3,
 :Category "NonBOG"
 (keyword "Title 10_32") 10,
 :StartDay :int,
 :OITitle :text,
 :Duration :int,
 :Enabled true
 :Vignette "peak_hold"
 :Type "DemandRecord",
 :Quantity :int})

(defn check-forge
  "Given a sequence of forge demands and an end day, return an empty
  vector if the first forge demand starts on a day that is greater
  than the end day.  Else, returns all forge demands."
  [forge-demands start-day end-day]
  (let [;;only need the forge demands that are >= StartDay
        rest-forges (filter (fn [r]  (> (:StartDay r) start-day))
                            forge-demands)]
  (cond (empty? rest-forges) []
        (> (:StartDay (first rest-forges)) end-day) []
        :else rest-forges)))

(defn add-info
  "Add unique SRC and OITitle to the hold records"
  [recs src title]
  (if (empty? recs)
    recs
    (map (fn [r] (assoc r :SRC src :OITitle title)) recs)))

(defn new-hold-record
  "We have encountered a change in the quantity of the priority demand
  groups, so we take whatever the current hold record looks like, and
  update the start day and quantity according to the record indicating
  the change in quantity."
  [curr-hold-rec delta-rec]
  (let [;;quantity that is reduced by how
        ;;much the quantity of the priority demand
        ;;group increased.
        new-quantity (- (:Quantity curr-hold-rec)
                        (:Quantity delta-rec))]                           
        ;;Allow quantity to be non-positive.  It just won't be added
        ;;to the collection of hold-demands in make-demands.
    (assoc curr-hold-rec :StartDay (:StartDay delta-rec)
           :Quantity new-quantity)))
        

;;Assumptions:
;;1)end of peak hold is on a forge demand start day (hence the > in check-forge)
;;2)There are a grouping of highest priority demands.  So every time
;;the quantity of that group increases, the quantity of peak hold
;;decreases. Listing out the priority demands might be verbose as
;;opposed to just specifying a target priority, but this is okay for now.

;;Note that this may work if one of the priority demands decreases as
;;well, but that hasn't been tested.  For now, they're likely to only increase.
(defn make-demands
  "This function creates the actual demand records to hold a number of
  units in order to meet the peak demand.  The results demand records
  should be concatenated to the existing demand records for this SRC.
  Expect demand-records to be filtered for one SRC already. supply-map
  is a map of compo to quantity for this SRC only.
  demands is a sequence of strings containing the name of demand
  groups to hold units for. end-day is an int that indicates which day
  to stop the peak hold demand (inclusive.  end-day can be manual for now).
  Forge is the only demand type that steps up, so we will decrease the
  hold demand every time that increases.  The forge demand name is a
  string for forge-name."
  [demand-records supply-map demands start-day end-day]
  (let [;;the number of units currently held for the main demand
           init-demand (init-hold demand-records supply-map demands
                                  start-day end-day)
        _ (println "init-demand=" init-demand)
        ;;every time forge quantity increases, decrease the peak
           ;;hold demand. If this is the last forge-demand with an
           ;;end day of end-day, then stop and concat the peak hold
        ;;records with the original demand records
        ;;if forge record on first day, remove it since it's already
        ;;accounted for in forge demand.
        ;;demand-group-key should be used here, too
        forge-demands (->> demand-records
                              (filter (fn [{:keys [DemandGroup]}]
                                        (contains? (set demands)
                                                   DemandGroup)))
                              (analyzer/add-deltas)
                              (map (fn [[day recs]]
                                     (assoc (first recs)
                                                   :Quantity
                                                   (apply + (map #(:Quantity %) recs)))))
                              (sort-by :StartDay)
                              ((fn [recs] (let [res (check-forge recs
                                                       start-day
                                                       end-day)]
                                            (println "forges " res)
                                            res))))
        ;;used to set the src of the hold demands
        src (:SRC (first demand-records))
        title (:OITitle (first demand-records))]
    (if (or (empty? forge-demands) (<= init-demand 0))
      ;;no hold demands to add
      demand-records
      (loop [;;what is the current forge demand.  Will use to detect
             ;;quantity change.
             curr-forge (sum-demands demand-records demands
                                     start-day)
             
             ;;a hold record that holds the start day of another peak
             ;;hold demand. When a change in forge quantity is
             ;;detected or we have come to the end day of the hold
             ;;period, the duration is added to this record, and then
             ;;the record is added to hold-demands.
             curr-hold-rec 
                             ;;we started with some quantity
                             (assoc
                              hold-record
                              :StartDay
                              start-day
                              :Quantity
                              init-demand)
             hold-demands []
             leftover-forge (if
                                (= curr-forge 0)
                              forge-demands
                              ;;otherwise, we don't need to process
                              ;;the first forge demand
                              (rest forge-demands))
                              
             ]
        (println "curr-forge: " curr-forge)
        (println "new quantity: " (:Quantity (first leftover-forge)))
        (println "first leftover: " (first leftover-forge))
        (cond (or (empty? leftover-forge)
                  ;;could happen with low forge demand.
                  ;;huh?
                  ;;(<= (:Quantity curr-hold-rec))
                  )
              (concat demand-records (add-info hold-demands src title))

              ;;no quantity change
              (= (:Quantity (first leftover-forge)) 0)
              (recur curr-forge curr-hold-rec hold-demands
                     (check-forge (rest
                                   leftover-forge)  start-day end-day))
              ;;new quantity          
              (not= (:Quantity (first leftover-forge)) 0)
              ;;change current forge
              (recur (+ curr-forge (:Quantity (first leftover-forge)))
                     ;;start a new curr-hold-rec with a start day based on the
                     ;;new forge record
                     (new-hold-record curr-hold-rec (first leftover-forge))
                     ;;add the new record to our collection of
                     ;;hold-demands and update the duration
                     ;;BUT don't add it if quantity of the
                     ;;curr-hold-rec is non positive (like maybe the
                     ;;quantity of the priority demand group increase
                     ;;above what the quantity will be at the
                     ;;specified end.
                     (if (pos? (:Quantity curr-hold-rec))
                       (conj hold-demands (assoc curr-hold-rec
                                               :Duration (- (:StartDay
                                                             (first
                                                             leftover-forge))
                                                             (:StartDay
                                                              curr-hold-rec))))
                       hold-demands)
                     (check-forge (rest leftover-forge) start-day end-day))
              ;;we shouldn't get here.
              :else
              (throw (Exception. "A case should have matched.")))))))

(defn enabled-demand
  "Parse the DemandRecords from an Excel workbook just like we do for
  MARATHON and only keep the enabled records."
  [wkbk-path]
  (filter (fn [r] (:Enabled r)) (tbl/table-records (:DemandRecords
                      (xl/marathon-book->marathon-tables wkbk-path)))))

;;Need to see if we can use this out of the box for a DemandRecords
;;transform in m4.
(defn make-demands-from
  "Given the path to a workbook containing DemandRecords and
  SupplyRecords, return new DemandRecords with added demand used to
  hold units for the demand at end-day."
  [wkbk-path demands start-day end-day]
  (let [demand (enabled-demand wkbk-path)
        demand-by-src (group-by :SRC demand)
        ;map of "SRC" to {"AC" int, "NG" int, "RC" int}
        supply-map (supply/quants-by-compo wkbk-path)]
    (reduce concat
            (for [[src demand-records] demand-by-src]
              (make-demands demand-records (supply-map src)
                            demands start-day end-day)))))
      
