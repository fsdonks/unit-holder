;;This namespace is intended to be loaded from proc.
;;The purpose of this namespace is to set aside units at the beginning
;;of conflict so that they can deploy to a specified set of demands as
;;those demands build over time.
;;At this time, the simplest approach seems to be to just add hold
;;demands to DemandRecords.
(ns unit-holder
  (:require [proc.demandanalysis :as analyzer]
            [proc.supply :as supply]
            [proc.util :as util]
            [spork.util [io :as io] [table :as tbl]]))


(defn sum-demands
  "Given a sequence of demand names, add the quantity of all demands
  together, sampled at time t in demand-records"
  [demand-records demands t]
  (let [;;demand-group-key would go here (and in args)
        sample (analyzer/peak-parts demand-records
                                    :periods [{:Name "time t"
                                               :FromDay t
                                               :ToDay t}])]
    ;Get all demand quantities from the sample and add them together.
    (reduce + (filter identity (map (first sample) demands)))))
   
(defn init-hold
  "Compute the number of initial units to hold."
  [demand-records supply-map demands start-day end-day]
  (let [;InitialDefeat = (AOR+FORGE+OffFORGE)
        ;The sum of the quantity from all demands in demands at the
                                        ;start day.
        init-demands (sum-demands demand-records demands start-day)
                                        ;pull inventories from the map
        ;these inventories will be 0 if they don't exist.
        [total NG RC AC] (supply/return-inv supply-map)
        num-cannibalized (sum-demands demand-records ["RC_NonBOG-War"]
                                      start-day)
        num-hld (sum-demands demand-records ["HLD"] start-day)
        ;LeftoverQuantity= RCSupply + ACSupply - NumRCCannibalized -
                                        ;HLD - InitialDefeat
        leftovers (- (+ (+ NG RC) AC)
                     num-cannibalized
                     num-hld
                     init-demands)
                                        ;PeakDefeat =(AOR+FORGE+OffFORGE)
        ;The sum of the quantity from all demands in demands at the
        ;end day.
        end-demands (sum-demands demand-records demands end-day)
        ]
    ;PeakHold = If (LeftoverQuantity>0) Min(LeftoverQuantity,
                                        ;PeakDefeat)
    ;;might be negative or 0
      (min leftovers end-demands)
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
  [forge-demands end-day]
  (cond (empty? forge-demands) []
        (> (:StartDay (first forge-demands)) end-day) []
        :else forge-demands))

(defn add-info
  "Add unique SRC and OITitle to the hold records"
  [recs src title]
  (if (empty? recs)
    recs
    (map (fn [r] (assoc r :SRC src :OITitle title)) recs)))

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
  [demand-records supply-map demands forge-name start-day end-day]
  (let [;;the number of units currently held for the main demand
           init-demand (init-hold demand-records supply-map demands
                                  start-day end-day)
        _ (println init-demand)
        ;;every time forge quantity increases, decrease the peak
           ;;hold demand. If this is the last forge-demand with an
           ;;end day of end-day, then stop and concat the peak hold
        ;;records with the original demand records
        ;;if forge record on first day, remove it since it's already
        ;;accounted for in forge demand.
        ;;demand-group-key should be used here, too
        forge-demands (->> demand-records
                              (filter (fn [{:keys [DemandGroup]}]
                                        (= DemandGroup forge-name)))
                              (sort-by :StartDay)
                              ((fn [recs] (check-forge recs
                                                       end-day))))
        ;;used to set the src of the hold demands
        src (:SRC (first demand-records))
        title (:OITitle (first demand-records))]
    (if (or (empty? forge-demands) (<= init-demand 0))
      ;;no hold demands to add
      demand-records
      (loop [;;what is the current forge demand.  Will use to detect
             ;;quantity change.
             curr-forge (sum-demands demand-records [forge-name]
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
             leftover-forge (if (= curr-forge 0)
                              forge-demands
                              ;;otherwise, we don't need to process
                              ;;the first forge demand
                              (rest forge-demands))
                              
             ]
        (cond (or (empty? leftover-forge)
                  ;;could happen with low forge demand.
                  (<= (:Quantity curr-hold-rec)))
              (concat demand-records (add-info hold-demands src title))

              ;;no quantity change
              (= (:Quantity (first leftover-forge)) curr-forge)
              (recur curr-forge curr-hold-rec hold-demands
                     (check-forge (rest
                                   leftover-forge)  end-day))
              ;;new quantity          
              (not= (:Quantity (first leftover-forge)) curr-forge)
              ;;change current forge
              (recur (:Quantity (first leftover-forge))
                     ;;start a new curr-hold-rec with a start day based on the
                     ;;new forge record
                     (assoc curr-hold-rec :StartDay (:StartDay (first
                                                                leftover-forge))
                            ;;and a quantity that is reduced by how
                            ;;much the forge demand increased.
                            :Quantity (- (:Quantity curr-hold-rec)
                                         (- (:Quantity (first
                                                        leftover-forge)) curr-forge)))
                     ;;add the new record to our collection of
                     ;;hold-demands and update the duration
                     (conj hold-demands (assoc curr-hold-rec
                                               :Duration (- (:StartDay
                                                             (first
                                                             leftover-forge))
                                                             (:StartDay
                                                              curr-hold-rec))))
                     (check-forge (rest (leftover-forge)) end-day))
              ;;we shouldn't get here.
              :else
              (throw (Exception. "A case should have matched.")))))))
                                          
(defn make-demands-from
  "Given the path to a workbook containing DemandRecords and
  SupplyRecords, return new DemandRecords with added demand used to
  hold units for the demand at end-day."
  [wkbk-path demands forge-name start-day end-day]
  (let [demand (util/enabled-records wkbk-path "DemandRecords")
        demand-by-src (group-by :SRC demand)
        ;map of "SRC" to {"AC" int, "NG" int, "RC" int}
        supply-map (supply/quants-by-compo wkbk-path)]
    (reduce concat
            (for [[src demand-records] demand-by-src]
              (make-demands demand-records (supply-map src)
                            demands forge-name start-day end-day)))))
      
