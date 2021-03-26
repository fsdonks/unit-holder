;;This namespace is intended to be loaded from proc.
;;The purpose of this namespace is to set aside units at the beginning
;;of conflict so that they can deploy to a specified set of demands as
;;those demands build over time.
;;At this time, the simplest approach seems to be to just add hold
;;demands to DemandRecords.
(ns unit-holder
  (:require [proc.demandanalysis :as analyzer]
            [proc.supply :as supply]
            [spork.util [io :as io] [table :as tbl]]))

(defn sum-demands
  "Given a sequence of demand names, add the quantity of all demands
  together, sampled at time t in demand-records"
  [demand-records demands t]
  (let [sample (analyzer/peak-parts demand-records
                                    :periods [{:Name "time t"
                                               :FromDay t
                                               :ToDay t}])]
    ;Get all demand quantities from the sample and add them together.
    (reduce + (map (first sample) demands))))
   
(defn init-hold
  "Compute the number of initial units to hold."
  [demands-records supply-map demands start-day end-day]
  (let [;InitialDefeat = (AOR+FORGE+OffFORGE)
        ;The sum of the quantity from all demands in demands at the
        ;start day.
        init-demands (sum-demands demand-records demands start-day)
        supply 
        ;LeftoverQuantity= RCSupply + ACSupply - NumRCCannibalized -
                                        ;HLD - InitialDefeat
        ;PeakDefeat =(AOR+FORGE+OffFORGE)
        ;PeakHold = If (LeftoverQuantity>0) Min(LeftoverQuantity,
        ;PeakDefeat)
        
        ]
    ))

(defn make-demands
  "This function creates the actual demand records to hold a number of
  units in order to meet the peak demand.  The results demand records
  should be concatenated to the existing demand records for this SRC.
  Expect demand-records to be filtered for one SRC already. supply-map
  is a map of compo to quantity for this SRC only.
  demands is a sequence of strings containing the name of demand
  groups to hold units for. end-day is an int that indicates which day
  to stop the peak hold demand (end-day can be manual for now)."
  [demands-records supply-map demands start-day end-day]
  (let [;InitialDefeat = (AOR+FORGE+OffFORGE)
        ;;will need to make sure inventories aren't nil
        ;LeftoverQuantity= RCSupply + ACSupply - NumRCCannibalized -
                                        ;HLD - InitialDefeat
        ;PeakDefeat =(AOR+FORGE+OffFORGE)
        ;PeakHold = If (LeftoverQuantity>0) Min(LeftoverQuantity,
        ;PeakDefeat)
        
        ]
    (loop [;;the number of units currently held for the main demand
           curr-main
           ;;every time forge quantity increases, decrease the peak
           ;;hold demand. If this is the last forge-demand with an
           ;;end day of end-day, then stop and concat the peak hold
           ;;records with the original demand recrods
           forge-demands]
  )))

(defn make-demands-from
  "Given the path to a workbook containing DemandRecords and
  SupplyRecords, return new DemandRecords with added demand used to
  hold units for the demand at end-day."
  [wkbk-path demands start-day end-day]
  (let [demand (util/enabled-demand wkbk-path)
        demand-by-src (group-by :SRC demand)
        ;map of "SRC" to {"AC" int, "NG" int, "RC" int}
        supply-map (supply/quants-by-compo wkbk-p)]
    (reduce concat
            (for [[src demand-records] demand-by-src]
              (make-demands demands-records (supply-map src)
                            demands start-day end-day)))))
      
