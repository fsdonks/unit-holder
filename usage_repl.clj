;;Paste into MARATHON REPL
;;;Load-file unit-holder
(load-file "/home/craig/workspace/unit-holder/unit-holder.clj")
(ns unit-holder)

;;;Make-demands-from
(def wkbk-path "/home/craig/runs/big_test/base-testdata-v7.xlsx")
(def priority-demands ["Hinny" "Moke"])
(def demands (make-demands-from 
               wkbk-path 
               priority-demands 
               722
               785))
               
;;;Change priorities per workflowy
(def demands-2 (for [{:keys [Vignette DemandGroup] :as r} demands]
                 (assoc r :Priority
                        (case DemandGroup "Hinny" 1
                          "Moke" 1
                          3))))

;;;Save demandrecords as an excel file
(require '[spork.util.excel.core :as spork-xl])
(require '[spork.util.io :as io])

(defn records->xlsx [wbpath sheetname recs]
  (->> (tbl/records->table recs)
       (tbl/stringify-field-names)
       (spork-xl/table->xlsx wbpath sheetname)
       ))

(in-ns 'spork.util.excel.docjure)
 (defn set-cell! [^Cell cell value]
   (if (nil? value)
     (let [^String null nil]
       (.setCellValue cell null)) ;do not call setCellValue(Date) with null
     (let [converted-value (cond (number? value) (double value)
                                 true (str value))]
       (.setCellValue cell converted-value)
       (if (date-or-calendar? value)
         (apply-date-format! cell "m/d/yy")))))
 (ns unit-holder)

(records->xlsx (str (io/fdir wkbk-path) "/peak-hold.xlsx") 
               "DemandRecords"
               demands-2)
  
;;;Copy previous MARATHON run file and put in new demand records
