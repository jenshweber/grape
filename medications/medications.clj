(use 'grape.core)

(gts 'medications)

;; Define rules here:
;; ------------------

(rule 'create_rx! ['substance]
      {:create (pattern (node 'p1 {:asserts {:kind "'Prescription'" :substance "'&substance'"}}))})

(rule 'add_instruction! ['action]
      {:read (pattern
               (node 'p1 {:asserts {:kind "'Prescription'"}})
               (NAC
                   (node 'inst {:asserts {:kind "'Instruction'"}})
                   (edge 'i1 {:label "instruction" :src 'p1 :tar 'inst})
               )
              )
       :create (pattern
                 (node 'inst {:asserts {:kind "'Instruction'" :action "'&action'"}})
                 (edge 'i2 {:label "instruction" :src 'p1 :tar 'inst})
                 )
       }
)

(rule 'add_timeframe! ['duration 'frame 'freq]
      {:read (pattern
               (node 'i1 {:asserts {:kind "'Instruction'"}})
               (NAC
                 (node 'tf1 {:asserts {:kind "'TimeFrame'"}})
                 (edge 'e {:label "timeframe" :src 'i1 :tar 'tf1})
               )
             )
       :create (pattern
                 (node 'tf2 {:asserts {:frame "'&frame'" :frequency "'&freq'" :kind "'TimeFrame'"}})
                 (edge 'e {:label "timeframe" :src 'i1 :tar 'tf2})
                 (assign "i1.duration=&duration")
               )
      }
)

(rule 'refine_timeframe! ['value 'frame 'freq]
    {:read (pattern
               (node 'tf1 {:asserts {:kind "'TimeFrame'"}})
           )
     :create (pattern
               (node 'cp2 {:asserts {:kind "'CompoundPoint'" :value "'&value'"}})
               (node 'tf2 {:asserts {:kind "'TimeFrame'" :frame "'&frame'" :frequency "'&freq'"}})
               (edge 'e2 {:label "point" :src 'tf1 :tar 'cp2})
               (edge 'e3 {:label "refine" :src 'cp2 :tar 'tf2})
             )

    }
)

(rule 'add_atomic! ['value]
    {:read (pattern
             (node 'tf1 {:asserts {:kind "'TimeFrame'"}})
           )
     :create (pattern
               (node 'ap1 {:asserts {:kind "'AtomicPoint'" :value "'&value'"}})
               (edge 'e {:label "point" :src 'tf1 :tar 'ap1})
             )
     }
)

(rule 'add_timeframe_dose! ['value 'unit]
      {:read (pattern
               (node 'tf {:asserts {:kind "'TimeFrame'"}})
               (NAC
                 (node 'd1 {:asserts{:kind "'Dose'"}})
                 (edge 'e1 {:label "dose" :src 'tf :tar 'd1})

                )
              )
       :create (pattern
                 (node 'd2 {:asserts {:kind "'Dose'" :value "'&value'" :unit "'&unit'"}})
                 (edge 'e {:label "dose" :src 'tf :tar 'd2})
                 )
       }
)

(rule 'add_point_dose! ['value 'unit]
      {:read (pattern
               (node 'ap {:asserts {:kind "'AtomicPoint'"}})
               (NAC
                 (node 'd1 {:asserts{:kind "'Dose'"}})
                 (edge 'e1 {:src 'ap :tar 'd1})

                )
              )
       :create (pattern
                 (node 'd2 {:asserts {:kind "'Dose'" :value "'&value'" :unit "'&unit'"}})
                 (edge 'e {:label "dose" :src 'ap :tar 'd2})
                )
       }
)

;; Execute rules here:
;; -------------------

(create_rx! "Aspirin")
(add_instruction! "take")
(add_timeframe! 10 "WEEK" "2")
(refine_timeframe! "1" "DAY" "1")
(refine_timeframe! "4" "DAY" "1")
(add_atomic! "8")
(add_point_dose! "81" "mg")
(add_point_dose! "81" "mg")
