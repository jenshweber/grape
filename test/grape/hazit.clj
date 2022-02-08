(ns hazit.rules)
(use 'grape.core)

(gts 'hazit)

(rule 'new-loop! ['cid 'pid 'aid 'sid]
      {
        :create (pattern
                  (node 'c {:label "Controller" :asserts {:id "'&cid'"}})
                  (node 's {:label "Sensor" :asserts {:id "'&sid'"}})
                  (node 'a {:label "Actuator" :asserts {:id "'&aid'"}})
                  (node 'p {:label "Process" :asserts {:id "'&pid'"}})

                  (edge {:label "stimulus" :src 'c :tar 'a})
                  (edge {:label "effect" :src 'a :tar 'p})
                  (edge {:label "reading" :src 'p :tar 's})
                  (edge {:label "perception" :src 's :tar 'c})
                  )
        })



(rule 'split-actuator! ['aid 'aid1 'aid2]
      { :read (pattern
                (node 'a {:label "Actuator" :asserts {:id "'&aid'"}})
                (node 'c)
                (node 'p)
                (edge {:label "stimulus" :src 'c :tar 'a})
                (edge {:label "effect" :src 'a :tar 'p})
                )
        :delete ['a]
        :create (pattern
                  (node 'a1 {:label "Actuator" :asserts {:id "'&aid1'"}})
                  (node 'a2 {:label "Actuator" :asserts {:id "'&aid2'"}})
                  (edge {:label "stimulus" :src 'c :tar 'a1})
                  (edge {:label "effect" :src 'a1 :tar 'p})
                  (edge {:label "stimulus" :src 'c :tar 'a2})
                  (edge {:label "effect" :src 'a2 :tar 'p})
                  )})

(rule 'split-sensor! ['sid 'sid1 'sid2]
      { :read (pattern
                (node 's {:label "Sensor" :asserts {:id "'&sid'"}})
                (node 'c)
                (node 'p)
                (edge {:label "reading" :src 'p :tar 's})
                (edge {:label "perception" :src 's :tar 'c})
                )
        :delete ['s]
        :create (pattern
                  (node 's1 {:label "Sensor" :asserts {:id "'&sid1'"}})
                  (node 's2 {:label "Sensor" :asserts {:id "'&sid2'"}})
                  (edge {:label "reading" :src 'p :tar 's1})
                  (edge {:label "perception" :src 's1 :tar 'c})
                  (edge {:label "reading" :src 'p :tar 's2})
                  (edge {:label "perception" :src 's2 :tar 'c})
                  )})

(rule 'extract-controller-loop! ['aid 'sid 'cid 'aid1 'sid1 'aid2 'sid2]
      { :read (pattern
                (node 'a {:label "Actuator" :asserts {:id "'&aid'"}})
                (node 's {:label "Sensor" :asserts {:id "'&sid'"}})
                (node 'c)
                (node 'p)
                (edge {:label "stimulus" :src 'c :tar 'a})
                (edge {:label "effect" :src 'a :tar 'p})
                (edge {:label "perception" :src 's :tar 'c})
                (edge {:label "reading" :src 'p :tar 's})
                )
        :delete ['a 's]
        :create (pattern
                   (node 'a1 {:label "Actuator" :asserts {:id "'&aid1'"}})
                (node 's1 {:label "Sensor" :asserts {:id "'&sid1'"}})
                           (node 'a2 {:label "Actuator" :asserts {:id "'&aid2'"}})
                (node 's2 {:label "Sensor" :asserts {:id "'&sid2'"}})
                                  (node 'c2 {:label "Controller" :asserts {:id "'&cid'"}})
                                  (edge {:label "stimulus" :src 'c :tar 'a1})
                (edge {:label "effect" :src 'a1 :tar 'c2})
                (edge {:label "perception" :src 's1 :tar 'c})
                (edge {:label "reading" :src 'c2 :tar 's1})

                                                    (edge {:label "stimulus" :src 'c2 :tar 'a2})
                (edge {:label "effect" :src 'a2 :tar 'p})
                (edge {:label "perception" :src 's2 :tar 'c2})
                (edge {:label "reading" :src 'p :tar 's2})

                  )
                })


(rule 'extract-controller-actuator! ['aid 'cid 'aid1 'sid1 'aid2 'sid2]
      { :read (pattern
                (node 'a {:label "Actuator" :asserts {:id "'&aid'"}})
                (node 'c)
                (node 'p)
                (edge {:label "stimulus" :src 'c :tar 'a})
                (edge {:label "effect" :src 'a :tar 'p})
                )
        :delete ['a]
        :create (pattern
                   (node 'a1 {:label "Actuator" :asserts {:id "'&aid1'"}})
                (node 's1 {:label "Sensor" :asserts {:id "'&sid1'"}})
                           (node 'a2 {:label "Actuator" :asserts {:id "'&aid2'"}})
                (node 's2 {:label "Sensor" :asserts {:id "'&sid2'"}})
                                  (node 'c2 {:label "Controller" :asserts {:id "'&cid'"}})
                                  (edge {:label "stimulus" :src 'c :tar 'a1})
                (edge {:label "effect" :src 'a1 :tar 'c2})
                (edge {:label "perception" :src 's1 :tar 'c})
                (edge {:label "reading" :src 'c2 :tar 's1})

                                                    (edge {:label "stimulus" :src 'c2 :tar 'a2})
                (edge {:label "effect" :src 'a2 :tar 'p})
                (edge {:label "perception" :src 's2 :tar 'c2})
                (edge {:label "reading" :src 'p :tar 's2})

                  )
                })


