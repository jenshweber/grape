(ns grape.moduleExample (:require [grape.core :refer :all]))

;; Create a new GTS, specify paths to modules to load. 
(gts 'moduleExample ["test/grape/test_module.clj"])


;; Call rules that are already associated with gts
(clear!) ;; from the default gts

(testRule!) ;; loaded from test_module.clj
