;;;; This file is part of gorilla-repl. Copyright (C) 2014-, Jony Hudson.
;;;;
;;;; gorilla-repl is licenced to you under the MIT licence. See the file LICENCE.txt for full details.

(ns gorilla-repl.render-values-mw
  (:require [nrepl.transport :as transport]
            [nrepl.middleware :as middleware]
            [gorilla-renderable.core :as render]
            [cheshire.core :as json])
  (:import nrepl.transport.Transport))

(defn custom-renderer
   [value writer opts]
   (let [printer (if *print-dup* print-dup print-method)]
     (printer (json/generate-string (render/render value)) writer)
     (str writer)))
