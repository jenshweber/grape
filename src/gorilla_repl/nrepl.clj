(ns gorilla-repl.nrepl
  (:require [gorilla-repl.websocket-relay :as ws-relay]
            [nrepl.server :as nrepl-server]
            [nrepl.middleware.print :as print]
            [cider.nrepl :as cider]))

(def nrepl (atom nil))

(defn start-and-connect
  ([nrepl-requested-port repl-port-file]
   (let [cider-mw (map resolve cider/cider-middleware)
         middleware (conj cider-mw #'print/wrap-print)
         nr (nrepl-server/start-server :port nrepl-requested-port
                                       :handler (apply nrepl-server/default-handler middleware))
         nrepl-port (:port nr)]
     (println "Started nREPL server on port" nrepl-port)
     (swap! nrepl (fn [_] nr))
     (ws-relay/connect-to-nrepl nrepl-port)
     (spit (doto repl-port-file .deleteOnExit) nrepl-port))))
