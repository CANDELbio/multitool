(ns org.candelbio.multitool.background
  (:import [java.util.concurrent Executors]))

;;; Minimal background task management

(def thread-pool (Executors/newCachedThreadPool))

(defn call-in-background [thunk]
  (.submit thread-pool thunk))

;;; TODO can't see log until terminates; that's not optimal

(defn in-background
  "Run thunk in the background.
Call success-cont with the value and log
   or fail-cont with the exception and log.
  error-logger is usually log/error, but I'm trying to hold down dependencies"
  [thunk success-cont fail-cont error-logger]
  (let [original-out *out*]
    (call-in-background
     #(with-out-str
        (try
          (let [result (thunk)
                log (str *out*)]
            (binding [*out* original-out]
              (success-cont result log)))
          (catch Exception e
            (error-logger "Error in background process" e)
            (let [log (str *out*)]
              (binding [*out* original-out]
                (fail-cont e log)))))))))

(defn not-in-background
  "API As above but done in-thread. For debugging"
  [thunk success-cont fail-cont error-logger]
  (let [original-out *out*]
    (with-out-str
      (try
        (let [result (thunk)]
          (binding [*out* original-out]
            (success-cont result (str *out*))))
        (catch Exception e
          (error-logger "Error in background process" e)
          (binding [*out* original-out]
            (fail-cont e (str *out*))))))))




