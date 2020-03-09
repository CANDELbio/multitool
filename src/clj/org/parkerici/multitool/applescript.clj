(ns org.parkerici.multitool.applescript
  [:require [clojure.string :as str]
   [clojure.java.shell :as shell]])

;;; Interface to AppleScript (OS X only, of course)

;;; This no longer works, and looks like Applescript engine isn't bundled with Java any more
#_
(defn exec [script]
  (let [engine (.getScriptEngine (apple.applescript.AppleScriptEngineFactory.))]
    (.eval engine script)))

;;; Use this method instead.
(defn exec
  [script]
  (let [{:keys [exit out err]}  (shell/sh "osascript" :in script)]
    (if (zero? exit)
      (str/trim out)
      (throw (ex-info err {:applescript script})))))

(defn speak
  "Say something"
  [text]
  (exec (format "say \"%s\"" text)))

(defn exec-app
  [app script]
  (exec (format "tell application \"%s\"\n %s\n end tell" app script)))

;;; Example: open a URL in Chrome incognito windoe

(defn open-incognito
  [url]
  (exec-app "Google Chrome"
   (format "make new window with properties {mode: \"incognito\"}
 activate
 set URL of active tab of first window to \"%s\" " url)))

;;; Example:  monitor user activity

(defn active-app
  "Return name of frontmost app"
  []
  (exec-app "System Events" "return name of first application process whose frontmost is true"))

(defmulti details (fn [app] app))

(defmethod details :default
  [app]
  nil)

(defmethod details "Google Chrome"
  [app]
  {:url (exec-app "Google Chrome" "get URL of active tab of first window")
   :title (exec-app "Google Chrome" "get title of active tab of first window")})

(defmethod details "Safari"
  [app]
  {:url (exec-app "Safari" "return url of front document")
   :title (exec-app "Safari" "return name of front document")})

(defn monitor
  []
  (while true
    (let [app (active-app)]
      (prn app (details app)))
    (Thread/sleep 1000)))


