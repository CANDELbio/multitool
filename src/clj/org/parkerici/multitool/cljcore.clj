(ns org.parkerici.multitool.cljcore
  "Java-only stuff"
  (:require
   [clojure.string :as str]
   [clojure.pprint :as pprint]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [org.parkerici.multitool.core :as core])
  (:import [java.util Base64 Date UUID]
           [java.io File Reader PushbackReader]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.net URL URI]
           [java.text SimpleDateFormat]
           [java.awt Desktop]))

;;; ⩇⩆⩇ Exceptions ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn error "Throw a generic Exception with formatted string"
  [s & args]
  (throw (Exception. (apply format s args))))

(defn warn [s & args]
  (println (str "WARNING: " (apply format s args))))

;;; TODO port to .cljc using window.performance.now() (returns usec)
(defn timing-fn
  "Returns a fn that acts like f, but return value is (time result), time in msec]"
  [f]
  (fn [& args]
    (let [start (System/nanoTime)
          ret (apply f args)]
      (list (/ (double (- (System/nanoTime) start)) 1000000.0)
            ret))))

(defn java-resource->string [resource]
  (-> resource
      io/resource
      io/input-stream
      slurp))

;;; ⩇⩆⩇ Files ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; Note: probably better to avoid these and use raynes/fs https://github.com/Raynes/fs

(defn content-files
  [dir & regex]
  (filter #(and (not (.isDirectory ^File %))
                (or (empty? regex) (re-find (first regex) (str %))))
          (file-seq (io/file dir))))

(defn file-exists?
  "True if file `path` exists"
  [path]
  (.exists (io/as-file path)))

(defn file-delete-recursively
  "Delete a directory and its contents"
  [fname]
  (letfn [(del1 [^File f]
            (when (.isDirectory f)
              (doseq [f2 (.listFiles f)]
                (del1 f2)))
            (io/delete-file f))]
    (del1 (io/file fname))))

(defn file-delete-safe
  "Delete a file or directory safely (that is, no error if doesn't exist)"
  [fname]
  (when (file-exists? fname)
    (file-delete-recursively fname)))

;;; http://stackoverflow.com/questions/840190/changing-the-current-working-directory-in-java
(defn cd "As in Unix shell cd"
  [^String dirname]
  (let [dir (.getAbsoluteFile (File. dirname))]
    (System/setProperty "user.dir" (.getAbsolutePath dir))
    dir))

(defn ^File temp-file []
  (File/createTempFile "temp" ""))

(defn temp-file-path []
  (.getPath ^File (temp-file)))

(defn temp-dir-path []
  (str (Files/createTempDirectory "temp" (into-array FileAttribute [] ))))

(defn directory-files [d filterfn]
  (filter (fn [^File f]
            (and (not (.isDirectory f))
                (.exists f)
                (filterfn (.getName f))))
          (file-seq (io/file d))))

(defn ensure-directory
  "Create directory if it doesn't exist (recursively)"
  [d]
  (let [^File f (File. d)]
    (when-not (.exists f)
      (.mkdirs f))))


(defn local-file [url]
  (let [url (URL. url)
        tmp (temp-file)]
    (io/copy (.openStream url) tmp)
    (.getPath tmp)))

(defn file-lines [file]
  (let [r (io/reader file)]
    (line-seq r)))

(defn file-lines-out [file seq]
  (let [w (io/writer file)]
    (binding [*out* w]
      (doseq [l seq]
        (println l)))))

(defn process-file-lines [f in out]
  (file-lines-out out (map f (file-lines in))))

;;; TODO generalize to more than one form?
(defn read-from-file
  "Read a form from a file"
  [file]
  (let [rdr (-> file
                io/file
                io/reader
                PushbackReader.)]
    (read rdr)))

;;; ⩇⩆⩇ Date/time ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; For more serious use, see clj-time https://github.com/clj-time/clj-time

(defn now []
  (Date.))

(core/defn-memoized date-formatter [f]
  (SimpleDateFormat. f))

; "yy-MM-dd kk:mm"
; "YYYY-MM-dd_HH_MM_SS")
(defn date-format [date format]
  (.format (date-formatter format) date))

(defn date+ [date days hours minutes]
  (Date. (+ (.getTime date) (* 60 1000 (+ minutes (* 60 (+ hours (* 24 days))))))))

;;; ⩇⩆⩇ Output ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn humanize-number [n]
  (cond (>= n 1e9)
        (format "%1.3gG" (/ n 1e9))
        (>= n 1e6)
        (format "%1.3gM" (/ n 1e6))
        (>= n 1e3)
        (format "%1.3gk" (/ n 1e3))
        :else
        (str n)
        ))

;;; ⩇⩆⩇ Shell ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn sh-errchecked [& args]
  (let [res (apply shell/sh args)]
    (when-not (= (:exit res) 0)
      (throw (Exception. "Bad result from shell" res))
      )))

;;; ⩇⩆⩇ Higher file fns ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn read-tsv-file
  "Given a tsv file with a header line, returns seq where each elt is a map of field names to strings"
  [f]
  (let [raw (file-lines f)
        fields (str/split (first raw) #"\t")]
    (map (fn [l]
           (core/clean-map
            (zipmap fields (str/split l #"\t"))
            #(= % "")))
         (rest raw))))

(defn open-url
  [url]
  (when (Desktop/isDesktopSupported)
    (.browse (Desktop/getDesktop)
             (URI/create url))))

;;; string-search removed, use str/index-of or str/includes?

(defn string-search-all
  "Return a sequence of all positions where sub is found in string"
  [^String string ^String sub & [^long start]]
  (let [start (or start 0)
        pos (.indexOf string sub start)]
    (if (> pos 0)
      (cons pos (string-search-all string sub (+ 1 pos)))
      ())))

(defn random-uuid
  []
  (str (UUID/randomUUID)))

(defn schpit
  "Like core/spit, but will do something sensible for lazy seqs."
  [f content & options]
  (with-open [w (apply io/writer f options)]
    (binding [*print-length* nil
              *out* w]
      (prn content))))

(defn schppit
  "Like schpit but will prettyprint."
  [f content & options]
  (with-open [w (apply io/writer f options)]
    (binding [*print-length* nil]
      (pprint/pprint content w))))

(defn read-chars
  [^Reader reader n]
  (let [a (char-array n)]
    (.read reader a)
    (String. a)))

;;; ⩇⩆⩇ String Parsing ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn parse-boolean-or-nil
  [^String value]
  (if (nil? value) nil (Boolean/valueOf value)))

(defn parse-long-or-nil
  [^String value]
  (if (nil? value) nil (Long/parseLong value)))

;;; ⩇⩆⩇ Path manipulation ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn split-path
  "Splits a path delimited with /"
  [path]
  (remove empty? (str/split path #"/")))

(defn join-path
  "Joins the elements passed in into a path"
  [& args]
  (str "/" (str/join "/" (flatten (map #(split-path %) (remove empty? args))))))

(defn base64-encode
  [^String string]
  (.encodeToString (Base64/getEncoder) (.getBytes string)))

(defn base64-decode
  [^String b64-string]
  (String. (.decode (Base64/getDecoder) (.getBytes b64-string))))
  
