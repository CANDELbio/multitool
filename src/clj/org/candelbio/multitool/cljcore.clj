(ns org.candelbio.multitool.cljcore
  "Java-only stuff"
  (:require
   [clojure.string :as str]
   [clojure.pprint :as pprint]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [org.candelbio.multitool.core :as core])
  (:import [java.util Base64 Date]
           [java.io File Reader PushbackReader]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.net URL URI]
           [java.text SimpleDateFormat]
           [java.awt Desktop]))

;;; ⩇⩆⩇ Parellelism ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; Clojurescript does not have pmap, unsuprisingly

(defn pmap-values
  "Map f over the values of hashmap in parallel"
  [f hashmap]
  (zipmap (keys hashmap) (pmap f (vals hashmap))))

(defn pmap-keys
  "Map f over the kes of hashmap in parallel"
  [f hashmap]
  (zipmap (pmap f (keys hashmap)) (vals hashmap)))

;;; TODO these are useless, a good way to crash a machine. But a version that limited itself to n threads might be good.
;;; TODO pmap does the right thing (pays attention to available cores) so redo based on that.
(defmacro pdoseq
  "Like doseq but each iteration gets its own thread."
  [seqs & body]
  `(doseq ~seqs
     (doto (Thread.
            (fn [] ~@body))
       (.setDaemon true)
       (.start))))

(defmacro pdoseq*
  "Like doseq* but each iteration gets its own thread."
  [seqs & body]
  `(core/doseq* ~seqs
     (doto (Thread.
            (fn [] ~@body))
       (.setDaemon true)
       (.start))))

(defn processors
  "Return the number of available processors"
  []
  (.availableProcessors (java.lang.Runtime/getRuntime)))

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

;;; TODO better name
(defn java-resource->string [resource]
  (-> resource
      io/resource
      io/input-stream
      slurp))

;;; TODO Java EDN resource, see also read-from-file

;;; ⩇⩆⩇ Files ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; Note: probably better to avoid these and use raynes/fs https://github.com/Raynes/fs

;;; Based on clojure.core/file-seq
(defn file-seq-filtered
  "A tree seq on java.io.Files"
  [remove-fn dir]
  (tree-seq
   (fn [^java.io.File f] (.isDirectory f))
   (fn [^java.io.File d] (remove remove-fn (seq (.listFiles d))))
   dir))

(defn list-dir-recursive
  "Like fs/list-dir, but recurses through subdirectories and does not include dirs in result. Ignore .dotfiles" ; TODO option to include dotfiles
  [dir]
  (->> dir
       io/as-file
       (file-seq-filtered #(= \. (first (.getName %))))
       (remove #(.isDirectory %))))

;;; Deprecated
(defn content-files
  [dir & regex]
  (filter #(or (empty? regex)
               (re-find (first regex) (str %))))
          (list-dir-recursive dir))

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

(defn ensure-file
  "Ensure that the directory structure to contain this file exists"
  [f]
  (let [parts (str/split f #"\/")]
    (ensure-directory (str/join "/" (butlast parts)))
    f))

(defn local-file
  "Make a copy of contens of url in a local temp file or supplied filename"
  ([url]
   (local-file url (temp-file)))
  ([url local-file]
   (let [url (URL. url)
         local-file (if (instance? java.io.File local-file)
                      local-file
                      (java.io.File. local-file))]
     (io/copy (.openStream url) local-file)
     (str local-file))))

;;; TODO use this more extensively.
;;; Note: incorrect for ~fred/blah paths, but I can't think of a reecent occasion of use for that.
(defn expand-homedir
  [path]
  (str/replace path #"^~" (System/getProperty "user.home")))

(defn reader
  "Return a reader for a file, with homedir expansin"
  [file]
  (-> file
      expand-homedir
      io/reader))

(defn file-lines [file]
  (let [r (reader file)]
    (line-seq r)))

(defn file-lines-out [file seq]
  (let [w (io/writer file)]
    (binding [*out* w]
      (doseq [l seq]
        (println l))
      (newline))))

;;; Copied from fs to avoid a dependency
(defn- tmp-file
  "The temporary file directory looked up via the java.io.tmpdir
   system property. Does not create a temporary directory."
  []
  (str (System/getProperty "java.io.tmpdir") (gensym "tmpfile")))

(defn- rename-file
  "Rename old-path to new-path. Only works on files."
  [old-path new-path]
  (.renameTo (io/file old-path) (io/file new-path)))

;;; TODO this can alter EOF newlines, causing spurious git modifications. Argh
(defn process-file-lines
  ([f in out]
   (file-lines-out out (map f (file-lines in))))
  #_                                    ;I'm deprecating this, its fucked me over on multiple occasions
  ([f in]
   (let [out (tmp-file)]
     (process-file-lines f in out)
     (rename-file out in))))

(defn substitute-file-lines
  [map]
  (partial 
   process-file-lines
   (fn [line]
     (core/str-replace-multiple map line))))

;;; TODO generalize to more than one form?
;;; TODO edn reader is better
(defn read-from-file
  "Read a form from a file"
  [file]
  (let [rdr (-> file
                io/file
                reader
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

;;; TODO combine these two into one thing that works

(defn sh-errchecked [& args]
  (let [{:keys [exit out] :as res} (apply shell/sh args)]
    (when-not (= exit 0)
      (throw (ex-info "Shell error" res)))
    out))

(defn sh-str
  "Version of sh that takes a single string, which can include pipe operators etc"
  [s]
  (shell/sh "/bin/bash" "-c" s))

;;; ⩇⩆⩇ Higher file fns ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; Very simple tab file i/o. Not sophisticated and won't handle some cases, use clojure.data.csv for real work

(defn read-tsv-rows
  "Read a tsv file into vectors"
  [f & [separator]]
  (map #(str/split % separator)
       (file-lines f)))

(defn read-csv-rows
  "Read a csv file into vectors"
  [f]
  (read-tsv-rows f #"\,"))

(defn read-tsv-maps
  "Given a tsv file with a header line, returns seq where each elt is a map of field names to strings"
  [f & [separator]]
  (let [rows (read-tsv-rows f separator)]
    (map #(zipmap (first rows) %)
         (rest rows))))

(defn read-csv-maps
  "Read a csv file into maps"
  [f]
  (read-tsv-maps f #"\,"))

(defn write-tsv-rows
  [f rows & [separator]]
  (file-lines-out
   f
   (map (partial str/join separator) rows)))

(defn write-tsv-maps
  [f rows & [separator]]
  (let [cols (keys (first rows))]
  (file-lines-out
   f
   (map (partial str/join separator)
        (cons (map name cols)
              (map (fn [row] (map (fn [col] (get row col)) cols))
                   rows))))))

(defn write-csv-rows
  [f rows]
  (write-tsv-rows f rows ","))

(defn write-csv-maps
  [f rows]
  (write-tsv-maps f rows ","))


;;; TODO parallel fns for .csv

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
  
(defn copy-paths
  "Copy in-path to out-path, in-path can be URI or filename, out-path should be a file"
  [in-path out-path]
  (with-open [in (io/input-stream in-path)
              out (io/output-stream out-path)]
    (io/copy in out)))

(defn download
  "Common use case for copy-paths"
  [uri file]
  (copy-paths uri file))

;;; ⩇⩆⩇ Processes ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn start-periodic-process!
  "Start a background process to execute thunk f every t mssc"
  [t f]
  (future
    (loop []
      (f)
      (Thread/sleep t)
      (recur)))
  nil)

;;; ⩇⩆⩇ Reflection ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; See clojure.reflect/reflect which does most of what I want

;;; Invoke a private or protected Java method (of no args – that requires more complication)
(defn invoke-private
  [o methodname]
  (let [m (.getDeclaredMethod (class o) nil)]
    (.invoke m o nil)))


