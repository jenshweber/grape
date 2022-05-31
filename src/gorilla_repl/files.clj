(ns gorilla-repl.files
  "Utility functions to help with scanning for and loading gorilla files."
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn gorilla-file?
  [file]
  (when (.isFile file)
    (with-open [r (java.io.BufferedReader. (java.io.FileReader. file))]
      (let [first-line (.readLine r)]
        (if (> (count first-line) 26)
          (let [header (subs first-line 0 26)]
            (= header ";; gorilla-repl.fileformat"))
          false)))))

(defn excluded-file-seq
  [file excludes]
  (tree-seq
   (fn [f] (and (.isDirectory f) (not (contains? excludes (.getName f)))))
   (fn [f] (.listFiles f))
   file))

(defn include-file?
  "Should a file be included in the 'load file' list? Currently all .cljw, .cljs, .cljc, .hl and .clj files are included."
  [file]
  (and
   (or (str/ends-with? (.getName file) ".clj")
       (str/ends-with? (.getName file) ".cljs")
       (str/ends-with? (.getName file) ".cljc")
       (str/ends-with? (.getName file) ".cljw")
       (str/ends-with? (.getName file) ".hl"))
   (gorilla-file? file)))

(defn gorilla-filepaths-in-current-directory
  [excludes]
  (map #(str/replace-first (. % getPath) "./" "")
       (filter include-file? (excluded-file-seq
                              (io/file ".")
                              excludes))))
