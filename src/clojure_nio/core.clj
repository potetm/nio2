(ns clojure-nio.core
  (:require [clojure.string :as string])
  (:import [java.nio.file FileSystem Files LinkOption Path OpenOption FileSystems]
           [java.nio.file.attribute FileAttribute PosixFilePermissions]
           [java.nio.charset StandardCharsets Charset]))

(defn default-fs []
  (FileSystems/getDefault))

(defn path [^FileSystem fs ^String path & paths]
  (.getPath fs path (into-array String (or paths []))))

(defn exists?
  ([^Path path]
   (exists? path []))
  ([^Path path link-options]
   (Files/exists path (into-array LinkOption link-options))))

(defn file?
  ([^Path path]
   (file? path []))
  ([^Path path link-options]
   (Files/isRegularFile path (into-array LinkOption link-options))))

(defn dir?
  ([^Path path]
   (dir? path []))
  ([^Path path link-options]
   (Files/isDirectory path (into-array LinkOption link-options))))

(defn sym-link? [^Path path]
  (Files/isSymbolicLink path))

(defn read-sym-link [^Path path]
  (Files/readSymbolicLink path))

(defn create-dir
  ([^Path path]
   (create-dir path []))
  ([^Path path file-attrs]
   (Files/createDirectory path (into-array FileAttribute file-attrs))))

(defn create-file
  ([^Path path]
   (create-file path []))
  ([^Path path file-attrs]
   (Files/createFile path (into-array FileAttribute file-attrs))))

(defn create-hard-link [^Path link ^Path existing]
  (Files/createLink link existing))

(defn create-sym-link
  ([^Path link ^Path existing]
   (create-sym-link link existing []))
  ([^Path link ^Path existing file-attrs]
   (Files/createSymbolicLink link existing (into-array FileAttribute file-attrs))))

(defn read-all-lines
  ([^Path path]
   (read-all-lines path StandardCharsets/UTF_8))
  ([^Path path ^Charset charset]
   (Files/readAllLines path charset)))

(defn write-bytes
  ([^Path path ^bytes bytes]
   (write-bytes path bytes []))
  ([^Path path ^bytes bytes open-options]
   (Files/write path bytes (into-array OpenOption open-options))))

(defn write-lines
  ([^Path path lines]
   (write-lines path lines StandardCharsets/UTF_8))
  ([^Path path lines ^Charset charset]
   (write-lines path lines charset []))
  ([^Path path lines ^Charset charset open-options]
   (Files/write path lines charset (into-array OpenOption open-options))))

;; UTILS

(defn- assoc-implicit-args [node]
  (assoc node
    :type (cond
            (:children node) :dir
            :else :file)))

(defn create-fs-tree! [^FileSystem fs ^String root children]
  (doseq [child-wo-type? children]
    (let [child (if (:type child-wo-type?)
                  child-wo-type?
                  (assoc-implicit-args child-wo-type?))
          ^Path path (path fs root (:name child))]
      (condp = (:type child)
        :dir (do
               (create-dir path)
               (create-fs-tree!
                 fs
                 (str path)
                 (:children child)))
        :file (do
                (create-file path)
                (when-let [c (:content child)]
                  (write-lines path c)))
        :link (create-hard-link path (path fs (:link-to child)))
        :sym-link (create-sym-link path (path fs (:link-to child)))
        (throw (ex-info "Illegal type" child))))))

(defn posix-perm-file-attrs [perm-str]
  (PosixFilePermissions/asFileAttribute
    (PosixFilePermissions/fromString
      perm-str)))
