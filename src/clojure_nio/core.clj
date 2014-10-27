(ns clojure-nio.core
  (:require [clojure.string :as string])
  (:import [java.nio.file FileSystem Files LinkOption Path OpenOption FileSystems CopyOption]
           [java.nio.file.attribute FileAttribute PosixFilePermissions]
           [java.nio.charset StandardCharsets Charset]
           [java.io OutputStream InputStream]))

(defn default-fs []
  (FileSystems/getDefault))

(defmacro varargs-array [type args]
  `(into-array ~type (or ~args [])))

(defmacro link-opts [args] `(varargs-array LinkOption ~args))
(defmacro copy-opts [args] `(varargs-array CopyOption ~args))
(defmacro open-opts [args] `(varargs-array OpenOption ~args))
(defmacro file-attrs [args] `(varargs-array FileAttribute ~args))

(defn copy [src target & copy-options]
  (if (instance? Path src)
    (if (instance? Path target)
      (Files/copy ^Path src ^Path target (copy-opts copy-options))
      (Files/copy ^Path src ^OutputStream target))
    (Files/copy ^InputStream src ^Path target (copy-opts copy-options))))

(defn path [^FileSystem fs ^String path & paths]
  (.getPath fs path (varargs-array String paths)))

(defn exists? [^Path path & link-options]
  (Files/exists path (link-opts link-options)))

(defn file? [^Path path & link-options]
  (Files/isRegularFile path (link-opts link-options)))

(defn dir? [^Path path & link-options]
  (Files/isDirectory path (link-opts link-options)))

(defn sym-link? [^Path path]
  (Files/isSymbolicLink path))

(defn read-sym-link [^Path path]
  (Files/readSymbolicLink path))

(defn create-dir [^Path path & file-attributes]
  (Files/createDirectory path (file-attrs file-attributes)))

(defn create-file [^Path path & file-attributes]
  (Files/createFile path (file-attrs file-attributes)))

(defn create-hard-link [^Path link ^Path existing]
  (Files/createLink link existing))

(defn create-sym-link [^Path link ^Path existing & file-attributes]
  (Files/createSymbolicLink link existing (file-attrs file-attributes)))

(defn read-all-lines
  ([^Path path]
   (read-all-lines path StandardCharsets/UTF_8))
  ([^Path path ^Charset charset]
   (Files/readAllLines path charset)))

(defn write-bytes [^Path path ^bytes bytes & open-options]
  (Files/write path bytes (open-opts open-options)))

(defn write-lines
  ([^Path path lines]
   (write-lines path lines StandardCharsets/UTF_8))
  ([^Path path lines ^Charset charset & open-options]
   (Files/write path lines charset (open-opts open-options))))

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
          ^Path my-path (path fs root (:name child))]
      (condp = (:type child)
        :dir (do
               (create-dir my-path)
               (create-fs-tree!
                 fs
                 (str my-path)
                 (:children child)))
        :file (do
                (create-file my-path)
                (when-let [c (:content child)]
                  (write-lines my-path c)))
        :link (create-hard-link my-path (path fs (:link-to child)))
        :sym-link (create-sym-link my-path (path fs (:link-to child)))
        (throw (ex-info "Illegal type" child))))))

(defn posix-perm-file-attrs [perm-str]
  (PosixFilePermissions/asFileAttribute
    (PosixFilePermissions/fromString
      perm-str)))
