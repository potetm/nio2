(ns clojure-nio.core
  (:require [clojure.string :as string])
  (:import [java.nio.file FileSystem Files LinkOption Path OpenOption FileSystems CopyOption DirectoryStream]
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

(defn path [^FileSystem fs ^String path & paths]
  (.getPath fs path (varargs-array String paths)))

;; QUERY

(defn exists? [^Path path & link-options]
  (Files/exists path (link-opts link-options)))

(defn file? [^Path path & link-options]
  (Files/isRegularFile path (link-opts link-options)))

(defn dir? [^Path path & link-options]
  (Files/isDirectory path (link-opts link-options)))

(defn sym-link? [^Path path]
  (Files/isSymbolicLink path))

;; INTERACT

(defn read-sym-link [^Path path]
  (Files/readSymbolicLink path))

(defn list-dir [^Path path]
  (let [files (atom [])]
    (with-open [^DirectoryStream stream (Files/newDirectoryStream path)]
      (doseq [file stream]
        (swap! files conj file)))
    @files))

;; CREATE

(defn create-dir [^Path path & file-attributes]
  (Files/createDirectory path (file-attrs file-attributes)))

(defn create-dirs [^Path path & file-attributes]
  (Files/createDirectories path (file-attrs file-attributes)))

(defn create-file [^Path path & file-attributes]
  (Files/createFile path (file-attrs file-attributes)))

(defn create-hard-link [^Path link ^Path existing]
  (Files/createLink link existing))

(defn create-sym-link [^Path link ^Path existing & file-attributes]
  (Files/createSymbolicLink link existing (file-attrs file-attributes)))

(defprotocol ICreateTmpDir
  (create-tmp-dir [me & rest]))

(extend Path
  ICreateTmpDir
  {:create-tmp-dir (fn create-tmp-dir [dir & [prefix & file-attributes]]
                     (Files/createTempDirectory dir prefix (file-attrs file-attributes)))})

(extend String
  ICreateTmpDir
  {:create-tmp-dir (fn create-tmp-dir [prefix & file-attributes]
                     (Files/createTempDirectory prefix (file-attrs file-attributes)))})

;; IO

(defn copy [src target & copy-options]
  (let [^"[Ljava.nio.file.CopyOption;" copy-options (copy-opts copy-options)]
    (if (instance? Path src)
      (if (instance? Path target)
        (Files/copy ^Path src ^Path target copy-options)
        (Files/copy ^Path src ^OutputStream target))
      (Files/copy ^InputStream src ^Path target copy-options))))

(defn read-all-lines
  ([^Path path]
   (read-all-lines path StandardCharsets/UTF_8))
  ([^Path path ^Charset charset]
   (Files/readAllLines path charset)))

(defn write-bytes [^Path path ^bytes bytes & open-options]
  (let [^"[Ljava.nio.file.OpenOption;" open-options (open-opts open-options)]
    (Files/write path bytes open-options)))

(defn write-lines
  ([^Path path lines]
   (write-lines path lines StandardCharsets/UTF_8))
  ([^Path path lines ^Charset charset & open-options]
   (Files/write path lines charset (open-opts open-options))))

;; UTILS

(defn- parse-args [[f s :as rest]]
  (cond
    (and (map? f) (:type f)) [f s]
    (vector? f) [{:type :dir} rest]
    (and (map? f) (vector? s)) [(assoc f :type :dir) s]
    (map? f) [(assoc f :type :file) s]
    :else [{:type :file} rest]))

(defn create-fs-tree! [^FileSystem fs ^String root children]
  (doseq [[file-name & rest] children
          :let [[attrs rest] (parse-args rest)
                ^Path my-path (path fs root (name file-name))]]
    (condp = (:type attrs)
      :dir (do
             (create-dir my-path)
             (when (seq rest)
               (create-fs-tree!
                 fs
                 (str my-path)
                 rest)))
      :file (do
              (create-file my-path)
              (when (seq rest)
                (write-lines my-path rest)))
      :link (do
              (assert (:link-to attrs)
                      (str "Attribute :link-to missing. path=" my-path))
              (create-hard-link my-path (path fs (:link-to attrs))))
      :sym-link (do
                  (assert (:link-to attrs)
                          (str "Attribute :link-to missing. path=" my-path))
                  (create-sym-link my-path (path fs (:link-to attrs))))
      (throw (ex-info "Illegal type" file-name)))))

(defn posix-perm-file-attrs [perm-str]
  (PosixFilePermissions/asFileAttribute
    (PosixFilePermissions/fromString
      perm-str)))
