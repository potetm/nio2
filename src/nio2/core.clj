(ns nio2.core
  (:refer-clojure :exclude [resolve])
  (:import [java.io BufferedReader
                    BufferedWriter
                    File
                    InputStream
                    OutputStream]
           [java.net URI]
           [java.nio.file CopyOption
                          DirectoryStream
                          LinkOption
                          FileAlreadyExistsException
                          Files
                          FileStore
                          FileSystem
                          FileSystems
                          OpenOption
                          Path]
           [java.nio.file.attribute FileAttribute
                                    FileTime
                                    PosixFilePermissions]
           [java.nio.charset Charset
                             StandardCharsets]
           [java.util Collection
                      HashSet
                      List
                      Set]))

(defn ^FileSystem default-fs []
  (FileSystems/getDefault))

(defmacro varargs-array [type args]
  `(into-array ~type (or ~args [])))

(defmacro copy-opts [args] `(varargs-array CopyOption ~args))
(defmacro file-attrs [args] `(varargs-array FileAttribute ~args))
(defmacro link-opts [args] `(varargs-array LinkOption ~args))
(defmacro open-opts [args] `(varargs-array OpenOption ~args))

;; Path fns

(def ^{:dynamic true :tag FileSystem} *fs*)

(defprotocol ^:private IPath
  (-path ^java.nio.file.Path [this paths]))

(extend-type FileSystem
  IPath
  (-path ^java.nio.file.Path [this [path & paths]]
    (let [^FileSystem this this]
      (.getPath this path (varargs-array String paths)))))

(extend-type String
  IPath
  (-path ^java.nio.file.Path [this paths]
    (.getPath *fs* this (varargs-array String paths))))

(extend-type Path
  IPath
  (-path ^java.nio.file.Path [this paths]
    (if (seq paths)
      (.getPath (.getFileSystem this)
                (str this)
                (varargs-array String (map str paths)))
      this)))

(defn path ^java.nio.file.Path [& [fs-or-path-str & paths]]
  (-path fs-or-path-str paths))

(defn absolute ^java.nio.file.Path [^Path path]
  (.toAbsolutePath path))

(defn file ^java.io.File [^Path path]
  (.toFile path))

(defn filename ^java.nio.file.Path [^Path path]
  (.getFileName path))

(defn get-fs ^java.nio.file.FileSystem [^Path path]
  (.getFileSystem path))

(defn join ^java.nio.file.Path [^Path parent ^Path child]
  (.resolve parent child))

(defn normalize ^java.nio.file.Path [^Path path]
  (.normalize path))

(defn parent ^java.nio.file.Path [^Path path]
  (.getParent path))

(defn relativize ^java.nio.file.Path [^Path from ^Path to]
  (.relativize from to))

(def resolve join)

(defn root ^java.nio.file.Path [^Path path]
  (.getRoot path))

(defn split [^Path path]
  (iterator-seq (.iterator path)))

(defn uri ^java.net.URI [^Path path]
  (.toUri path))

;; QUERY

(defn absolute? ^Boolean [^Path path]
  (.isAbsolute path))

(defn executable? ^Boolean [^Path path]
  (Files/isExecutable path))

(defn exists? ^Boolean [^Path path & link-options]
  (Files/exists path (link-opts link-options)))

(defn file? ^Boolean [^Path path & link-options]
  (Files/isRegularFile path (link-opts link-options)))

(defn dir? ^Boolean [^Path path & link-options]
  (Files/isDirectory path (link-opts link-options)))

(defn hidden? ^Boolean [^Path path]
  (Files/isHidden path))

(defn readable? ^Boolean [^Path path]
  (Files/isReadable path))

(defn same-file? ^Boolean [^Path path1 ^Path path2]
  (Files/isSameFile path1 path2))

(defn sym-link? ^Boolean [^Path path]
  (Files/isSymbolicLink path))

(defn writable? ^Boolean [^Path path]
  (Files/isWritable path))

;; INTERACT

(defn dir-stream
  "Lazily list the contents of a directory.

   Returns a new DirectoryStream. Should be used inside a with-open block.

   Because Streams implement Iterable, it can basically be used as a clojure seq."
  (^java.nio.file.DirectoryStream
  [^Path path]
   (Files/newDirectoryStream path))
  (^java.nio.file.DirectoryStream
  [^Path path ^String glob]
   (Files/newDirectoryStream path glob)))

(defn attr [^Path path ^String attr & link-options]
  (Files/getAttribute path attr (link-opts link-options)))

(defn attrs [^Path path ^String attributes & link-options]
  (let [^"[Ljava.nio.file.LinkOption;" link-options (link-opts link-options)]
    (Files/readAttributes path attributes link-options)))

(defn file-store ^java.nio.file.FileStore [^Path path]
  (Files/getFileStore path))

(defn last-modified
  "Get the last modified time in millis from the epoch"
  [^Path path & link-options]
  (.toMillis ^FileTime (Files/getLastModifiedTime path (link-opts link-options))))

(defn owner [^Path path & link-options]
  (.getName (Files/getOwner path (link-opts link-options))))

(defn probe-content-type [^Path path]
  (Files/probeContentType path))

(defn read-sym-link [^Path path]
  (Files/readSymbolicLink path))

(defn set-attr [^Path path ^String attribute ^Object value & link-options]
  (Files/setAttribute path attribute value (link-opts link-options)))

(defn set-posix-perms [^Path path ^Set posix-perms]
  (Files/setPosixFilePermissions path posix-perms))

(defn size [^Path path]
  (Files/size path))

;; CREATE / DELETE

(defn create-dir [^Path path & file-attributes]
  (try
    ;; the check for file exists is atomic in this method, so we want to take advantage of that
    (Files/createDirectory path (file-attrs file-attributes))
    (catch FileAlreadyExistsException _)))

(defn create-dirs [^Path path & file-attributes]
  (Files/createDirectories path (file-attrs file-attributes)))

(defn create-file [^Path path & file-attributes]
  (try
    ;; the check for file exists is atomic in this method, so we want to take advantage of that
    (Files/createFile path (file-attrs file-attributes))
    (catch FileAlreadyExistsException _)))

(defn create-hard-link [^Path link ^Path existing]
  (Files/createLink link existing))

(defn create-sym-link [^Path link ^Path existing & file-attributes]
  (Files/createSymbolicLink link existing (file-attrs file-attributes)))

(defn create-tmp-dir [^Path dir ^String prefix & file-attributes]
  (Files/createTempDirectory dir prefix (file-attrs file-attributes)))

(defn create-tmp-dir-on-default-fs [^String prefix & file-attributes]
  (Files/createTempDirectory prefix (file-attrs file-attributes)))

(defn create-tmp-file [^Path dir ^String prefix ^String suffix & file-attributes]
  (Files/createTempFile dir prefix suffix (file-attrs file-attributes)))

(defn create-tmp-file-on-default-fs [^String prefix ^String suffix & file-attributes]
  (Files/createTempFile prefix suffix (file-attrs file-attributes)))

(defn move [^Path source ^Path target & copy-options]
  (Files/move source target (copy-opts copy-options)))

(defn delete [^Path path]
  (Files/deleteIfExists path))

;; IO

(defn buffered-reader
  (^java.io.BufferedReader [^Path path]
   (buffered-reader path StandardCharsets/UTF_8))
  (^java.io.BufferedReader [^Path path ^Charset charset]
   (Files/newBufferedReader path charset)))

(defn buffered-writer
  (^java.io.BufferedWriter [^Path path]
   (buffered-writer path StandardCharsets/UTF_8))
  (^java.io.BufferedWriter [^Path path ^Charset charset & open-options]
   (Files/newBufferedWriter path charset (open-opts open-options))))

(defn byte-channel [^Path path open-options & file-attributes]
  (Files/newByteChannel path (HashSet. ^Collection open-options) (file-attrs file-attributes)))

(defn copy-file [src target & copy-options]
  (let [^"[Ljava.nio.file.CopyOption;" copy-options (copy-opts copy-options)]
    (if (instance? Path src)
      (if (instance? Path target)
        (Files/copy ^Path src ^Path target copy-options)
        (Files/copy ^Path src ^OutputStream target))
      (Files/copy ^InputStream src ^Path target copy-options))))

(defn input-stream [^Path path & open-options]
  (Files/newInputStream path (open-opts open-options)))

(defn output-stream [^Path path & open-options]
  (Files/newOutputStream path (open-opts open-options)))

(defn read-all-bytes [^Path path]
  (Files/readAllBytes path))

(defn read-all-lines
  (^List [^Path path]
   (read-all-lines path StandardCharsets/UTF_8))
  ([^Path path ^Charset charset]
   (Files/readAllLines path charset)))

(defn write-bytes [^Path path ^bytes bytes & open-options]
  (let [^"[Ljava.nio.file.OpenOption;" open-options (open-opts open-options)]
    (Files/write path bytes open-options)))

(defn write-lines
  (^Path [^Path path lines]
   (write-lines path lines StandardCharsets/UTF_8))
  (^Path [^Path path lines ^Charset charset & open-options]
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

(defn posix-perm-file-attrs [^String perm-str]
  (PosixFilePermissions/asFileAttribute
    (PosixFilePermissions/fromString
      perm-str)))

(defn posix-perms [^String perms]
  (PosixFilePermissions/fromString perms))
