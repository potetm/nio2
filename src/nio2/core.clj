(ns nio2.core
  (:refer-clojure :exclude [resolve])
  (:import [java.io File InputStream OutputStream]
           [java.net URI]
           [java.nio.file CopyOption
                          DirectoryStream
                          LinkOption
                          FileAlreadyExistsException
                          Files
                          FileSystem
                          FileSystems
                          OpenOption
                          Path]
           [java.nio.file.attribute FileAttribute FileTime PosixFilePermissions]
           [java.nio.charset Charset StandardCharsets]
           [java.util Collection HashSet Set]))

(defn ^FileSystem default-fs []
  (FileSystems/getDefault))

(defmacro varargs-array [type args]
  `(into-array ~type (or ~args [])))

(defmacro copy-opts [args] `(varargs-array CopyOption ~args))
(defmacro file-attrs [args] `(varargs-array FileAttribute ~args))
(defmacro link-opts [args] `(varargs-array LinkOption ~args))
(defmacro open-opts [args] `(varargs-array OpenOption ~args))

;; Path fns

(defn ^Path path [^FileSystem fs ^String path & paths]
  (.getPath fs path (varargs-array String paths)))

(defn ^Path absolute [^Path path]
  (.toAbsolutePath path))

(defn ^File file [^Path path]
  (.toFile path))

(defn ^Path filename [^Path path]
  (.getFileName path))

(defn ^FileSystem get-fs [^Path path]
  (.getFileSystem path))

(defn ^FileSystem join [^Path parent ^Path child]
  (.resolve parent child))

(defn ^Path normalize [^Path path]
  (.normalize path))

(defn ^Path parent [^Path path]
  (.getParent path))

(defn relativize [^Path from ^Path to]
  (.relativize from to))

(def resolve join)

(defn ^Path root [^Path path]
  (.getRoot path))

(defn split [^Path path]
  (iterator-seq (.iterator path)))

(defn ^URI uri [^Path path]
  (.toUri path))

;; QUERY

(defn ^Boolean absolute? [^Path path]
  (.isAbsolute path))

(defn ^Boolean executable? [^Path path]
  (Files/isExecutable path))

(defn ^Boolean exists? [^Path path & link-options]
  (Files/exists path (link-opts link-options)))

(defn ^Boolean file? [^Path path & link-options]
  (Files/isRegularFile path (link-opts link-options)))

(defn ^Boolean dir? [^Path path & link-options]
  (Files/isDirectory path (link-opts link-options)))

(defn ^Boolean hidden? [^Path path]
  (Files/isHidden path))

(defn ^Boolean readable? [^Path path]
  (Files/isReadable path))

(defn ^Boolean same-file? [^Path path1 ^Path path2]
  (Files/isSameFile path1 path2))

(defn ^Boolean sym-link? [^Path path]
  (Files/isSymbolicLink path))

(defn ^Boolean writable? [^Path path]
  (Files/isWritable path))

;; INTERACT

(defn ^DirectoryStream dir-stream
  "Lazily list the contents of a directory.

   Returns a new DirectoryStream. Should be used inside a with-open block.

   Because Streams implement Iterable, it can basically be used as a clojure seq."
  ([^Path path]
    (Files/newDirectoryStream path))
  ([^Path path ^String glob]
    (Files/newDirectoryStream path glob)))

(defn attr [^Path path ^String attr & link-options]
  (Files/getAttribute path attr (link-opts link-options)))

(defn attrs [^Path path ^String attributes & link-options]
  (let [^"[Ljava.nio.file.LinkOption;" link-options (link-opts link-options)]
    (Files/readAttributes path attributes link-options)))

(defn file-store [^Path path]
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
  ([^Path path]
    (buffered-reader path StandardCharsets/UTF_8))
  ([^Path path ^Charset charset]
    (Files/newBufferedReader path charset)))

(defn buffered-writer
  ([^Path path]
    (buffered-writer path StandardCharsets/UTF_8))
  ([^Path path ^Charset charset & open-options]
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

(defn posix-perm-file-attrs [^String perm-str]
  (PosixFilePermissions/asFileAttribute
    (PosixFilePermissions/fromString
      perm-str)))

(defn posix-perms [^String perms]
  (PosixFilePermissions/fromString perms))
