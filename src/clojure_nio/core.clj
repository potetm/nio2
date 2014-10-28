(ns clojure-nio.core
  (:import [java.nio.file FileSystem Files LinkOption Path OpenOption FileSystems CopyOption DirectoryStream FileStore]
           [java.nio.file.attribute FileAttribute PosixFilePermissions]
           [java.nio.charset StandardCharsets Charset]
           [java.io OutputStream InputStream]
           [java.util.concurrent TimeUnit]
           [java.util HashSet Collection]))

(defn ^FileSystem default-fs []
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

(defn executable? [^Path path]
  (Files/isExecutable path))

(defn exists? [^Path path & link-options]
  (Files/exists path (link-opts link-options)))

(defn file? [^Path path & link-options]
  (Files/isRegularFile path (link-opts link-options)))

(defn dir? [^Path path & link-options]
  (Files/isDirectory path (link-opts link-options)))

(defn hidden? [^Path path]
  (Files/isHidden path))

(defn readable? [^Path path]
  (Files/isReadable path))

(defn same-file? [^Path path1 ^Path path2]
  (Files/isSameFile path1 path2))

(defn sym-link? [^Path path]
  (Files/isSymbolicLink path))

(defn writable? [^Path path]
  (Files/isWritable path))

;; INTERACT

(defn read-sym-link [^Path path]
  (Files/readSymbolicLink path))

(defn get-attr [^Path path ^String attr & link-options]
  (Files/getAttribute path attr (link-opts link-options)))

(defn get-file-store [^Path path]
  (Files/getFileStore path))

(defn get-last-modified [^Path path & link-options]
  (.toMillis (Files/getLastModifiedTime path (link-opts link-options))))

(defn get-owner [^Path path & link-options]
  (.getName (Files/getOwner path (link-opts link-options))))

(defn list-dir [^Path path]
  (let [files (atom [])]
    (with-open [^DirectoryStream stream (Files/newDirectoryStream path)]
      (doseq [file stream]
        (swap! files conj file)))
    @files))

;; CREATE / DELETE

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

(defn create-tmp-dir [^Path dir ^String prefix & file-attributes]
  (Files/createTempDirectory dir prefix (file-attrs file-attributes)))

(defn create-tmp-dir-on-default-fs [^String prefix & file-attributes]
  (Files/createTempDirectory prefix (file-attrs file-attributes)))

(defn create-tmp-file [^Path dir ^String prefix ^String suffix & file-attributes]
  (Files/createTempFile dir prefix suffix (file-attrs file-attributes)))

(defn create-tmp-file-on-default-fs [^String prefix ^String suffix & file-attributes]
  (Files/createTempFile prefix suffix (file-attrs file-attributes)))

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
