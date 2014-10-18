(ns clojure-nio.jimfs
  (:require [clojure-nio.core :as nio])
  (:import [com.google.common.jimfs Configuration Jimfs]
           [java.nio.file FileSystem Files LinkOption Path OpenOption]
           [java.nio.file.attribute FileAttribute PosixFilePermissions]
           [java.io File]
           [java.nio.charset StandardCharsets]
           [java.util ArrayList]))

(defn configs []
  {:unix {:configuration (Configuration/unix)}
  :ox })

(defn create-fs
  ([struct]
   (create-fs struct :unix))
  ([struct config]
   (let [^FileSystem fs (Jimfs/newFileSystem (condp config =
                                               :unix (Configuration/unix)
                                               :osx (Configuration/osX)
                                               :windows (Configuration/windows)))
         ^String root (condp config =
                        :unix "/"
                        :osx "/"
                        :windows "C:\\")]
     (nio/create-fs-tree! fs root struct)
     fs)))
