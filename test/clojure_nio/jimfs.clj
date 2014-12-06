(ns clojure-nio.jimfs
  (:require [clojure-nio.core :as nio])
  (:import [com.google.common.jimfs Configuration Jimfs]))

(defprotocol IConfiguration
  (os [_])
  (init-fs [_])
  (fs-root [_]))

(defrecord OsConfiguration [os init-config rootfs]
  IConfiguration
  (os [_]
    os)
  (init-fs [_]
    (Jimfs/newFileSystem (init-config)))
  (fs-root [_]
    rootfs))

(def configurations
  {:unix (->OsConfiguration :unix #(Configuration/unix) "/")
   :osx (->OsConfiguration :osx #(Configuration/osX) "/")
   :windows (->OsConfiguration :windows #(Configuration/windows) "C:\\")})

(defn create-fs
  ([struct]
    (create-fs struct :unix))
  ([struct os-kw]
    (let [config (os-kw configurations)
          fs (init-fs config)]
      (nio/create-fs-tree! fs (fs-root config) struct)
      fs)))
