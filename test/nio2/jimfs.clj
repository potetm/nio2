(ns nio2.jimfs
  (:require [nio2.core :as nio2])
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
      (nio2/create-fs-tree! fs (fs-root config) struct)
      fs)))
