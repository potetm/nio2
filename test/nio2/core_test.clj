(ns nio2.core-test
  (:import (java.nio.file FileSystems Path LinkOption)
           (java.io File)
           (java.net URI))
  (:require [clojure.test :refer :all]
            [nio2.core :as nio2]
            [nio2.jimfs :as jimfs]))

(deftest default-fs
  (testing "it returns the default fs"
    (is (= (nio2/default-fs) (FileSystems/getDefault)))))

(deftest path
  (let [fs (jimfs/create-fs [])]
    (testing "it returns a path"
      (is (instance? Path (nio2/path fs "path"))))
    (testing "the varargs form"
      (is (= (nio2/path fs "path/to/file")
             (nio2/path fs "path" "to" "file"))))
    (testing "it resolves from *fs* when not supplied"
      (with-bindings {#'nio2.core/*fs* fs}
        (is (instance? Path (nio2/path "path")))))
    (testing "resolving from *fs* works with multiple arguments"
      (with-bindings {#'nio2.core/*fs* fs}
        (is (instance? Path (nio2/path "path" "to" "file")))))))

(deftest absolute
  (testing
    "It makes an absolute path.

     NOTE: The way absolute path resolution works is by
     resolving the path to some fs-dependent default dir. In
     JimFS's case, that's /work.

     http://docs.oracle.com/javase/7/docs/api/java/nio/file/Path.html#toAbsolutePath%28%29"
    (let [fs (jimfs/create-fs [])]
      (is (= (nio2/absolute (nio2/path fs "file"))
             (nio2/path fs "/work/file"))))))

(deftest file
  (testing
    "It returns a file. NOTE: JimFS can't do this so we use the default fs"
    (let [tmp-dir (nio2/create-tmp-dir-on-default-fs "nio-test")]
      (is (= (str tmp-dir)
             (.getAbsolutePath ^File (nio2/file tmp-dir)))))))

(deftest filename
  (testing "It returns the filename"
    (let [fs (jimfs/create-fs [])]
      (is (= (nio2/filename (nio2/path fs "path/to/file"))
             (nio2/path fs "file"))))))

(deftest get-fs
  (testing "It returns the fs"
    (let [fs (jimfs/create-fs [])]
      (is (= (nio2/get-fs (nio2/path fs "path/to/file"))
             fs)))))

(deftest join
  (testing "It joins two nio2"
    (let [fs (jimfs/create-fs [])
          parent (nio2/path fs "/parent")
          child (nio2/path fs "child")]
      (is (= (nio2/join parent child)
             (nio2/path fs "/parent/child"))))))

(deftest normalize
  (testing "It normalizes"
    (let [fs (jimfs/create-fs [])]
      (is (= (nio2/path fs "path/to/file")
             (nio2/normalize (nio2/path fs "path/../path/./to/file")))))))

(deftest parent
  (testing "It parents"
    (let [fs (jimfs/create-fs [])]
      (is (= (nio2/parent (nio2/path fs "parent/child"))
             (nio2/path fs "parent"))))))

(deftest relativize
  (testing "it makes a relative path"
    (let [fs (jimfs/create-fs [])]
      (is (= (nio2/path fs "to/file")
             (nio2/relativize (nio2/path fs "/my/path")
                              (nio2/path fs "/my/path/to/file")))))))

(deftest root
  (testing "it roots"
    (let [fs (jimfs/create-fs [])]
      (is (= (nio2/root (nio2/path fs "/path/to/file"))
             (nio2/path fs "/")))))
  (testing "it returns nil for relative nio2"
    (let [fs (jimfs/create-fs [])]
      (is (nil? (nio2/root (nio2/path fs "relative")))))))

(deftest split
  (testing "it returns a seq of path parts"
    (let [fs (jimfs/create-fs
               [[:dir1
                 [:dir2
                  [:file1]]]])]
      (is (= (nio2/split (nio2/path fs "/dir1/dir2/file1"))
             (map (partial nio2/path fs)
                  ["dir1" "dir2" "file1"]))))))

(deftest uri
  (testing "it uris"
    (let [fs (jimfs/create-fs [])]
      (is (instance? URI (nio2/uri (nio2/path fs "/path/to/file"))))
      (is (.endsWith (str (nio2/uri (nio2/path fs "/path/to/file")))
                     "/path/to/file")))))

(deftest exists?
  (testing "it returns true if the file exists"
    (let [fs (jimfs/create-fs [[:i-exist]])]
      (is (nio2/exists? (nio2/path fs "/i-exist")))))
  (testing "it returns false if the file don't exist"
    (is (not (nio2/exists? (nio2/path (jimfs/create-fs []) "no-exist")))))
  (testing "multi-arity"
    (let [fs (jimfs/create-fs
               [[:link {:type :sym-link
                        :link-to "/target"}]])]
      (is (nio2/exists?
            (nio2/path fs "/link")
            LinkOption/NOFOLLOW_LINKS))
      (is (not (nio2/exists? (nio2/path fs "/link")))))))

(deftest dir-stream
  (testing "it works"
    (let [fs (jimfs/create-fs
               [[:dir1
                 [:file1]
                 [:dir2 {:type :dir}]]])]
      (with-open [ds (nio2/dir-stream (nio2/path fs "/dir1"))]
        (is (= (set ds)
               #{(nio2/path fs "/dir1/file1")
                 (nio2/path fs "/dir1/dir2")})))))
  (testing "it can be filtered"
    (let [fs (jimfs/create-fs
               [[:dir1
                 [:file1]
                 [:dir2 {:type :dir}]]])]
      (with-open [ds (nio2/dir-stream (nio2/path fs "/dir1"))]
        (is (= (set (filter (partial = (nio2/path fs "/dir1/file1")) ds))
               #{(nio2/path fs "/dir1/file1")})))))
  (testing "it can be mapped"
    (let [fs (jimfs/create-fs
               [[:dir1
                 [:file1]
                 [:dir2 {:type :dir}]]])]
      (with-open [ds (nio2/dir-stream (nio2/path fs "/dir1"))]
        (is (= (set (map (comp str nio2/filename) ds))
               #{"file1" "dir2"})))))
  (testing "the glob"
    (let [fs (jimfs/create-fs
               [[:dir
                 [:matches]
                 [:dont-match]]])]
      (with-open [ds (nio2/dir-stream (nio2/path fs "/dir") "mat*")]
        (is (= [(nio2/path fs "/dir/matches")]
               (into [] ds)))))))

(deftest last-modified
  (testing "it returns the last modified date for a file"
    (let [fs (jimfs/create-fs
               [[:file]])]
      (is (number? (nio2/last-modified (nio2/path fs "/file")))))))

(deftest create-fs-test
  (testing "a single file"
    (let [s [[:foo]]
          fs (jimfs/create-fs s)
          path (nio2/path fs "/foo")]
      (is (nio2/exists? path))
      (is (nio2/file? path))))

  (testing "two files"
    (let [s [[:foo]
             [:bar]]
          fs (jimfs/create-fs s)
          foo (nio2/path fs "/foo")
          bar (nio2/path fs "/bar")]
      (is (nio2/exists? foo))
      (is (nio2/file? foo))
      (is (nio2/exists? bar))
      (is (nio2/file? bar))))

  (testing "the negative case"
    (let [s [[:foo]]
          fs (jimfs/create-fs s)]
      (is (not (nio2/exists? (nio2/path fs "/not-exists"))))))

  (testing "nesting"
    (let [my-fs-struct [[:foo
                         [:bar
                          [:baz]]]]
          fs (jimfs/create-fs my-fs-struct)
          path (nio2/path fs "/foo/bar/baz")]
      (is (nio2/exists? path))
      (is (nio2/file? path))))

  (testing "creating an empty dir"
    (let [s [[:foo {:type :dir}]]
          fs (jimfs/create-fs s)]
      (is (nio2/dir? (nio2/path fs "/foo")))))

  (testing "creating a sym link"
    (let [s [[:foo]
             [:linky {:type :sym-link
                      :link-to "/foo"}]]
          fs (jimfs/create-fs s)
          link (nio2/path fs "/linky")]
      (is (nio2/exists? link))
      (is (nio2/sym-link? link))
      (is (= (nio2/read-sym-link link) (nio2/path fs "/foo")))))

  (testing "creating a sym link without a :link-to throws an AssertionError"
    (let [s [[:foo]
             [:linky {:type :sym-link}]]]
      (is (thrown? AssertionError (jimfs/create-fs s)))))

  (testing "creating a hard link; notice it must be ordered after its :link-to"
    (let [s [[:foo]
             [:hardlink {:type :link
                         :link-to "/foo"}]]
          fs (jimfs/create-fs s)
          path (nio2/path fs "/hardlink")]
      (is (nio2/exists? path))
      (is (nio2/file? path))))

  (testing "creating a sym link without a :link-to throws an AssertionError"
    (let [s [[:foo]
             [:hardlink {:type :link}]]]
      (is (thrown? AssertionError (jimfs/create-fs s)))))

  (testing "writing contents"
    (let [s [[:foo "hello, world!"]]
          fs (jimfs/create-fs s)]
      (is (= (nio2/read-all-lines (nio2/path fs "/foo"))
             ["hello, world!"]))))

  (testing "complex structure"
    (let [fs (jimfs/create-fs
               [[:my
                 [:path
                  [:to
                   [:file]
                   [:has-content "line 1" "line 2"]]
                  [:empty-dir {:type :dir}]]
                 [:link {:type :sym-link, :link-to "/my/path/to"}]]
                [:hard-link {:type :link, :link-to "/my/path/to/file"}]])]
      (are [result path] (= result (nio2/dir? (nio2/path fs path)))
                         true "/my"
                         true "/my/path"
                         true "/my/path/to"
                         true "/my/path/empty-dir")
      (is (nio2/file? (nio2/path fs "/my/path/to/file")))
      (is (nio2/file? (nio2/path fs "/hard-link")))
      (is (nio2/sym-link? (nio2/path fs "/my/link")))
      (is (= (nio2/read-sym-link (nio2/path fs "/my/link")) (nio2/path fs "/my/path/to")))
      (is (= (nio2/read-all-lines (nio2/path fs "/my/path/to/has-content"))
             ["line 1"
              "line 2"])))))

(deftest create-file
  (testing "creating a file that already exists returns nil"
    (testing "creating a file that already exists returns nil and doesn't throw exception"
      (let [fs (jimfs/create-fs
                 [[:dir1
                   [:dir2
                    [:file1]]]])]
        (is (nil? (nio2/create-file (nio2/path fs "/dir1/dir2/file1"))))))
    (testing "creating a file for a path that exists and isn't a file returns nil and doesn't throw exception"
      (let [fs (jimfs/create-fs
                 [[:dir1
                   [:dir2
                    [:file1]]]])]
        (is (nil? (nio2/create-file (nio2/path fs "/dir1/dir2"))))))))

(deftest create-dir
  (testing "creating a dir that already exists returns nil"
    (testing "creating a dir that already exists returns nil and doesn't throw exception"
      (let [fs (jimfs/create-fs
                 [[:dir1
                   [:dir2
                    [:file1]]]])]
        (is (nil? (nio2/create-dir (nio2/path fs "/dir1/dir2")))))))
  (testing "creating a dir for a path that exists and isn't a dir returns nil and doesn't throw exception"
    (let [fs (jimfs/create-fs
               [[:dir1
                 [:dir2
                  [:file1]]]])]
      (is (nil? (nio2/create-dir (nio2/path fs "/dir1/dir2/file1")))))))
