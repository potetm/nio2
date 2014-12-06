(ns clojure-nio.core-test
  (:import (java.nio.file FileSystems Path LinkOption)
           (java.io File)
           (java.net URI))
  (:require [clojure.test :refer :all]
            [clojure-nio.core :as nio]
            [clojure-nio.jimfs :as jimfs]))

(deftest default-fs
  (testing "it returns the default fs"
    (is (= (nio/default-fs) (FileSystems/getDefault)))))

(deftest path
  (let [fs (jimfs/create-fs [])]
    (testing "it returns a path"
      (is (instance? Path (nio/path fs "path"))))
    (testing "the varargs form"
      (is (= (nio/path fs "path/to/file")
             (nio/path fs "path" "to" "file"))))))

(deftest absolute
  (testing
    "It makes an absolute path.

     NOTE: The way absolute path resolution works is by
     resolving the path to some fs-dependent default dir. In
     JimFS's case, that's /work.

     http://docs.oracle.com/javase/7/docs/api/java/nio/file/Path.html#toAbsolutePath%28%29"
    (let [fs (jimfs/create-fs [])]
      (is (= (nio/absolute (nio/path fs "file"))
             (nio/path fs "/work/file"))))))

(deftest file
  (testing
    "It returns a file. NOTE: JimFS can't do this so we use the default fs"
    (let [tmp-dir (nio/create-tmp-dir-on-default-fs "nio-test")]
      (is (= (str tmp-dir)
             (.getAbsolutePath ^File (nio/file tmp-dir)))))))

(deftest filename
  (testing "It returns the filename"
    (let [fs (jimfs/create-fs [])]
      (is (= (nio/filename (nio/path fs "path/to/file"))
             (nio/path fs "file"))))))

(deftest get-fs
  (testing "It returns the fs"
    (let [fs (jimfs/create-fs [])]
      (is (= (nio/get-fs (nio/path fs "path/to/file"))
             fs)))))

(deftest join
  (testing "It joins two paths"
    (let [fs (jimfs/create-fs [])
          parent (nio/path fs "/parent")
          child (nio/path fs "child")]
      (is (= (nio/join parent child)
             (nio/path fs "/parent/child"))))))

(deftest normalize
  (testing "It normalizes"
    (let [fs (jimfs/create-fs [])]
      (is (= (nio/path fs "path/to/file")
             (nio/normalize (nio/path fs "path/../path/./to/file")))))))

(deftest parent
  (testing "It parents"
    (let [fs (jimfs/create-fs [])]
      (is (= (nio/parent (nio/path fs "parent/child"))
             (nio/path fs "parent"))))))

(deftest relativize
  (testing "it makes a relative path"
    (let [fs (jimfs/create-fs [])]
      (is (= (nio/path fs "to/file")
             (nio/relativize (nio/path fs "/my/path")
                             (nio/path fs "/my/path/to/file")))))))

(deftest root
  (testing "it roots"
    (let [fs (jimfs/create-fs [])]
      (is (= (nio/root (nio/path fs "/path/to/file"))
             (nio/path fs "/")))))
  (testing "it returns nil for relative paths"
    (let [fs (jimfs/create-fs [])]
      (is (nil? (nio/root (nio/path fs "relative")))))))

(deftest split
  (testing "it returns a seq of path parts"
    (let [fs (jimfs/create-fs
               [[:dir1
                 [:dir2
                  [:file1]]]])]
      (is (= (nio/split (nio/path fs "/dir1/dir2/file1"))
             (map (partial nio/path fs)
                  ["dir1" "dir2" "file1"]))))))

(deftest uri
  (testing "it uris"
    (let [fs (jimfs/create-fs [])]
      (is (instance? URI (nio/uri (nio/path fs "/path/to/file"))))
      (is (.endsWith (str (nio/uri (nio/path fs "/path/to/file")))
                     "/path/to/file")))))

(deftest exists?
  (testing "it returns true if the file exists"
    (let [fs (jimfs/create-fs [[:i-exist]])]
      (is (nio/exists? (nio/path fs "/i-exist")))))
  (testing "it returns false if the file don't exist"
    (is (not (nio/exists? (nio/path (jimfs/create-fs []) "no-exist")))))
  (testing "multi-arity"
    (let [fs (jimfs/create-fs
               [[:link {:type :sym-link
                        :link-to "/target"}]])]
      (is (nio/exists?
            (nio/path fs "/link")
            LinkOption/NOFOLLOW_LINKS))
      (is (not (nio/exists? (nio/path fs "/link")))))))

(deftest dir-stream
  (testing "it works"
    (let [fs (jimfs/create-fs
               [[:dir1
                 [:file1]
                 [:dir2 {:type :dir}]]])]
      (with-open [ds (nio/dir-stream (nio/path fs "/dir1"))]
        (is (= (set ds)
               #{(nio/path fs "/dir1/file1")
                 (nio/path fs "/dir1/dir2")})))))
  (testing "it can be filtered"
    (let [fs (jimfs/create-fs
               [[:dir1
                 [:file1]
                 [:dir2 {:type :dir}]]])]
      (with-open [ds (nio/dir-stream (nio/path fs "/dir1"))]
        (is (= (set (filter (partial = (nio/path fs "/dir1/file1")) ds))
               #{(nio/path fs "/dir1/file1")})))))
  (testing "it can be mapped"
    (let [fs (jimfs/create-fs
               [[:dir1
                 [:file1]
                 [:dir2 {:type :dir}]]])]
      (with-open [ds (nio/dir-stream (nio/path fs "/dir1"))]
        (is (= (set (map (comp str nio/filename) ds))
               #{"file1" "dir2"})))))
  (testing "the glob"
    (let [fs (jimfs/create-fs
               [[:dir
                 [:matches]
                 [:dont-match]]])]
      (with-open [ds (nio/dir-stream (nio/path fs "/dir") "mat*")]
        (is (= [(nio/path fs "/dir/matches")]
               (into [] ds)))))))

(deftest last-modified
  (testing "it returns the last modified date for a file"
    (let [fs (jimfs/create-fs
               [[:file]])]
      (is (number? (nio/last-modified (nio/path fs "/file")))))))

(deftest create-fs-test
  (testing "a single file"
    (let [s [[:foo]]
          fs (jimfs/create-fs s)
          path (nio/path fs "/foo")]
      (is (nio/exists? path))
      (is (nio/file? path))))

  (testing "two files"
    (let [s [[:foo]
             [:bar]]
          fs (jimfs/create-fs s)
          foo (nio/path fs "/foo")
          bar (nio/path fs "/bar")]
      (is (nio/exists? foo))
      (is (nio/file? foo))
      (is (nio/exists? bar))
      (is (nio/file? bar))))

  (testing "the negative case"
    (let [s [[:foo]]
          fs (jimfs/create-fs s)]
      (is (not (nio/exists? (nio/path fs "/not-exists"))))))

  (testing "nesting"
    (let [my-fs-struct [[:foo
                         [:bar
                          [:baz]]]]
          fs (jimfs/create-fs my-fs-struct)
          path (nio/path fs "/foo/bar/baz")]
      (is (nio/exists? path))
      (is (nio/file? path))))

  (testing "creating an empty dir"
    (let [s [[:foo {:type :dir}]]
          fs (jimfs/create-fs s)]
      (is (nio/dir? (nio/path fs "/foo")))))

  (testing "creating a sym link"
    (let [s [[:foo]
             [:linky {:type :sym-link
                      :link-to "/foo"}]]
          fs (jimfs/create-fs s)
          link (nio/path fs "/linky")]
      (is (nio/exists? link))
      (is (nio/sym-link? link))
      (is (= (nio/read-sym-link link) (nio/path fs "/foo")))))

  (testing "creating a sym link without a :link-to throws an AssertionError"
    (let [s [[:foo]
             [:linky {:type :sym-link}]]]
      (is (thrown? AssertionError (jimfs/create-fs s)))))

  (testing "creating a hard link; notice it must be ordered after its :link-to"
    (let [s [[:foo]
             [:hardlink {:type :link
                         :link-to "/foo"}]]
          fs (jimfs/create-fs s)
          path (nio/path fs "/hardlink")]
      (is (nio/exists? path))
      (is (nio/file? path))))

  (testing "creating a sym link without a :link-to throws an AssertionError"
    (let [s [[:foo]
             [:hardlink {:type :link}]]]
      (is (thrown? AssertionError (jimfs/create-fs s)))))

  (testing "writing contents"
    (let [s [[:foo "hello, world!"]]
          fs (jimfs/create-fs s)]
      (is (= (nio/read-all-lines (nio/path fs "/foo"))
             ["hello, world!"]))))

  (testing "complex structure"
    (let [s [[:my
              [:path
               [:to
                [:file]
                [:has-content "line 1" "line 2"]]
               [:empty-dir {:type :dir}]]
              [:link {:type :sym-link, :link-to "/my/path/to"}]]
             [:hard-link {:type :link, :link-to "/my/path/to/file"}]]
          fs (jimfs/create-fs s)]
      (are [result path] (= result (nio/dir? (nio/path fs path)))
        true "/my"
        true "/my/path"
        true "/my/path/to"
        true "/my/path/empty-dir")
      (is (nio/file? (nio/path fs "/my/path/to/file")))
      (is (nio/file? (nio/path fs "/hard-link")))
      (is (nio/sym-link? (nio/path fs "/my/link")))
      (is (= (nio/read-sym-link (nio/path fs "/my/link")) (nio/path fs "/my/path/to")))
      (is (= (nio/read-all-lines (nio/path fs "/my/path/to/has-content"))
             ["line 1"
              "line 2"])))))

(deftest create-file
  (testing "creating a file that already exists returns nil"
    (testing "creating a file that already exists returns nil and doesn't throw exception"
      (let [fs (jimfs/create-fs
                 [[:dir1
                   [:dir2
                    [:file1]]]])]
        (is (nil? (nio/create-file (nio/path fs "/dir1/dir2/file1"))))))
    (testing "creating a file for a path that exists and isn't a file returns nil and doesn't throw exception"
      (let [fs (jimfs/create-fs
                 [[:dir1
                   [:dir2
                    [:file1]]]])]
        (is (nil? (nio/create-file (nio/path fs "/dir1/dir2"))))))))

(deftest create-dir
  (testing "creating a dir that already exists returns nil"
    (testing "creating a dir that already exists returns nil and doesn't throw exception"
      (let [fs (jimfs/create-fs
                 [[:dir1
                   [:dir2
                    [:file1]]]])]
        (is (nil? (nio/create-dir (nio/path fs "/dir1/dir2")))))))
  (testing "creating a dir for a path that exists and isn't a dir returns nil and doesn't throw exception"
    (let [fs (jimfs/create-fs
               [[:dir1
                 [:dir2
                  [:file1]]]])]
      (is (nil? (nio/create-dir (nio/path fs "/dir1/dir2/file1")))))))
