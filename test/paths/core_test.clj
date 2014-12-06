(ns paths.core-test
  (:import (java.nio.file FileSystems Path LinkOption)
           (java.io File)
           (java.net URI))
  (:require [clojure.test :refer :all]
            [paths.core :as paths]
            [paths.jimfs :as jimfs]))

(deftest default-fs
  (testing "it returns the default fs"
    (is (= (paths/default-fs) (FileSystems/getDefault)))))

(deftest path
  (let [fs (jimfs/create-fs [])]
    (testing "it returns a path"
      (is (instance? Path (paths/path fs "path"))))
    (testing "the varargs form"
      (is (= (paths/path fs "path/to/file")
             (paths/path fs "path" "to" "file"))))))

(deftest absolute
  (testing
    "It makes an absolute path.

     NOTE: The way absolute path resolution works is by
     resolving the path to some fs-dependent default dir. In
     JimFS's case, that's /work.

     http://docs.oracle.com/javase/7/docs/api/java/nio/file/Path.html#toAbsolutePath%28%29"
    (let [fs (jimfs/create-fs [])]
      (is (= (paths/absolute (paths/path fs "file"))
             (paths/path fs "/work/file"))))))

(deftest file
  (testing
    "It returns a file. NOTE: JimFS can't do this so we use the default fs"
    (let [tmp-dir (paths/create-tmp-dir-on-default-fs "nio-test")]
      (is (= (str tmp-dir)
             (.getAbsolutePath ^File (paths/file tmp-dir)))))))

(deftest filename
  (testing "It returns the filename"
    (let [fs (jimfs/create-fs [])]
      (is (= (paths/filename (paths/path fs "path/to/file"))
             (paths/path fs "file"))))))

(deftest get-fs
  (testing "It returns the fs"
    (let [fs (jimfs/create-fs [])]
      (is (= (paths/get-fs (paths/path fs "path/to/file"))
             fs)))))

(deftest join
  (testing "It joins two paths"
    (let [fs (jimfs/create-fs [])
          parent (paths/path fs "/parent")
          child (paths/path fs "child")]
      (is (= (paths/join parent child)
             (paths/path fs "/parent/child"))))))

(deftest normalize
  (testing "It normalizes"
    (let [fs (jimfs/create-fs [])]
      (is (= (paths/path fs "path/to/file")
             (paths/normalize (paths/path fs "path/../path/./to/file")))))))

(deftest parent
  (testing "It parents"
    (let [fs (jimfs/create-fs [])]
      (is (= (paths/parent (paths/path fs "parent/child"))
             (paths/path fs "parent"))))))

(deftest relativize
  (testing "it makes a relative path"
    (let [fs (jimfs/create-fs [])]
      (is (= (paths/path fs "to/file")
             (paths/relativize (paths/path fs "/my/path")
                             (paths/path fs "/my/path/to/file")))))))

(deftest root
  (testing "it roots"
    (let [fs (jimfs/create-fs [])]
      (is (= (paths/root (paths/path fs "/path/to/file"))
             (paths/path fs "/")))))
  (testing "it returns nil for relative paths"
    (let [fs (jimfs/create-fs [])]
      (is (nil? (paths/root (paths/path fs "relative")))))))

(deftest split
  (testing "it returns a seq of path parts"
    (let [fs (jimfs/create-fs
               [[:dir1
                 [:dir2
                  [:file1]]]])]
      (is (= (paths/split (paths/path fs "/dir1/dir2/file1"))
             (map (partial paths/path fs)
                  ["dir1" "dir2" "file1"]))))))

(deftest uri
  (testing "it uris"
    (let [fs (jimfs/create-fs [])]
      (is (instance? URI (paths/uri (paths/path fs "/path/to/file"))))
      (is (.endsWith (str (paths/uri (paths/path fs "/path/to/file")))
                     "/path/to/file")))))

(deftest exists?
  (testing "it returns true if the file exists"
    (let [fs (jimfs/create-fs [[:i-exist]])]
      (is (paths/exists? (paths/path fs "/i-exist")))))
  (testing "it returns false if the file don't exist"
    (is (not (paths/exists? (paths/path (jimfs/create-fs []) "no-exist")))))
  (testing "multi-arity"
    (let [fs (jimfs/create-fs
               [[:link {:type :sym-link
                        :link-to "/target"}]])]
      (is (paths/exists?
            (paths/path fs "/link")
            LinkOption/NOFOLLOW_LINKS))
      (is (not (paths/exists? (paths/path fs "/link")))))))

(deftest dir-stream
  (testing "it works"
    (let [fs (jimfs/create-fs
               [[:dir1
                 [:file1]
                 [:dir2 {:type :dir}]]])]
      (with-open [ds (paths/dir-stream (paths/path fs "/dir1"))]
        (is (= (set ds)
               #{(paths/path fs "/dir1/file1")
                 (paths/path fs "/dir1/dir2")})))))
  (testing "it can be filtered"
    (let [fs (jimfs/create-fs
               [[:dir1
                 [:file1]
                 [:dir2 {:type :dir}]]])]
      (with-open [ds (paths/dir-stream (paths/path fs "/dir1"))]
        (is (= (set (filter (partial = (paths/path fs "/dir1/file1")) ds))
               #{(paths/path fs "/dir1/file1")})))))
  (testing "it can be mapped"
    (let [fs (jimfs/create-fs
               [[:dir1
                 [:file1]
                 [:dir2 {:type :dir}]]])]
      (with-open [ds (paths/dir-stream (paths/path fs "/dir1"))]
        (is (= (set (map (comp str paths/filename) ds))
               #{"file1" "dir2"})))))
  (testing "the glob"
    (let [fs (jimfs/create-fs
               [[:dir
                 [:matches]
                 [:dont-match]]])]
      (with-open [ds (paths/dir-stream (paths/path fs "/dir") "mat*")]
        (is (= [(paths/path fs "/dir/matches")]
               (into [] ds)))))))

(deftest last-modified
  (testing "it returns the last modified date for a file"
    (let [fs (jimfs/create-fs
               [[:file]])]
      (is (number? (paths/last-modified (paths/path fs "/file")))))))

(deftest create-fs-test
  (testing "a single file"
    (let [s [[:foo]]
          fs (jimfs/create-fs s)
          path (paths/path fs "/foo")]
      (is (paths/exists? path))
      (is (paths/file? path))))

  (testing "two files"
    (let [s [[:foo]
             [:bar]]
          fs (jimfs/create-fs s)
          foo (paths/path fs "/foo")
          bar (paths/path fs "/bar")]
      (is (paths/exists? foo))
      (is (paths/file? foo))
      (is (paths/exists? bar))
      (is (paths/file? bar))))

  (testing "the negative case"
    (let [s [[:foo]]
          fs (jimfs/create-fs s)]
      (is (not (paths/exists? (paths/path fs "/not-exists"))))))

  (testing "nesting"
    (let [my-fs-struct [[:foo
                         [:bar
                          [:baz]]]]
          fs (jimfs/create-fs my-fs-struct)
          path (paths/path fs "/foo/bar/baz")]
      (is (paths/exists? path))
      (is (paths/file? path))))

  (testing "creating an empty dir"
    (let [s [[:foo {:type :dir}]]
          fs (jimfs/create-fs s)]
      (is (paths/dir? (paths/path fs "/foo")))))

  (testing "creating a sym link"
    (let [s [[:foo]
             [:linky {:type :sym-link
                      :link-to "/foo"}]]
          fs (jimfs/create-fs s)
          link (paths/path fs "/linky")]
      (is (paths/exists? link))
      (is (paths/sym-link? link))
      (is (= (paths/read-sym-link link) (paths/path fs "/foo")))))

  (testing "creating a sym link without a :link-to throws an AssertionError"
    (let [s [[:foo]
             [:linky {:type :sym-link}]]]
      (is (thrown? AssertionError (jimfs/create-fs s)))))

  (testing "creating a hard link; notice it must be ordered after its :link-to"
    (let [s [[:foo]
             [:hardlink {:type :link
                         :link-to "/foo"}]]
          fs (jimfs/create-fs s)
          path (paths/path fs "/hardlink")]
      (is (paths/exists? path))
      (is (paths/file? path))))

  (testing "creating a sym link without a :link-to throws an AssertionError"
    (let [s [[:foo]
             [:hardlink {:type :link}]]]
      (is (thrown? AssertionError (jimfs/create-fs s)))))

  (testing "writing contents"
    (let [s [[:foo "hello, world!"]]
          fs (jimfs/create-fs s)]
      (is (= (paths/read-all-lines (paths/path fs "/foo"))
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
      (are [result path] (= result (paths/dir? (paths/path fs path)))
        true "/my"
        true "/my/path"
        true "/my/path/to"
        true "/my/path/empty-dir")
      (is (paths/file? (paths/path fs "/my/path/to/file")))
      (is (paths/file? (paths/path fs "/hard-link")))
      (is (paths/sym-link? (paths/path fs "/my/link")))
      (is (= (paths/read-sym-link (paths/path fs "/my/link")) (paths/path fs "/my/path/to")))
      (is (= (paths/read-all-lines (paths/path fs "/my/path/to/has-content"))
             ["line 1"
              "line 2"])))))

(deftest create-file
  (testing "creating a file that already exists returns nil"
    (testing "creating a file that already exists returns nil and doesn't throw exception"
      (let [fs (jimfs/create-fs
                 [[:dir1
                   [:dir2
                    [:file1]]]])]
        (is (nil? (paths/create-file (paths/path fs "/dir1/dir2/file1"))))))
    (testing "creating a file for a path that exists and isn't a file returns nil and doesn't throw exception"
      (let [fs (jimfs/create-fs
                 [[:dir1
                   [:dir2
                    [:file1]]]])]
        (is (nil? (paths/create-file (paths/path fs "/dir1/dir2"))))))))

(deftest create-dir
  (testing "creating a dir that already exists returns nil"
    (testing "creating a dir that already exists returns nil and doesn't throw exception"
      (let [fs (jimfs/create-fs
                 [[:dir1
                   [:dir2
                    [:file1]]]])]
        (is (nil? (paths/create-dir (paths/path fs "/dir1/dir2")))))))
  (testing "creating a dir for a path that exists and isn't a dir returns nil and doesn't throw exception"
    (let [fs (jimfs/create-fs
               [[:dir1
                 [:dir2
                  [:file1]]]])]
      (is (nil? (paths/create-dir (paths/path fs "/dir1/dir2/file1")))))))
