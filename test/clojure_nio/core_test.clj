(ns clojure-nio.core-test
  (:require [clojure.test :refer :all]
            [clojure-nio.core :as nio]
            [clojure-nio.jimfs :as jimfs]))

(defn existing-file? [path]
  (and (nio/exists? path)
       (nio/file? path)))

(defn existing-dir? [path]
  (and (nio/exists? path)
       (nio/dir? path)))

(defn existing-sym-link-to? [link link-to]
  (and (nio/exists? link)
       (nio/sym-link? link)
       (= (nio/read-sym-link link) link-to)))

;; this is a better syntax I think

(deftest create-fs-test

  (testing "a single file"
    (let [s [[:foo]]
          fs (jimfs/create-fs s)]
      (is (existing-file? (nio/path fs "/foo")))))

  (testing "two files"
    (let [s [[:foo]
             [:bar]]
          fs (jimfs/create-fs s)]
      (are [result arg] (= result (existing-file? (nio/path fs arg)))
        true "/foo"
        true "/bar")))

  (testing "the negative case"
    (let [s [[:foo]]
          fs (jimfs/create-fs s)]
      (is (not (nio/exists? (nio/path fs "/not-exists"))))))

  (testing "nesting"
    (let [my-fs-struct [[:foo
                         [:bar
                          [:baz]]]]
          fs (jimfs/create-fs my-fs-struct)]
      (is (existing-file? (nio/path fs "/foo/bar/baz")))))

  (testing "creating an empty dir"
    (let [s [[:foo {:type :dir}]]
          fs (jimfs/create-fs s)]
      (is (nio/dir? (nio/path fs "/foo")))))

  (testing "creating a sym link"
    (let [s [[:foo]
             [:linky {:type :sym-link
                      :link-to "/foo"}]]
          fs (jimfs/create-fs s)]
      (is (existing-sym-link-to? (nio/path fs "/linky")
                                 (nio/path fs "/foo")))))

  (testing "creating a sym link without a :link-to throws an AssertionError"
    (let [s [[:foo]
             [:linky {:type :sym-link}]]]
      (is (thrown? AssertionError (jimfs/create-fs s)))))

  (testing "creating a hard link; notice it must be ordered after its :link-to"
    (let [s [[:foo]
             [:hardlink {:type :link
                         :link-to "/foo"}]]
          fs (jimfs/create-fs s)]
      (is (existing-file? (nio/path fs "/hardlink")))))

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
      (are [result path] (= result (existing-dir? (nio/path fs path)))
        true "/my"
        true "/my/path"
        true "/my/path/to"
        true "/my/path/empty-dir")
      (is (existing-file? (nio/path fs "/my/path/to/file")))
      (is (existing-file? (nio/path fs "/hard-link")))
      (is (existing-sym-link-to? (nio/path fs "/my/link") (nio/path fs "/my/path/to")))
      (is (= (nio/read-all-lines (nio/path fs "/my/path/to/has-content"))
             ["line 1"
              "line 2"])))))

#_ (deftest list-dir
  (testing "it works"
    (let [fs (jimfs/create-fs [{:name "/dir"
                                :children lkj}])])))

#_(deftest create-tmp-dir
  (testing "it works"
    (let [fs (jimfs/create-fs [])])))
