(defproject
  nio2 "0.2.3"
  :description "A clojure wrapper around java.nio.file package"
  :url "http://github.com/potetm/nio2"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :profiles {:test {:dependencies [[com.google.jimfs/Jimfs "1.0"]]}
             :repl [:test]})
