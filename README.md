# nio2
A lightweight wrapper around the `java.nio.file` package.

# Motivation
There are two primary reasons this library was created:
1. To allow clojure developers to take advantage of NIO2 features
2. To make FileSystem IO testable in clojure

[NIO2](http://www.oracle.com/technetwork/articles/javase/nio-139333.html) introduced the
ability to lazily interact with the file system (e.g. walking a directory structure or watching a file),
improved access to file attributes, and improved interop across platforms and file systems.

In addition, the introduction of NIO2 made it possible for developers to create their
own file system implementations (e.g. zip file systems, or in memory file systems).
This, of course, opens the door for improved testing around IO code.

Unfortunately, the improved API for Java developers makes it difficult for clojure
developers due to copious varargs methods.

Nio2 hopes to ease those pains.

# New Features
The only new feature in this library is in the `create-fs-tree!` function. This function
allows you to declaratively create a file system structure using a hiccup-like syntax.

Here's an example from Nio2's unit tests:
```clojure
(nio2/create-fs-tree!
  (nio2/default-fs)
  "/tmp"
  [[:my
    [:path
     [:to
      [:file] ;; <-- a file because it has no children
      [:has-content "line 1" "line 2"]]
     [:empty-dir {:type :dir}]]
    [:link {:type :sym-link, :link-to "/my/path/to"}]]
   [:hard-link {:type :link, :link-to "/my/path/to/file"}]])
```

Hopefully the syntax is clear enough that

# Testing
This library makes use of [Jimfs](https://github.com/google/jimfs) in its
[unit tests](https://github.com/potetm/nio2/blob/master/test/nio2/core_test.clj).

The combination of Nio2 and Jimfs made IO testing, dare I say, enjoyable.

## License

Copyright © 2014 Timothy Pote

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
