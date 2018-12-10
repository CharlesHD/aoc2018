(def +version+ "0.0.1")

(set-env!
 :source-paths #{"src"}
 :resource-paths #{"resources"}
 :target-path "tmp"
 :dependencies '[[org.clojure/clojure "1.9.0"]
                 [com.taoensso/tufte "2.0.1"]
                 [com.rpl/specter "1.1.2"]
                 [net.cgrand/xforms "0.18.2"]])

(task-options!
 pom {:project 'chu.graph
      :version +version+
      :description "Advent of Code 2018 in Clojure*"
      :url "https://github.com/CharlesHD/aoc2018"
      :scm {:url "https://github.com/CharlesHD/aoc2018"}}
 jar {:manifest {"Foo" "bar"}})
