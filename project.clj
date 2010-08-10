(defproject clj-mackay "1.0.0-SNAPSHOT"
  :description "Clojure code for some of the stuff in the book 'Information Theory, Inference, and Learning Algorithms' by David MacKay."
  :java-fork "true"
  :dependencies [[org.clojure/clojure "1.2.0-beta1"]
                 [org.clojure/clojure-contrib "1.2.0-beta1"]
                 [incanter/incanter-core "1.2.3-SNAPSHOT"]]
  :dev-dependencies [[swank-clojure "1.2.1"]]
  :aot [clj-mackay.hamming
        clj-mackay.repetition]
  :warn-on-reflection true
  :jvm-opts ["-Xmx128M"])
