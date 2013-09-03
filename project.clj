(defproject com.twitter/thrift-clj "0.1.0-SNAPSHOT"
  :description "Clojure parser for the Thrift IDL."
  :url "https://github.com/twitter/thrift-clj"
  :license {:name "Apache 2"
            :url "http://www.apache.org/licenses/LICENSE-2.0.html"
            :distribution :repo
            :comments "A business-friendly OSS license."}
  :deploy-repositories [["releases" "https://oss.sonatype.org/service/local/staging/deploy/maven2"]
                        ["snapshots" "https://oss.sonatype.org/content/repositories/snapshots"]]
  :min-lein-version "2.0.0"
  :warn-on-reflection true
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/algo.monads "0.1.0"]
                 [org.apache.thrift/libthrift "0.8.0"]]
  :plugins [[lein-midje "2.0.0"]]
  :profiles {:dev {:dependencies [[midje "1.4.0"]]}}
  :pom-addition [:developers
                 [:developer
                  [:id "oscar"]
                  [:name "Oscar Boykin"]
                  [:url "http://twitter.com/posco"]]
                 [:developer
                  [:id "sritchie"]
                  [:name "Sam Ritchie"]
                  [:url "http://twitter.com/sritchie"]]])
