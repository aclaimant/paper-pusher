(defproject com.aclaimant/paper-pusher "1.0.2-SNAPSHOT"
  :description "Fill fillable PDFs."
  :url "https://github.com/aclaimant/paper-pusher"
  :license {:name "AGPL"
            :url "http://www.gnu.org/licenses/agpl-3.0.html"}
  :dependencies [[com.itextpdf/itextpdf "5.5.9"]
                 [com.outpace/config "0.9.0"]
                 [compojure "1.5.0"]
                 [fogus/ring-edn "0.3.0"]
                 [org.clojure/clojure "1.8.0"]
                 [ring "1.2.1"]]
  :plugins [[lein-ring "0.9.7"]]

  :ring {:handler aclaimant.paper-pusher.service/app
         :init aclaimant.paper-pusher.service/main
         :nrepl {:start? true :port 7005 :host "0.0.0.0"}}

  :uberjar-name "paper-pusher-standalone.jar"
  :min-lein-version "2.0.0"
  :profiles {:dev {:jvm-opts ["-Dconfig.edn=config.edn"]}
             :production {:jvm-opts ["-Dconfig.edn=config.edn"]}
             :uberjar {:aot :all}})
