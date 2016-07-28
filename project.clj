(defproject financial-forecasting "0.1.0-SNAPSHOT"
  :description "Generative model for Financial Forecasting in Anglican"
  :url ""
  :license {:name " "GNU General Public License Version 3; Other commercial licenses available."
            :url "http://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.priority-map "0.0.7"]
                 [anglican "0.7.0-SNAPSHOT"]
                 [net.mikera/core.matrix "0.33.2"]
                 [net.mikera/core.matrix.stats "0.5.0"]
                 [net.mikera/vectorz-clj "0.29.0"]]
  :plugins [[lein-gorilla "0.3.4"]]
  :jvm-opts ["-Xmx2g" "-Xms2g"] 
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
