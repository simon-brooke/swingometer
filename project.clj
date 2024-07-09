(defproject rsvggraph "0.1.0-SNAPSHOT"
  :dependencies [[clojure2d "1.4.5"] ;; (mainly) for colours
                 [generateme/fastmath "2.4.0"]
                 [hiccup "2.0.0-RC3"]
                 [javax.xml.bind/jaxb-api "2.4.0-b180830.0359"]
                 [org.clojure/clojure "1.11.3"]
                 [org.clojure/data.xml "0.0.8"]
                ;;  [org.clojure/clojurescript "1.9.229"]
                ;;  [org.omcljs/om "1.0.0-beta1"]
                ;;  [reagent "0.6.0"]
                ;;  [re-frame "0.9.4"]
                ;;  [re-com "2.0.0"]
                 ]

  ;; :plugins [[lein-cljsbuild "1.1.4"]]

  :min-lein-version "2.5.3"

  :source-paths ["src/clj"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]

  :figwheel {:css-dirs ["resources/public/css"]}

  ;; :profiles
  ;; {:dev
  ;;  {:dependencies [[binaryage/devtools "0.8.2"]]

  ;;   :plugins      [[lein-figwheel "0.5.9"]]
  ;;   }}

  ;; :cljsbuild
  ;; {:builds
  ;;  [{:id           "dev"
  ;;    :source-paths ["src/cljs"]
  ;;    :figwheel     {:on-jsload "rsvggraph.core/mount-root"}
  ;;    :compiler     {:main                 rsvggraph.core
  ;;                   :output-to            "resources/public/js/compiled/app.js"
  ;;                   :output-dir           "resources/public/js/compiled/out"
  ;;                   :asset-path           "js/compiled/out"
  ;;                   :source-map-timestamp true
  ;;                   :preloads             [devtools.preload]
  ;;                   :external-config      {:devtools/config {:features-to-install :all}}
  ;;                   }}

  ;;   {:id           "min"
  ;;    :source-paths ["src/cljs"]
  ;;    :compiler     {:main            rsvggraph.core
  ;;                   :output-to       "resources/public/js/compiled/app.js"
  ;;                   :optimizations   :advanced
  ;;                   :closure-defines {goog.DEBUG false}
  ;;                   :pretty-print    false}}]})
)
