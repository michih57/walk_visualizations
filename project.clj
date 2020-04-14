(defproject walk_visualizations "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0-RC5"]
                 [org.clojure/clojurescript "1.7.228" :scope "provided"]
                 [ring "1.4.0"]
                 [ring/ring-defaults "0.1.5"]
                 [bk/ring-gzip "0.1.1"]
                 [ring.middleware.logger "0.5.0"]
                 [compojure "1.4.0"]
                 [org.clojure/data.json "0.2.6"]
                 [http.async.client "1.1.0"]
                 [cljsjs/pixi "3.0.7-0"]
                 [cljs-ajax "0.5.3"]
                 [environ "1.0.2"]
                 [org.zeromq/jeromq "0.3.3"]
                 [org.zeromq/cljzmq "0.1.4" :exclusions [org.zeromq/jzmq]]
                 [hoplon/javelin "3.8.4"]
                 [prismatic/dommy "1.1.0"]]

  :plugins [[lein-cljsbuild "1.1.1"]
            [lein-environ "1.0.1"]]

  :min-lein-version "2.5.3"

  :source-paths ["src/clj" "src/cljs" "dev"]

  :test-paths ["test/clj"]

  :clean-targets ^{:protect false} [:target-path :compile-path "resources/public/js"]

  :uberjar-name "walk_visualizations.jar"

  ;; Use `lein run` if you just want to start a HTTP server, without figwheel
  :main walk-visualizations.server

  ;; nREPL by default starts in the :main namespace, we want to start in `user`
  ;; because that's where our development helper functions like (run) and
  ;; (browser-repl) live.
  :repl-options {:init-ns user
                 :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}

  :cljsbuild {:builds
              {:app
               {:source-paths ["src/cljs"]

                :figwheel true
                ;; Alternatively, you can configure a function to run every time figwheel reloads.
                ;; :figwheel {:on-jsload "walk-visualizations.core/on-figwheel-reload"}

                :compiler {:main walk-visualizations.core
                           :asset-path "js/compiled/out"
                           :output-to "resources/public/js/compiled/walk_visualizations.js"
                           :output-dir "resources/public/js/compiled/out"
                           :source-map-timestamp true}}}}

  ;; When running figwheel from nREPL, figwheel will read this configuration
  ;; stanza, but it will read it without passing through leiningen's profile
  ;; merging. So don't put a :figwheel section under the :dev profile, it will
  ;; not be picked up, instead configure figwheel here on the top level.

  :figwheel {;; :http-server-root "public"       ;; serve static assets from resources/public/
             ;; :server-port 3449                ;; default
             ;; :server-ip "127.0.0.1"           ;; default
             :css-dirs ["resources/public/css"]  ;; watch and update CSS

             ;; Instead of booting a separate server on its own port, we embed
             ;; the server ring handler inside figwheel's http-kit server, so
             ;; assets and API endpoints can all be accessed on the same host
             ;; and port. If you prefer a separate server process then take this
             ;; out and start the server with `lein run`.
             :ring-handler user/http-handler

             ;; Start an nREPL server into the running figwheel process. We
             ;; don't do this, instead we do the opposite, running figwheel from
             ;; an nREPL process, see
             ;; https://github.com/bhauman/lein-figwheel/wiki/Using-the-Figwheel-REPL-within-NRepl
             ;; :nrepl-port 7888

             ;; To be able to open files in your editor from the heads up display
             ;; you will need to put a script on your path.
             ;; that script will have to take a file path and a line number
             ;; ie. in  ~/bin/myfile-opener
             ;; #! /bin/sh
             ;; emacsclient -n +$2 $1
             ;;
             ;; :open-file-command "myfile-opener"

             :server-logfile "log/figwheel.log"}

  :doo {:build "test"}

  :profiles {:dev
             {:dependencies [[figwheel "0.5.0-6"]
                             [figwheel-sidecar "0.5.0-6"]
                             [com.cemerick/piggieback "0.2.1"]
                             [org.clojure/tools.nrepl "0.2.12"]]

              :plugins [[lein-figwheel "0.5.0-6"]
                        [lein-doo "0.1.6"]]

              :cljsbuild {:builds
                          {:test
                           {:source-paths ["src/cljs" "test/cljs"]
                            :compiler
                            {:output-to "resources/public/js/compiled/testable.js"
                             :main walk-visualizations.test-runner
                             :optimizations :none}}}}}

             :uberjar
             {:source-paths ^:replace ["src/clj"]
              :hooks [leiningen.cljsbuild]
              :omit-source true
              :aot :all
              :cljsbuild {:builds
                          {:app
                           {:source-paths ^:replace ["src/cljs"]
                            :compiler
                            {:optimizations :advanced
                             :pretty-print false}}}}}})
