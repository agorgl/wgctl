(ns build
  (:refer-clojure :exclude [test])
  (:require [clojure.java.io :as io]
            [clojure.tools.build.api :as b]))

(def lib 'net.clojars.agorgl/wgctl)
(def version "0.1.0-SNAPSHOT")
(def main 'agorgl.wgctl.core)
(def class-dir "target/classes")

(defn test "Run all the tests." [opts]
  (let [basis    (b/create-basis {:aliases [:test]})
        cmds     (b/java-command
                  {:basis     basis
                   :main      'clojure.main
                   :main-args ["-m" "cognitect.test-runner"]})
        {:keys [exit]} (b/process cmds)]
    (when-not (zero? exit) (throw (ex-info "Tests failed" {}))))
  opts)

(defn- uber-opts [opts]
  (assoc opts
         :lib lib :main main
         :uber-file (format "target/%s-%s.jar" lib version)
         :basis (b/create-basis {})
         :class-dir class-dir
         :src-dirs ["src"]
         :ns-compile [main]))

(defn uber "Build the uberjar." [opts]
  (b/delete {:path "target"})
  (let [opts (uber-opts opts)]
    (println "Copying source...")
    (b/copy-dir {:src-dirs ["resources" "src"] :target-dir class-dir})
    (println (str "Compiling " main "..."))
    (b/compile-clj opts)
    (println "Building JAR...")
    (b/uber opts))
  opts)

(defn ci "Run the CI pipeline of tests (and build the uberjar)." [opts]
  (test opts)
  (uber opts))

(defn native "Build the native binary." [opts]
  (uber opts)
  (println "Building native binary...")
  (if-let [graal-home (System/getenv "GRAALVM_HOME")]
    (let [jar (:uber-file (uber-opts opts))
          binary (format "target/%s-%s" lib version)
          command [(str (io/file graal-home "bin" "native-image"))
                   "-jar" jar
                   binary
                   "-H:+ReportExceptionStackTraces"
                   "-J-Dclojure.compiler.direct-linking=true"
                   "-J-Dclojure.spec.skip-macros=true"
                   "--no-fallback"
                   "--static"
                   "--verbose"]]
      (b/process {:command-args command}))
    (throw (ex-info "Environment variable GRAALVM_HOME is not set" {}))))
