(ns agorgl.wgctl.remote
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data.json :as json])
  (:import [com.pty4j PtyProcessBuilder]))

(def remote-shell-streams (atom nil))

(defn enter-raw-mode []
  (let [cmd ["stty" "-icanon" "-echo"]]
    (-> (ProcessBuilder. ^java.util.List cmd)
        (.inheritIO)
        (.start)
        (.waitFor))))

(defn leave-raw-mode []
  (let [cmd ["stty" "icanon" "echo"]]
    (-> (ProcessBuilder. ^java.util.List cmd)
        (.inheritIO)
        (.start)
        (.waitFor))))

(defn remote-shell [host]
  (when (nil? @remote-shell-streams)
    (enter-raw-mode)
    (let [streams
          (future
            (let [cmd ["ssh" "-q" "-t" host
                       (format "sudo bash -c '%s'"
                               (str "printf \"\\0\" ; "
                                    "while read -r msg; do "
                                    "cmd=$(jq -r '\\''.cmd'\\'' <<< \"$msg\"); in=$(jq -r '\\''.in'\\'' <<< \"$msg\"); "
                                    "{ IFS= read -rd '\\'''\\'' err; IFS= read -rd '\\'''\\'' out; IFS= read -rd '\\'''\\'' exit; } < <({ out=$(bash -c \"$cmd\" <<< \"$in\"); } 2>&1; printf '\\''\\0%s'\\'' \"$out\" \"$?\"); "
                                    "jq -n -c -M --arg exit \"$exit\" --arg out \"$out\" --arg err \"$err\" '\\''{\"exit\": $exit, \"out\": $out, \"err\": $err}'\\''; "
                                    "done"))]
                  proc (-> (PtyProcessBuilder. (into-array cmd))
                           (.setConsole true)
                           (.start))
                  streams {:in (.getOutputStream proc)
                           :out (.getInputStream proc)
                           :err (.getErrorStream proc)}]
              (.addShutdownHook (Runtime/getRuntime) (Thread. (fn [] (.destroy proc))))
              (reset! remote-shell-streams streams)))
          fout
          (future
            (let [out (:out @streams)]
              (loop []
                (let [ch (.read out)]
                  (when (not= ch -1)
                    (if (= ch 0)
                      :ready
                      (do
                        (.write *out* ch)
                        (.flush *out*)
                        (recur))))))))
          ferr
          (future
            (let [err (:err @streams)]
              (loop []
                (let [ch (.read err)]
                  (when (not= ch -1)
                    (.write *err* ch)
                    (.flush *err*)
                    (recur))))))
          fin
          (future
            (let [in (:in @streams)]
              (loop []
                (let [ch (.read *in*)]
                  (when (not= ch -1)
                    (.write in ch)
                    (.flush in)
                    (recur))))))]
      @fout ; Wait for ready
      (leave-raw-mode)
      (run! future-cancel [ferr fin])))
  @remote-shell-streams)

(defn remote-command [host cmd in]
  (let [payload (str (json/write-str {:cmd cmd :in in}) "\n")
        streams (remote-shell host)
        in (io/writer (:in streams))
        out (io/reader (:out streams))
        err (io/reader (:err streams))]
    (.write in payload)
    (.flush in)
    (when (.ready err)
      (let [msg (str "Error while executing remote command:\n" (.readLine err))]
        (throw (ex-info msg {}))))
    (let [line (.readLine out)
          result (json/read-str line :key-fn keyword)]
      (-> result
          (update-in [:exit] (fn [n] (Integer/parseInt n)))
          (update-in [:err] str/trim-newline)))))
