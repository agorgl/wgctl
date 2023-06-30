(ns agorgl.wgctl.remote
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data.json :as json])
  (:import [com.pty4j PtyProcessBuilder]))

(def remote-shell-proc (atom nil))

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

(defn proc-streams [proc]
  {:in (.getOutputStream proc)
   :out (.getInputStream proc)
   :err (.getErrorStream proc)})

(defn remote-connect [host]
  (if-let [remote-connect (System/getenv "WIREGUARD_REMOTE_CONNECT")]
    (-> remote-connect
        (str/replace #"\{\}" host)
        (str/split #" "))
    ["ssh" "-t" host]))

(defn remote-shell [host]
  (when (nil? @remote-shell-proc)
    (enter-raw-mode)
    (let [proc
          (future
            (let [cmd (conj (remote-connect host)
                            (slurp (io/resource "remote-exec")))
                  proc (-> (PtyProcessBuilder. (into-array cmd))
                           (.setConsole false)
                           (.start))]
              (.addShutdownHook (Runtime/getRuntime) (Thread. (fn [] (.destroy proc))))
              (reset! remote-shell-proc proc)))
          streams (proc-streams @proc)
          fout
          (future
            (let [out (:out streams)]
              (loop []
                (let [buf (byte-array 1024)
                      nb (.read out buf)]
                  (when (not= nb -1)
                    (if (= (first buf) 0)
                      :ready
                      (do
                        (.write *out* (String. buf) 0 nb)
                        (.flush *out*)
                        (recur))))))))
          ferr
          (future
            (let [err (:err streams)]
              (loop []
                (let [buf (byte-array 1024)
                      nb (.read err buf)]
                  (when (not= nb -1)
                    (.write *err* (String. buf) 0 nb)
                    (.flush *err*)
                    (recur))))))
          fin
          (future
            (let [in (:in streams)]
              (loop []
                (let [ch (.read *in*)]
                  (when (not= ch -1)
                    (.write in ch)
                    (.flush in)
                    (recur))))))]
      @fout ; Wait for ready
      (leave-raw-mode)
      (if (.isAlive @proc)
        (run! future-cancel [ferr fin])
        (do
          @ferr
          (let [msg "Could not start remote executor"]
            (throw (ex-info msg {})))))))
  (proc-streams @remote-shell-proc))

(defn remote-command [host cmd in]
  (let [payload (str (json/write-str {:cmd cmd :in in}) "\n")
        streams (remote-shell host)
        in (io/writer (:in streams))
        out (io/reader (:out streams))
        err (io/reader (:err streams))]
    (.write in payload)
    (.flush in)
    (.readLine out) ; skip echo output
    (when (.ready err)
      (let [msg (str "Error while executing remote command:\n" (.readLine err))]
        (throw (ex-info msg {}))))
    (let [line (.readLine out)
          result (json/read-str line :key-fn keyword)]
      (-> result
          (update-in [:exit] (fn [n] (Integer/parseInt n)))
          (update-in [:err] str/trim-newline)))))
