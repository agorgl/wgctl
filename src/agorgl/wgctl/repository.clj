(ns agorgl.wgctl.repository
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [agorgl.wgctl.wireguard :as wg]
            [agorgl.wgctl.remote :refer [remote-command]]))

(defn local-list-files [dir]
  (->> dir
       io/file
       file-seq
       (filter #(.isFile ^java.io.File %))
       (mapv #(.getName ^java.io.File %))))

(defn local-file-exists? [file]
  (.exists (io/file file)))

(defn local-read-file [file]
  (slurp file))

(defn local-write-file [file data]
  (io/make-parents file)
  (spit file data))

(defn local-delete-file [file]
  (io/delete-file file))

(defn remote-list-files [host dir]
  (let [cmd (str "find " dir " -type f -exec basename {} \\;")
        {:keys [exit out err]} (remote-command host cmd nil)]
    (if (zero? exit)
      (str/split out #"\n")
      (let [msg (format "Remote command failed with: %s" err)]
        (throw (ex-info msg {}))))))

(defn remote-file-exists? [host file]
  (let [cmd (str "test -f " file)
        {:keys [exit err]} (remote-command host cmd nil)]
    (if (str/blank? err)
      (zero? exit)
      (let [msg (format "Remote command failed with: %s" err)]
        (throw (ex-info msg {}))))))

(defn remote-read-file [host file]
  (let [cmd (str "cat " file)
        {:keys [exit out err]} (remote-command host cmd nil)]
    (if (zero? exit)
      out
      (let [msg (format "Remote command failed with: %s" err)]
        (throw (ex-info msg {}))))))

(defn remote-write-file [host file data]
  (let [cmd (format "mkdir -p $(dirname %s); cat > %s" file file)
        {:keys [exit out err]} (remote-command host cmd data)]
    (if (zero? exit)
      out
      (let [msg (format "Remote command failed with: %s" err)]
        (throw (ex-info msg {}))))))

(defn remote-delete-file [host file]
  (let [cmd (str "rm " file)
        {:keys [err]} (remote-command host cmd nil)]
    (when-not (str/blank? err)
      (let [msg (format "Remote command failed with: %s" err)]
        (throw (ex-info msg {}))))))

(defn list-files [host dir]
  (if (nil? host)
    (local-list-files dir)
    (remote-list-files host dir)))

(defn file-exists? [host file]
  (if (nil? host)
    (local-file-exists? file)
    (remote-file-exists? host file)))

(defn read-file [host file]
  (if (nil? host)
    (local-read-file file)
    (remote-read-file host file)))

(defn write-file [host file data]
  (if (nil? host)
    (local-write-file file data)
    (remote-write-file host file data)))

(defn delete-file [host file]
  (if (nil? host)
    (local-delete-file file)
    (remote-delete-file host file)))

(defn network-file [name]
  (str (wg/config-dir) "/" name ".json"))

(defn config-file [name]
  (str (wg/config-dir) "/" name ".conf"))

(defn network-list [host]
  (->> (wg/config-dir)
       (list-files host)
       (filter #(re-matches #".*\.json$" %))
       (mapv #(str/replace % #"\.json$" ""))))

(defn network-exists [host name]
  (let [file (network-file name)]
    (file-exists? host file)))

(defn network-load [host name]
  (let [file (network-file name)]
    (-> (read-file host file)
        (json/read-str :key-fn keyword))))

(defn network-save [host network]
  (let [name (:name network)
        file (network-file name)
        data (json/write-str network :indent true :escape-slash false)]
    (write-file host file data)))

(defn network-delete [host name]
  (let [file (network-file name)]
    (delete-file host file)))

(defn config-save [host name config]
  (let [file (config-file name)]
    (write-file host file config)))

(defn config-delete [host name]
  (let [file (config-file name)]
    (delete-file host file)))
