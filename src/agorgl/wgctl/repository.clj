(ns agorgl.wgctl.repository
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [agorgl.wgctl.wireguard :as wg]))

(defn network-list []
  (->> wg/config-dir
       io/file
       file-seq
       (filter #(.isFile ^java.io.File %))
       (mapv #(.getName ^java.io.File %))
       (filter #(re-matches #".*\.json$" %))
       (mapv #(str/replace % #"\.json$" ""))))

(defn network-file [name]
  (str wg/config-dir "/" name ".json"))

(defn network-exists [name]
  (let [file (network-file name)]
    (.exists (io/file file))))

(defn network-load [name]
  (-> name
      network-file
      slurp
      (json/read-str :key-fn keyword)))

(defn network-save [network]
  (let [name (:name network)
        file (network-file name)
        data (json/write-str network :indent true :escape-slash false)]
    (io/make-parents file)
    (spit file data)))

(defn network-delete [name]
  (let [file (network-file name)]
    (io/delete-file file)))

(defn config-file [name]
  (str wg/config-dir "/" name ".conf"))

(defn config-save [name config]
  (let [file (config-file name)]
    (io/make-parents file)
    (spit file config)))

(defn config-delete [name]
  (let [file (config-file name)]
    (io/delete-file file)))
