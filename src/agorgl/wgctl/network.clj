(ns agorgl.wgctl.network
  (:require [clojure.string :as str]))

(defn- str->int [s]
  (Integer/parseInt s))

(defn ip->int [ip]
  (let [[a b c d]
        (->> (str/split ip #"\.")
             (mapv str->int))]
    (bit-or
     (bit-shift-left (bit-and a 0xFF) 24)
     (bit-shift-left (bit-and b 0xFF) 16)
     (bit-shift-left (bit-and c 0xFF) 8)
     (bit-shift-left (bit-and d 0xFF) 0))))

(defn int->ip [x]
  (str/join "." [(bit-shift-right (bit-and x 0xFF000000) 24)
                 (bit-shift-right (bit-and x 0x00FF0000) 16)
                 (bit-shift-right (bit-and x 0x0000FF00) 8)
                 (bit-shift-right (bit-and x 0x000000FF) 0)]))

(defn address [cidr]
  (first (str/split cidr #"/")))

(defn plength [cidr]
  (-> cidr
      (str/split #"/")
      second
      str->int))

(defn netmask [len]
  (bit-shift-left -1 (- 32 len)))

(defn network [cidr]
  (let [address (address cidr)
        len (plength cidr)]
    (int->ip (bit-and (ip->int address) (netmask len)))))

(defn block [cidr]
  (str (network cidr) "/" (plength cidr)))

(defn host-range [cidr]
  (let [network (network cidr)
        len (plength cidr)
        mask (netmask len)
        host-min (inc (ip->int network))
        host-max (+ host-min (- (bit-not mask) 2))]
    (mapv int->ip [host-min host-max])))

(defn in-host-range? [cidr ip]
  (let [[host-min host-max] (mapv ip->int (host-range cidr))
        address (ip->int (address ip))]
    (<= host-min address host-max)))

(defn next-ip [cidr]
  (let [address (ip->int (address cidr))
        nip (int->ip (inc address))]
    (when (in-host-range? cidr nip)
      nip)))

(defn next-gap [coll]
  (when-not (empty? coll)
    (let [gaps (->> (sort coll)
                    (partition 2 1)
                    (filter #(< 1 (- (second %) (first %)))))]
      (inc (if (seq gaps)
             (-> gaps
                 first
                 first)
             (last coll))))))

(defn next-available-ip [cidr allocated-ips]
  (let [addresses (map (comp ip->int address) allocated-ips)
        network (ip->int (network cidr))
        gap (next-gap (conj addresses network))]
    (when (some? gap)
      (let [ip (int->ip gap)]
        (when (in-host-range? cidr ip)
          ip)))))
