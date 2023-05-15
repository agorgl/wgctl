(ns agorgl.wgctl.wireguard
  (:require [clojure.string :as str]
            [clojure.java.shell :refer [sh]]))

(def interface-keys
  (array-map
   :private-key "PrivateKey"
   :listen-port "ListenPort"
   :address "Address"
   :dns "DNS"
   :mtu "MTU"
   :table "Table"
   :pre-up "PreUp"
   :post-up "PostUp"
   :pre-down "PreDown"
   :post-down "PostDown"))

(def peer-keys
  (array-map
   :public-key "PublicKey"
   :shared-key "PresharedKey"
   :allowed-ips "AllowedIPs"
   :endpoint "Endpoint"
   :persistent-keepalive "PersistentKeepalive"))

(defn ini-section [title props]
  (let [title-line (format "[%s]" title)
        prop-lines (map (fn [[k v]] (format "%s = %s" k v)) props)]
    (->> (conj prop-lines title-line)
         (str/join "\n"))))

(defn interface-section [props]
  (->> (select-keys props (keys interface-keys))
       (map (fn [[k v]] [(k interface-keys) v]))
       (into {})
       (ini-section "Interface")))

(defn peer-section [props]
  (->> (select-keys props (keys peer-keys))
       (map (fn [[k v]] [(k peer-keys) v]))
       (into {})
       (ini-section "Peer")))

(defn config-dir []
  (or (System/getenv "WIREGUARD_DIR") "/etc/wireguard"))

(defn config-file [entries]
  (let [interface-section (interface-section (first entries))
        peer-sections (map peer-section (rest entries))]
    (str/join "\n\n" (conj peer-sections interface-section))))

(defn keypair []
  (let [private-key (str/trim-newline (:out (sh "wg" "genkey")))
        public-key (str/trim-newline (:out (sh "wg" "pubkey" :in private-key)))]
    [private-key public-key]))
