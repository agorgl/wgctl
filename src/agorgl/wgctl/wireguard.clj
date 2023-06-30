(ns agorgl.wgctl.wireguard
  (:require [clojure.string :as str]
            [clojure.java.shell :refer [sh]]
            [agorgl.wgctl.remote :refer [remote-command]]))

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

(defn coll-sep [k]
  (cond
    (#{:allowed-ips} k) ", "
    (#{:pre-up :post-up :pre-down :post-down} k) "; "))

(defn prop-value [k v]
  (if (sequential? v)
    (str/join (coll-sep k) v)
    v))

(defn ini-section [title props]
  (let [title-line (format "[%s]" title)
        prop-lines (map (fn [[k v]] (format "%s = %s" k v)) props)]
    (->> (conj prop-lines title-line)
         (str/join "\n"))))

(defn interface-section [props]
  (->> (select-keys props (keys interface-keys))
       (map (fn [[k v]] [(k interface-keys) (prop-value k v)]))
       (into {})
       (ini-section "Interface")))

(defn peer-section [props]
  (->> (select-keys props (keys peer-keys))
       (map (fn [[k v]] [(k peer-keys) (prop-value k v)]))
       (into {})
       (ini-section "Peer")
       (str (format "# %s\n" (:name props)))))

(def file-perms "600")

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

(defn execute [cmd host]
  (if host
    (remote-command host (str/join " " cmd) nil)
    (apply sh cmd)))

(defn wg-quick [args host]
  (let [cmd (concat ["wg-quick"] args)
        {:keys [exit out err]} (execute cmd host)]
    (if (zero? exit)
      (do
        (when (seq err) (println err))
        (when (seq out) (println out)))
      (let [msg (format "Command '%s' failed with:\n%s" (str/join " " cmd) err)]
        (throw (ex-info msg {}))))))

(defn up [interface host]
  (wg-quick ["up" interface] host))

(defn down [interface host]
  (wg-quick ["down" interface] host))

(defn reload [interface host]
  (when (zero? (:exit (execute ["ip" "link" "show" interface] host)))
    (down interface host))
  (up interface host))
