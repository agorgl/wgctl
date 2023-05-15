(ns agorgl.wgctl.command
  (:require [clojure.string :as str]
            [agorgl.wgctl.domain :as d]
            [agorgl.wgctl.network :as net]
            [agorgl.wgctl.wireguard :as wg]
            [agorgl.wgctl.repository :as r]))

(defn address-cidr [address]
  (str address "/32"))

(defn peer->interface-entry [peer]
  (-> {:private-key (:private-key peer)
       :address (address-cidr (:address peer))}))

(defn peer->peer-entry [peer]
  (-> {:public-key (:public-key peer)
       :allowed-ips (address-cidr (:address peer))}))

(defn network-config [network]
  (let [interface-entry (peer->interface-entry (first (:peers network)))
        peer-entries (map peer->peer-entry (rest (:peers network)))]
    (wg/config-file (conj peer-entries interface-entry))))

(defn load-network [name]
  (if (r/network-exists name)
    (r/network-load name)
    (let [msg (format "Network with name '%s' does not exist" name)]
      (throw (ex-info msg {})))))

(defn save-network [network]
  (let [config (network-config network)]
    (r/network-save network)
    (r/config-save (:name network) config)))

(defn network-create [name addresses options]
  (if (not (r/network-exists name))
    (let [[private-key public-key] (wg/keypair)
          self-address (net/next-ip addresses)
          self-peer (d/make-self-peer private-key public-key self-address)
          network (d/make-network name addresses self-peer)]
      (save-network network))
    (let [msg (format "Network with name '%s' already exists" name)]
      (throw (ex-info msg {})))))

(defn network-list [options]
  (let [network-names (r/network-list)]
    (println (str/join "\n" network-names))))

(defn network-remove [name options]
  (when (r/network-exists name)
    (r/config-delete name)
    (r/network-delete name)))

(defn next-network-address [network]
  (let [addresses (:addresses network)
        occupied (map :address (:peers network))]
    (net/next-available-ip addresses occupied)))

(defn peer-add [peer-name public-key options]
  (let [network (load-network (:network options))
        address (next-network-address network)
        peer (d/make-peer peer-name public-key address)]
    (-> network
        (d/add-peer peer)
        save-network)))

(defn peer-set [peer-name property value options]
  (let [network (load-network (:network options))]
    (-> network
        (d/set-peer peer-name property value)
        save-network)))

(defn peer-list [options]
  (let [network (load-network (:network options))
        peer-names (d/list-peers network)]
    (println (str/join "\n" peer-names))))

(defn peer-remove [peer-name options]
  (let [network (load-network (:network options))]
    (-> network
        (d/remove-peer peer-name)
        save-network)))
