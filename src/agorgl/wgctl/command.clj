(ns agorgl.wgctl.command
  (:require [clojure.string :as str]
            [agorgl.wgctl.domain :as d]
            [agorgl.wgctl.network :as net]
            [agorgl.wgctl.wireguard :as wg]
            [agorgl.wgctl.repository :as r]))

(defn address-cidr [address plen]
  (str address "/" (or plen 32)))

(defn peer->interface-entry [network peer]
  (-> {:private-key (:private-key peer)
       :address (address-cidr (:address peer)
                              (when (:hub peer)
                                (net/plength (:addresses network))))}
      (cond-> (some? (:endpoint peer))
        (assoc :listen-port (last (str/split (:endpoint peer) #":"))))
      (cond-> (:hub peer)
        (assoc :post-up "sysctl -w net.ipv4.conf.%i.forwarding=1"))))

(defn peer->peer-entry [network peer]
  (-> {:public-key (:public-key peer)
       :allowed-ips (if (:hub peer)
                      (:addresses network)
                      (address-cidr (:address peer) nil))}
      (cond-> (some? (:endpoint peer))
        (assoc :endpoint (:endpoint peer)))))

(defn network-config [network]
  (let [interface-entry (peer->interface-entry network (first (:peers network)))
        peer-entries (map #(peer->peer-entry network %) (rest (:peers network)))]
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

(defn pick-network [name]
  (let [name (or name (let [v (r/network-list)]
                        (when (= (count v) 1)
                          (first v))))]
    (if (some? name)
      name
      (let [msg (format "Network was not specified")]
        (throw (ex-info msg {}))))))

(defn peer-add [peer-name public-key options]
  (let [network (load-network (pick-network (:network options)))
        address (next-network-address network)
        peer (d/make-peer peer-name public-key address)]
    (-> network
        (d/add-peer peer)
        save-network)))

(defn peer-get [peer-name property options]
  (let [network (load-network (pick-network (:network options)))
        prop (d/get-peer-prop network peer-name property)]
    (when (some? prop)
      (println prop))))

(defn peer-set [peer-name property value options]
  (let [network (load-network (pick-network (:network options)))]
    (-> network
        (d/set-peer-prop peer-name property value)
        save-network)))

(defn peer-list [options]
  (let [network (load-network (pick-network (:network options)))
        peer-names (d/list-peers network)]
    (println (str/join "\n" peer-names))))

(defn peer-remove [peer-name options]
  (let [network (load-network (pick-network (:network options)))]
    (-> network
        (d/remove-peer peer-name)
        save-network)))
