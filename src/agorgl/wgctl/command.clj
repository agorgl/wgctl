(ns agorgl.wgctl.command
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [agorgl.wgctl.domain :as d]
            [agorgl.wgctl.network :as net]
            [agorgl.wgctl.wireguard :as wg]
            [agorgl.wgctl.repository :as r]
            [agorgl.wgctl.cli :as cli]))

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
  (-> {:name (:name peer)
       :public-key (:public-key peer)
       :allowed-ips (str/join ", "
                              (conj (some->> (:routes peer)
                                             (map :addresses))
                                    (if (:hub peer)
                                      (:addresses network)
                                      (address-cidr (:address peer) nil))))}
      (cond-> (some? (:endpoint peer))
        (assoc :endpoint (:endpoint peer)))))

(defn network-config [network]
  (let [interface-entry (peer->interface-entry network (first (:peers network)))
        peer-entries (map #(peer->peer-entry network %) (rest (:peers network)))]
    (wg/config-file (conj peer-entries interface-entry))))

(defn load-network [name remote]
  (if (r/network-exists remote name)
    (r/network-load remote name)
    (let [msg (format "Network with name '%s' does not exist" name)]
      (throw (ex-info msg {})))))

(defn save-network [network remote]
  (let [config (network-config network)]
    (r/network-save remote network)
    (r/config-save remote (:name network) config)))

(defn network-create [name addresses {:keys [remote]}]
  (if (not (r/network-exists remote name))
    (let [[private-key public-key] (wg/keypair)
          self-address (net/next-ip addresses)
          self-peer (d/make-self-peer private-key public-key self-address)
          network (d/make-network name addresses self-peer)]
      (save-network network remote))
    (let [msg (format "Network with name '%s' already exists" name)]
      (throw (ex-info msg {})))))

(defn network-list [{:keys [remote]}]
  (let [network-names (r/network-list remote)]
    (println (str/join "\n" network-names))))

(defn network-remove [name {:keys [remote]}]
  (when (r/network-exists remote name)
    (r/config-delete remote name)
    (r/network-delete remote name)))

(defn next-network-address [network]
  (let [addresses (:addresses network)
        occupied (map :address (:peers network))]
    (net/next-available-ip addresses occupied)))

(defn pick-network [name remote]
  (let [name (or name (let [v (r/network-list remote)]
                        (when (= (count v) 1)
                          (first v))))]
    (if (some? name)
      name
      (let [msg (format "Network was not specified")]
        (throw (ex-info msg {}))))))

(defn peer-add [peer-name public-key {:keys [remote network]}]
  (let [network (load-network (pick-network network remote) remote)
        address (next-network-address network)
        peer (d/make-peer peer-name public-key address)]
    (-> network
        (d/add-peer peer)
        (save-network remote))))

(defn peer-properties []
  (let [properties (map name d/peer-properties)]
    (println (str/join "\n" properties))))

(defn peer-get [peer-name property {:keys [remote network]}]
  (let [network (load-network (pick-network network remote) remote)
        prop (d/get-peer-prop network peer-name property)]
    (when (some? prop)
      (println prop))))

(defn peer-set [peer-name property value {:keys [remote network]}]
  (let [network (load-network (pick-network network remote) remote)]
    (-> network
        (d/set-peer-prop peer-name property value)
        (save-network remote))))

(defn peer-list [{:keys [remote network]}]
  (let [network (load-network (pick-network network remote) remote)
        peer-names (d/list-peers network)]
    (println (str/join "\n" peer-names))))

(defn peer-remove [peer-name {:keys [remote network]}]
  (let [network (load-network (pick-network network remote) remote)]
    (-> network
        (d/remove-peer peer-name)
        (save-network remote))))

(defn route-add [addresses {:keys [remote network peer]}]
  (let [network (load-network (pick-network network remote) remote)
        peer-name peer
        route (d/make-route addresses)]
    (-> network
        (d/add-route peer-name route)
        (save-network remote))))

(defn route-list [{:keys [remote network peer]}]
  (let [network (load-network (pick-network network remote) remote)
        peer-name peer
        route-addresses (d/list-routes network peer-name)]
    (println (str/join "\n" route-addresses))))

(defn route-remove [addresses {:keys [remote network peer]}]
  (let [network (load-network (pick-network network remote) remote)
        peer-name peer]
    (-> network
        (d/remove-route peer-name addresses)
        (save-network remote))))

(defn complete-type [type {:keys [remote] :as options}]
  (when-not remote
    (case type
      :network (str/split (with-out-str (network-list options)) #"\n")
      :peer (str/split (with-out-str (peer-list options)) #"\n")
      :peer-property (str/split (with-out-str (peer-properties)) #"\n")
      :route (str/split (with-out-str (route-list options)) #"\n")
      nil)))

(defn complete [args]
  (let [completion (cli/complete cli/spec args)]
    (if (map? completion)
      (try
        (let [{:keys [type opts]} completion]
          (when-let [coll (complete-type type opts)]
            (println (str/join "\n" coll))))
        (catch Exception _))
      (println (str/join "\n" completion)))))

(defn print-version []
  (let [version (str/trim (slurp (io/resource "version")))]
    (println (format "wgctl %s" version))))
