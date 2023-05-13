(ns agorgl.wgctl.domain
  (:require [clojure.spec.alpha :as s]))

(defn network-name? [s]
  (re-matches #"^(\p{Lower}|\d|-){2,15}$" s))

(defn address? [s]
  (re-matches #"^(\d){1,3}(\.\d{1,3}){3}$" s))

(defn cidr? [s]
  (re-matches #"^(\d){1,3}(\.\d{1,3}){3}/(\d){1,2}$" s))

(s/def :peer/name
  string?)

(s/def :peer/private-key
  string?)

(s/def :peer/public-key
  string?)

(s/def :peer/address
  (s/and string? address?))

(s/def :peer/endpoint
  string?)

(s/def :peer/hub
  boolean?)

(s/def :peer/peer
  (s/keys :req-un [:peer/name
                   :peer/public-key
                   :peer/address]
          :opt-un [:peer/private-key
                   :peer/endpoint
                   :peer/hub]))

(defn make-peer [name public-key address]
  (s/assert :peer/peer
            {:name name
             :public-key public-key
             :address address}))

(defn make-self-peer [private-key public-key address]
  (s/assert :peer/peer
            {:name "self"
             :private-key private-key
             :public-key public-key
             :address address}))

(s/def :network/name
  (s/and string? network-name?))

(s/def :network/addresses
  (s/and string? cidr?))

(s/def :network/peers
  (s/coll-of :peer/peer))

(s/def :network/network
  (s/keys :req-un [:network/name
                   :network/addresses
                   :network/peers]))

(defn make-network [name addresses self]
  (s/assert :network/network
            {:name name
             :addresses addresses
             :peers [self]}))

(defn find-peer [network peer-name]
  (->> network
       :peers
       (filter #(= (:name %) peer-name))
       first))

(defn add-peer [network peer]
  (if (not (find-peer network (:name peer)))
    (update-in network [:peers] conj peer)
    (let [msg (format "Peer with name '%s' already exists in network %s"
                      (:name peer)
                      (:name network))]
      (throw (ex-info msg {})))))

(defn list-peers [network]
  (map :name (:peers network)))

(defn remove-peer [network peer-name]
  (if (not= peer-name "self")
    (update-in network [:peers] (fn [peers] (remove #(= (:name %) peer-name) peers)))
    (let [msg "Cannot remove 'self' from peers list"]
      (throw (ex-info msg {})))))
