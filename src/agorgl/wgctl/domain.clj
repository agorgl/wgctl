(ns agorgl.wgctl.domain
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]))

(defn network-name? [s]
  (re-matches #"^(\p{Lower}|\d|-){2,15}$" s))

(defn address? [s]
  (re-matches #"^(\d){1,3}(\.\d{1,3}){3}$" s))

(defn cidr? [s]
  (re-matches #"^(\d){1,3}(\.\d{1,3}){3}/(\d){1,2}$" s))

(s/def :route/addresses
  (s/and string? cidr?))

(s/def :route/route
  (s/keys :req-un [:route/addresses]))

(s/def :peer/name
  string?)

(defn make-route [addresses]
  (s/assert :route/route
            {:addresses addresses}))

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

(s/def :peer/routes
  (s/coll-of :route/route))

(s/def :peer/nat
  boolean?)

(s/def :peer/keepalive
  int?)

(s/def :peer/autofw
  boolean?)

(def peer-properties
  #{:name
    :public-key
    :address
    :private-key
    :endpoint
    :hub
    :routes
    :nat
    :keepalive
    :autofw})

(s/def :peer/peer
  (s/and
   (s/keys :req-un [:peer/name
                    :peer/public-key
                    :peer/address]
           :opt-un [:peer/private-key
                    :peer/endpoint
                    :peer/hub
                    :peer/routes
                    :peer/nat
                    :peer/keepalive
                    :peer/autofw])
   #(every? peer-properties (keys %))))

(defn make-peer [name public-key address]
  (s/assert :peer/peer
            {:name name
             :public-key public-key
             :address address
             :routes []}))

(defn prop-error [spec m]
  (when-let [problem (first (::s/problems (s/explain-data spec m)))]
    (let [{:keys [path pred val]} problem
          nested-pred (when (seq? pred) (nth pred 2))]
      (cond
        (seq path)
        (let [invalid (last path)]
          [:invalid-value (name invalid)])
        (= (first nested-pred) 'clojure.core/contains?)
        (let [[_ _ required-key] nested-pred]
          [:missing-key (name required-key)])
        (= (first nested-pred) 'clojure.core/every?)
        (let [[_ allowed-keys] nested-pred
              disallowed-keys (set/difference (set (keys val)) allowed-keys)]
          [:invalid-key (name (first disallowed-keys))])
        :else
        (let [object (-> problem :via first name)]
          [:invalid-state object])))))

(defn make-self-peer [private-key public-key address]
  (s/assert :peer/peer
            {:name "self"
             :private-key private-key
             :public-key public-key
             :address address
             :routes []}))

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
       (keep-indexed #(when (= (:name %2) peer-name) %1))
       first))

(defn add-peer [network peer]
  (if (not (find-peer network (:name peer)))
    (update-in network [:peers] conj peer)
    (let [msg (format "Peer with name '%s' already exists in network %s"
                      (:name peer)
                      (:name network))]
      (throw (ex-info msg {})))))

(defn get-peer [network peer-name]
  (if-let [peer-index (find-peer network peer-name)]
    (let [peer (get-in network [:peers peer-index])]
      peer)
    (let [msg (format "Peer with name '%s' does not exist in network %s"
                      peer-name
                      (:name network))]
      (throw (ex-info msg {})))))

(defn get-peer-prop [network peer-name property]
  (if-let [peer-index (find-peer network peer-name)]
    (let [peer (get-in network [:peers peer-index])]
      ((keyword property) peer))
    (let [msg (format "Peer with name '%s' does not exist in network %s"
                      peer-name
                      (:name network))]
      (throw (ex-info msg {})))))

(defn convert-boolean [value]
  (case value
    "true" true
    "false" false
    (let [msg (format "Cannot convert value '%s' to boolean" value)]
      (throw (ex-info msg {})))))

(defn convert-int [value]
  (try
    (Integer/parseInt value)
    (catch Exception _
      (let [msg (format "Cannot convert value '%s' to int" value)]
        (throw (ex-info msg {}))))))

(defn convert-prop [prop value]
  (let [f (case prop
            :hub convert-boolean
            :nat convert-boolean
            :keepalive convert-int
            :autofw convert-boolean
            identity)]
    (f value)))

(defn set-peer-prop [network peer-name property value]
  (when (and (= peer-name "self") (= property "name"))
    (let [msg (format "Cannot set property '%s' of peer '%s'" property peer-name)]
      (throw (ex-info msg {}))))
  (if-let [peer-index (find-peer network peer-name)]
    (let [peer (get-in network [:peers peer-index])
          updated-peer
          (if (not (#{"nil" ""} value))
            (assoc peer (keyword property) (convert-prop (keyword property) value))
            (dissoc peer (keyword property)))]
      (if (s/valid? :peer/peer updated-peer)
        (assoc-in network [:peers peer-index] updated-peer)
        (let [[error p] (prop-error :peer/peer updated-peer)
              msg (case error
                    :invalid-value (format "Invalid value for peer property '%s'" p)
                    :missing-key (format "Cannot remove required peer property '%s'" p)
                    :invalid-key (format "Invalid peer property '%s'" p)
                    :invalid-state (format "Invalid state for peer object"))]
          (throw (ex-info msg {})))))
    (let [msg (format "Peer with name '%s' does not exist in network %s"
                      peer-name
                      (:name network))]
      (throw (ex-info msg {})))))

(defn list-peers [network]
  (map :name (:peers network)))

(defn remove-peer [network peer-name]
  (if (not= peer-name "self")
    (update-in network [:peers] (fn [peers] (remove #(= (:name %) peer-name) peers)))
    (let [msg "Cannot remove 'self' from peers list"]
      (throw (ex-info msg {})))))

(defn find-route [peer addresses]
  (->> peer
       :routes
       (keep-indexed #(when (= (:addresses %2) addresses) %1))
       first))

(defn add-route [network peer-name route]
  (if-let [peer-index (find-peer network peer-name)]
    (let [peer (get-in network [:peers peer-index])]
      (if (not (find-route peer (:addresses route)))
        (update-in network [:peers peer-index :routes] conj route)
        (let [msg (format "Route with addresses '%s' already exists in peer %s"
                          (:addresses route)
                          (:name peer))]
          (throw (ex-info msg {})))))
    (let [msg (format "Peer with name '%s' does not exist in network %s"
                      peer-name
                      (:name network))]
      (throw (ex-info msg {})))))

(defn list-routes [network peer-name]
  (if-let [peer-index (find-peer network peer-name)]
    (let [peer (get-in network [:peers peer-index])]
      (map :addresses (:routes peer)))
    (let [msg (format "Peer with name '%s' does not exist in network %s"
                      peer-name
                      (:name network))]
      (throw (ex-info msg {})))))

(defn remove-route [network peer-name route-addresses]
  (if-let [peer-index (find-peer network peer-name)]
    (update-in network [:peers peer-index :routes]
               (fn [routes] (remove #(= (:addresses %) route-addresses) routes)))
    (let [msg (format "Peer with name '%s' does not exist in network %s"
                      peer-name
                      (:name network))]
      (throw (ex-info msg {})))))
