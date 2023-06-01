(ns agorgl.wgctl.domain-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [agorgl.wgctl.domain :refer [network-name?
                                         address?
                                         cidr?
                                         make-route
                                         make-peer
                                         make-self-peer
                                         make-network
                                         add-peer
                                         get-peer-prop
                                         set-peer-prop
                                         list-peers
                                         remove-peer
                                         add-route
                                         list-routes
                                         remove-route]]))

(deftest validation-test
  (testing "validates network names"
    (is (network-name? "wg0"))
    (is (network-name? "wg-hello"))
    (is (network-name? "wg-aloha0"))
    (is (not (network-name? "wg-toolongnetname")))
    (is (not (network-name? "wg-inv@l1d"))))

  (testing "validates network addresses"
    (is (address? "10.0.0.1"))
    (is (address? "192.168.1.1")))

  (testing "validates network cidrs"
    (is (cidr? "10.0.0.0/16"))
    (is (cidr? "192.168.1.0/24"))))

(deftest domain-test
  (testing "makes valid peer"
    (is (s/valid? :peer/peer (make-peer "peer" "<pubkey>" "10.0.0.2"))))

  (testing "makes valid network"
    (let [self-peer (make-self-peer "<privkey>" "<pubkey>" "10.0.0.1")]
      (is (s/valid? :network/network (make-network "network" "10.0.0.0/24" self-peer)))))

  (testing "adds peer to network"
    (let [self-peer (make-self-peer "<privkey>" "<pubkey>" "10.0.0.1")
          network (make-network "network" "10.0.0.0/24" self-peer)
          peer (make-peer "peer" "<pubkey>" "10.0.0.2")]
      (is (= 2 (count (:peers (add-peer network peer)))))
      (is (thrown? Exception (add-peer (add-peer network peer) peer)))))

  (testing "gets peer property"
    (let [self-peer (make-self-peer "<privkey>" "<pubkey>" "10.0.0.1")
          network (make-network "network" "10.0.0.0/24" self-peer)]
      (is (= (get-peer-prop network "self" "address") "10.0.0.1"))
      (is (= (get-peer-prop network "self" "endpoint") nil))))

  (testing "sets peer property"
    (let [self-peer (make-self-peer "<privkey>" "<pubkey>" "10.0.0.1")
          network (make-network "network" "10.0.0.0/24" self-peer)]
      (is (= (get-in (set-peer-prop network "self" "endpoint" "somehost:51820") [:peers 0 :endpoint]) "somehost:51820"))
      (is (thrown? Exception (set-peer-prop network "self" "name" "otherself")))
      (is (thrown? Exception (set-peer-prop network "self" "public-key" 12345)))
      (is (thrown? Exception (set-peer-prop network "self" "public-key" nil)))
      (is (thrown? Exception (set-peer-prop network "self" "fafoufa" "fafafa")))))

  (testing "lists network peers"
    (let [self-peer (make-self-peer "<privkey>" "<pubkey>" "10.0.0.1")
          network (make-network "network" "10.0.0.0/24" self-peer)
          peer (make-peer "peer" "<pubkey>" "10.0.0.2")]
      (is (= (list-peers network) ["self"]))
      (is (= (list-peers (add-peer network peer)) ["self" "peer"]))))

  (testing "removes network peer"
    (let [self-peer (make-self-peer "<privkey>" "<pubkey>" "10.0.0.1")
          network (make-network "network" "10.0.0.0/24" self-peer)
          peer (make-peer "peer" "<pubkey>" "10.0.0.2")]
      (is (thrown? Exception (remove-peer network "self")))
      (is (= 1 (count (:peers (remove-peer (add-peer network peer) "peer")))))))

  (testing "adds route to peer"
    (let [self-peer (make-self-peer "<privkey>" "<pubkey>" "10.0.0.1")
          network (make-network "network" "10.0.0.0/24" self-peer)
          route (make-route "172.16.0.0/16")]
      (is (= (:addresses (get-in (add-route network "self" route) [:peers 0 :routes 0]))
             "172.16.0.0/16"))
      (is (thrown? Exception (add-route (add-route network "self" route) "self" route)))))

  (testing "lists peer routes"
    (let [self-peer (make-self-peer "<privkey>" "<pubkey>" "10.0.0.1")
          network (make-network "network" "10.0.0.0/24" self-peer)
          route (make-route "172.16.0.0/16")]
      (is (= (list-routes (add-route network "self" route) "self")
             ["172.16.0.0/16"]))))

  (testing "removes peer route"
    (let [self-peer (make-self-peer "<privkey>" "<pubkey>" "10.0.0.1")
          network (make-network "network" "10.0.0.0/24" self-peer)
          route (make-route "172.16.0.0/16")]
      (is (= (get-in (remove-route (add-route network "self" route) "self" "172.16.0.0/16") [:peers 0 :routes])
             [])))))
