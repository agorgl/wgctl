(ns agorgl.wgctl.domain-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [agorgl.wgctl.domain :refer [network-name?
                                         address?
                                         cidr?
                                         make-peer
                                         make-self-peer
                                         make-network
                                         add-peer
                                         list-peers
                                         remove-peer]]))

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
      (is (= 1 (count (:peers (remove-peer (add-peer network peer) "peer"))))))))