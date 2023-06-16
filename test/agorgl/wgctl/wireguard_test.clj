(ns agorgl.wgctl.wireguard-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [agorgl.wgctl.wireguard :refer [config-file]]))

(deftest config-test
  (testing "generates config file"
    (is (= (config-file
            [{:private-key "<privkey>"
              :address "10.2.2.1/24"
              :listen-port 51820}
             {:name "peer1"
              :public-key "<pubkey1>"
              :allowed-ips "10.2.2.2/32"}
             {:name "peer2"
              :public-key "<pubkey2>"
              :allowed-ips ["10.2.2.3/32" "192.168.100.0/24"]}])
           (str/join "\n" ["[Interface]"
                           "PrivateKey = <privkey>"
                           "ListenPort = 51820"
                           "Address = 10.2.2.1/24"
                           ""
                           "# peer1"
                           "[Peer]"
                           "PublicKey = <pubkey1>"
                           "AllowedIPs = 10.2.2.2/32"
                           ""
                           "# peer2"
                           "[Peer]"
                           "PublicKey = <pubkey2>"
                           "AllowedIPs = 10.2.2.3/32, 192.168.100.0/24"])))))
