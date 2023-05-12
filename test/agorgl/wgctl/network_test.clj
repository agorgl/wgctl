(ns agorgl.wgctl.network-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [agorgl.wgctl.network :refer [ip->int
                                          int->ip
                                          address
                                          plength
                                          network
                                          block
                                          host-range
                                          in-host-range?
                                          next-ip
                                          next-available-ip]]))

(deftest conversion-test
  (testing "converts between ip representations"
    (is (= (ip->int "10.2.2.5")
           167903749))
    (is (= (int->ip 167903749)
           "10.2.2.5"))))

(deftest cidr-test
  (testing "parses cidr components"
    (is (= (address "10.2.2.5/24")
           "10.2.2.5"))
    (is (= (plength "10.2.2.5/24")
           24))))

(deftest network-test
  (testing "performs network calculations"
    (is (= (network "10.2.2.5/24")
           "10.2.2.0"))
    (is (= (block "10.2.2.5/24")
           "10.2.2.0/24"))
    (is (= (host-range "10.2.2.5/24")
           ["10.2.2.1" "10.2.2.254"]))
    (is (= (in-host-range? "10.2.2.0/24" "10.2.2.5")
           true))
    (is (= (in-host-range? "10.2.2.0/24" "10.2.5.5")
           false))
    (is (= (next-ip "10.2.2.5/24")
           "10.2.2.6"))
    (is (= (next-ip "10.2.2.254/24")
           nil))
    (is (= (next-available-ip "10.2.2.0/24" [])
           "10.2.2.1"))
    (is (= (next-available-ip "10.2.2.0/24" ["10.2.2.1"])
           "10.2.2.2"))
    (is (= (next-available-ip "10.2.2.0/24" ["10.2.2.1" "10.2.2.2"])
           "10.2.2.3"))
    (is (= (next-available-ip "10.2.2.0/24" ["10.2.2.1" "10.2.2.3"])
           "10.2.2.2"))
    (is (= (next-available-ip "10.2.2.0/24" ["10.2.2.2"])
           "10.2.2.1"))
    (is (= (next-available-ip "10.2.2.0/24" (map #(str "10.2.2." %) (range 1 255)))
           nil))))
