(ns agorgl.wgctl.firewall
  (:require [clojure.string :as str]))

(def listen-chains "
  chain {{prefix}}_input {
      udp dport {{listen-port}} counter accept
  }
")

(def hub-chains "
  chain {{prefix}}_forward {
      iifname {{interface}} counter accept
      oifname {{interface}} counter accept
  }
")

(def nat-chains "
  chain {{prefix}}_prerouting {
      iifname {{interface}} meta mark set 0x7767 counter
  }

  chain {{prefix}}_postrouting {
      oifname != {{interface}} mark 0x7767 counter masquerade
      oifname {{interface}} counter masquerade
  }
")

(def input-rules "jump {{prefix}}_input")
(def forward-rules "jump {{prefix}}_forward")
(def prerouting-rules "jump {{prefix}}_prerouting")
(def postrouting-rules "jump {{prefix}}_postrouting")

(defn unindent [s]
  (str/replace s #"(?m)^[^\S\r\n]{2}" ""))

(defn strip-empty-lines [s]
  (str/replace s #"(^\s*|\s*$)" ""))

(defn render-template [template vars]
  (reduce (fn [t [k v]]
            (str/replace t (format "{{%s}}" (name k)) (str v)))
          template
          (->> vars
               (into [] cat)
               (partition 2))))

(defn config-dir []
  (or (System/getenv "FIREWALL_DIR") "/etc/nftables.d/wireguard"))

(defn template-file [template params]
  (-> template
      (unindent)
      (strip-empty-lines)
      (render-template params)))

(defn file [interface-name ftype listen-port]
  (let [prefix (str/replace interface-name #"\-" "_")
        params {:prefix prefix
                :interface interface-name
                :listen-port listen-port}]
    (case ftype
      :listen-chains (template-file listen-chains params)
      :hub-chains (template-file hub-chains params)
      :nat-chains (template-file nat-chains params)
      :input-rules (template-file input-rules params)
      :forward-rules (template-file forward-rules params)
      :prerouting-rules (template-file prerouting-rules params)
      :postrouting-rules (template-file postrouting-rules params))))
