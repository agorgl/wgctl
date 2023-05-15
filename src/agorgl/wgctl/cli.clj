(ns agorgl.wgctl.cli
  (:require [clojure.string :as str]
            [clojure.tools.cli :as cli]))

(def spec
  {:name "wgctl"
   :desc "Manage wireguard networks and peers"
   :opts [{:name "remote"
           :refn "HOST"
           :desc "Manage remote host"
           :alias "r"}
          {:name "version"
           :desc "Show program version"
           :alias "v"}
          {:name "help"
           :desc "Show help summary"
           :alias "h"}]
   :cmds [{:name "network"
           :desc "Manage networks"
           :opts [{:name "help"
                   :desc "Show help summary"
                   :alias "h"}]
           :cmds [{:name "create"
                   :desc "Create network"
                   :args [{:name "name"}
                          {:name "addresses"}]}
                  {:name "ls"
                   :desc "List networks"}
                  {:name "rm"
                   :desc "Remove network"
                   :args [{:name "name"}]}]}
          {:name "peer"
           :desc "Manage peers"
           :opts [{:name "network"
                   :refn "NETWORK"
                   :desc "Network to manage"
                   :alias "n"}
                  {:name "help"
                   :desc "Show help summary"
                   :alias "h"}]
           :cmds [{:name "add"
                   :desc "Add peer"
                   :args [{:name "name"}
                          {:name "public-key"}]}
                  {:name "set"
                   :desc "Set peer property"
                   :args [{:name "name"}
                          {:name "property"}
                          {:name "value"}]}
                  {:name "ls"
                   :desc "List peers"}
                  {:name "rm"
                   :desc "Remove peer"
                   :args [{:name "name"}]}]}]})

(defn subspec [spec cmd]
  (loop [spec spec
         cmd (rest cmd)]
    (if (seq cmd)
      (recur (->> spec :cmds (filter #(= (:name %) (first cmd))) first)
             (rest cmd))
      spec)))

(defn usage [spec cmd]
  (let [{:keys [opts args cmds]} (subspec spec cmd)]
    (->> [(str/join " " cmd)
          (when (some? opts) "[options]")
          (when (some? cmds) "[command]")
          (when (some? args)
            (let [format-arg
                  (fn [{:keys [name optional]}]
                    (let [fmt (if optional "[%s]" "<%s>")]
                      (format fmt name)))]
              (->> args
                   (map format-arg)
                   (str/join " "))))]
         (filter some?)
         (str/join " "))))

(defn summary [spec cmd]
  (let [usage (usage spec cmd)
        {:keys [desc cmds opts]} (subspec spec cmd)
        str-section (fn [[title lines]]
                      (str/join "\n" (into [title] (map #(str "  " %) lines))))
        str-cmd (fn [{:keys [name desc]}]
                  (format "%-8s %s" name desc))
        str-opt (fn [{:keys [name alias desc]}]
                  (format "-%s, --%-8s  %s" alias name desc))]
    (-> []
        (cond-> (some? desc)
          (conj [desc]))
        (cond-> (some? usage)
          (conj ["Usage:" [usage]]))
        (cond-> (some? cmds)
          (conj ["Commands:" (map str-cmd cmds)]))
        (cond-> (some? opts)
          (conj ["Options:" (map str-opt opts)]))
        (->> (map str-section)
             (str/join "\n\n")))))

(defn opts->tools-cli-spec [opts]
  (let [convert
        (fn [{:keys [name alias desc refn]}]
          (merge
           {:id name
            :desc desc
            :long-opt (str "--" name)}
           (when alias
             {:short-opt (str "-" alias)})
           (when (some? refn)
             {:required refn})))]
    (mapv convert opts)))

(defn parse-opts [spec [cmd args]]
  (let [{:keys [cmds opts]} (subspec spec cmd)]
    (cli/parse-opts
     args (opts->tools-cli-spec opts)
     :in-order (some? cmds))))

(defn parse-cmd [spec [cmd args]]
  (let [parse-result (parse-opts spec [cmd args])
        args (-> parse-result :arguments)
        command (first args)]
    (let [usage (usage spec cmd)
          spec (subspec spec cmd)
          command-errors
          (when (some? (:cmds spec))
            (-> []
                (cond-> (nil? command)
                  (conj (format "Missing command '%s COMMAND'" (str/join " " cmd))))
                (cond-> (and (some? command) (nil? (subspec spec [(last cmd) command])))
                  (conj (format "Unrecognized command `%s`" (str/join " " (conj cmd command)))))))
          argument-errors
          (when (some? (:args spec))
            (-> []
                (cond-> (nil? args)
                  (conj (format "Missing arguments '%s'" usage)))
                (cond-> (not (<= (count (remove :optional (:args spec))) (count args) (count (:args spec))))
                  (conj (format "Incorrect number of arguments '%s'" usage)))))]
      [(-> {:command cmd}
           (merge (select-keys parse-result [:options :arguments :errors]))
           (update-in [:options] update-keys keyword)
           (update-in [:arguments] (fn [args] (zipmap (->> spec :args (map #(keyword (:name %)))) args)))
           (update-in [:errors] concat command-errors argument-errors))
       (when (and (seq args) (:cmds spec) (empty? command-errors))
         [(conj cmd command) (rest args)])])))

(defn parse-args [spec args]
  (loop [result []
         args [[(:name spec)] (rest args)]]
    (let [[r a] (parse-cmd spec args)
          result (conj result r)]
      (if (some? a)
        (recur result a)
        result))))
