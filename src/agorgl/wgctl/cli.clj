(ns agorgl.wgctl.cli
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.tools.cli :as cli]))

(def spec
  {:name "wgctl"
   :desc "Manage wireguard networks and peers"
   :opts [{:type :remote
           :name "remote"
           :refn "HOST"
           :desc "Manage remote host"
           :alias "r"
           :env "WIREGUARD_REMOTE"}
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
                   :args [{:type :network
                           :name "name"}]}
                  {:name "show"
                   :desc "Show network"
                   :args [{:type :network
                           :name "name"}]}]}
          {:name "peer"
           :desc "Manage peers"
           :opts [{:type :network
                   :name "network"
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
                  {:name "get"
                   :desc "Get peer property"
                   :args [{:type :peer
                           :name "name"}
                          {:type :peer-property
                           :name "property"}]}
                  {:name "set"
                   :desc "Set peer property"
                   :args [{:type :peer
                           :name "name"}
                          {:type :peer-property
                           :name "property"}
                          {:name "value"}]}
                  {:name "ls"
                   :desc "List peers"}
                  {:name "rm"
                   :desc "Remove peer"
                   :args [{:type :peer
                           :name "name"}]}
                  {:name "show"
                   :desc "Show peer"
                   :args [{:type :peer
                           :name "name"}]}]}
          {:name "route"
           :desc "Manage routes"
           :opts [{:type :network
                   :name "network"
                   :refn "NETWORK"
                   :desc "Network to manage"
                   :alias "n"}
                  {:type :peer
                   :name "peer"
                   :refn "PEER"
                   :desc "Peer to manage"
                   :alias "p"}
                  {:name "help"
                   :desc "Show help summary"
                   :alias "h"}]
           :cmds [{:name "add"
                   :desc "Add route"
                   :args [{:name "addresses"}]}
                  {:name "ls"
                   :desc "List routes"}
                  {:name "rm"
                   :desc "Remove route"
                   :args [{:type :route
                           :name "addresses"}]}]}
          {:name "complete"
           :desc "Complete commands"
           :hidden true}]})

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
        {:keys [desc cmds opts]} (-> (subspec spec cmd)
                                     (update-in [:cmds] #(remove :hidden %)))
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
        (cond-> (seq cmds)
          (conj ["Commands:" (map str-cmd cmds)]))
        (cond-> (some? opts)
          (conj ["Options:" (map str-opt opts)]))
        (->> (map str-section)
             (str/join "\n\n")))))

(defn opts->tools-cli-spec [opts]
  (let [convert
        (fn [{:keys [name alias desc refn env]}]
          (merge
           {:id name
            :desc desc
            :long-opt (str "--" name)}
           (when alias
             {:short-opt (str "-" alias)})
           (when (some? refn)
             {:required refn})
           (when env
             {:default (System/getenv env)})))]
    (mapv convert opts)))

(defn distinct-by [f coll]
  (let [step (fn step [xs seen]
               (lazy-seq
                ((fn [[x :as xs] seen]
                   (when-let [s (seq xs)]
                     (let [fx (f x)]
                       (if (contains? seen fx)
                         (recur (rest s) seen)
                         (cons x (step (rest s) (conj seen fx)))))))
                 xs seen)))]
    (step coll #{})))

(defn cmdopts [spec cmd]
  (loop [spec spec
         opts (:opts spec)
         cmd (rest cmd)]
    (if (seq cmd)
      (let [spec (->> spec :cmds (filter #(= (:name %) (first cmd))) first)]
        (recur spec
               (->> (:opts spec)
                    (concat opts)
                    (reverse)
                    (distinct-by :name)
                    (reverse)
                    (into []))
               (rest cmd)))
      opts)))

(defn remove-nils [m]
  (into {} (remove (fn [[_ v]] (nil? v)) m)))

(defn parse-opts [spec [cmd args]]
  (let [{:keys [cmds]} (subspec spec cmd)
        opts (cmdopts spec cmd)]
    (cli/parse-opts
     args (opts->tools-cli-spec opts)
     :in-order true)))

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
           (update-in [:options] (fn [opts] (-> opts (update-keys keyword) remove-nils)))
           (update-in [:arguments] (fn [args] (zipmap (->> spec :args (map #(keyword (:name %)))) args)))
           (update-in [:errors] concat command-errors argument-errors)
           (cond-> (and (empty? (:cmds spec)) (> (count args) (count (:args spec))))
             (update-in [:rest-arguments] (fn [_] (subvec args (count (:args spec)))))))
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

(defn second-to-last [coll]
  (first (take-last 2 coll)))

(defn complete [spec args]
  (let [result (parse-args spec args)
        command (-> result last :command)
        subspec (subspec spec command)
        cmdopts (cmdopts spec command)
        previous-opt (first (filter #(#{(:name %) (:alias %)} (str/replace (second-to-last args) #"^-+" "")) cmdopts))
        next-arg (get (:args subspec) (dec (+ (count (-> result last :arguments)) (count (-> result last :rest-arguments)))))
        allopts (->> result (map :options) (apply merge) (#(dissoc % (keyword (:name previous-opt)))))]
    (if-let [t (or (:type previous-opt)
                   (:type next-arg))]
      {:type t :opts allopts}
      (let [available-cmds (->> (:cmds subspec) (remove :hidden) (map :name))
            provided-opts (map name (keys allopts))
            available-opts (map :name cmdopts)
            unspecified-opts (set/difference (into #{} available-opts) (into #{} provided-opts))]
        (if (or (str/starts-with? (last args) "-") (empty? available-cmds))
          (map #(str "--" %) unspecified-opts)
          (seq available-cmds))))))
