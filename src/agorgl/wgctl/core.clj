(ns agorgl.wgctl.core
  (:gen-class)
  (:require [agorgl.wgctl.cli :as cli]
            [agorgl.wgctl.command :as cmd]))

(defn greet
  "Callable entry point to the application."
  [data]
  (println (str "Hello, " (or (:name data) "World") "!")))

(defn check-complete [args]
  (when (= (first args) "complete")
    (cmd/complete (rest args))
    (System/exit 0)))

(defn check-help [args]
  (let [hlpcmd (->> args (filter #(-> % :options :help)) first)]
    (when (some? hlpcmd)
      (-> (cli/summary cli/spec (:command hlpcmd))
          (println))
      (System/exit 0))))

(defn check-version [args]
  (let [vercmd (->> args (filter #(-> % :options :version)) first)]
    (when (some? vercmd)
      (cmd/print-version)
      (System/exit 0))))

(defn check-error [args]
  (let [errcmd (->> args (filter #(seq (:errors %))) first)]
    (when (some? errcmd)
      (println "Error: " (first (:errors errcmd)))
      (System/exit 1))))

(defn dispatch-array [args]
  (let [allopts (->> args (map :options) (apply merge))
        {:keys [command options arguments]} (-> args last (assoc :options allopts))]
    (case (rest command)
      ["network" "add"] [:network-add (:name arguments) (:addresses arguments) options]
      ["network" "ls"] [:network-list options]
      ["network" "rm"] [:network-remove (:name arguments) options]
      ["network" "show"] [:network-show (:name arguments) options]
      ["network" "import"] [:network-import (:file arguments) options]
      ["network" "export"] [:network-export (:name arguments) (:file arguments) options]
      ["network" "up"] [:network-up (:name arguments) options]
      ["network" "down"] [:network-down (:name arguments) options]
      ["network" "reload"] [:network-reload (:name arguments) options]
      ["peer" "add"] [:peer-add (:name arguments) (:public-key arguments) options]
      ["peer" "get"] [:peer-get (:name arguments) (:property arguments) options]
      ["peer" "set"] [:peer-set (:name arguments) (:property arguments) (:value arguments) options]
      ["peer" "ls"] [:peer-list options]
      ["peer" "rm"] [:peer-remove (:name arguments) options]
      ["peer" "show"] [:peer-show (:name arguments) options]
      ["route" "add"] [:route-add (:addresses arguments) options]
      ["route" "ls"] [:route-list options]
      ["route" "rm"] [:route-remove (:addresses arguments) options])))

(defn dispatch [args]
  (-> (case (first args)
        :network-add cmd/network-add
        :network-list cmd/network-list
        :network-remove cmd/network-remove
        :network-show cmd/network-show
        :network-import cmd/network-import
        :network-export cmd/network-export
        :network-up cmd/network-up
        :network-down cmd/network-down
        :network-reload cmd/network-reload
        :peer-add cmd/peer-add
        :peer-get cmd/peer-get
        :peer-set cmd/peer-set
        :peer-list cmd/peer-list
        :peer-remove cmd/peer-remove
        :peer-show cmd/peer-show
        :route-add cmd/route-add
        :route-list cmd/route-list
        :route-remove cmd/route-remove)
      (apply (rest args))))

(defn -main [& args]
  (check-complete args)
  (let [args (cli/parse-args cli/spec (concat ["wgctl"] args))]
    (check-help args)
    (check-version args)
    (check-error args)
    (try
      (dispatch (dispatch-array args))
      (catch Exception e
        (println (.getMessage e))
        (System/exit 1))))
  (shutdown-agents)
  (System/exit 0))
