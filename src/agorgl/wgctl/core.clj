(ns agorgl.wgctl.core
  (:gen-class)
  (:require [agorgl.wgctl.cli :as cli]))

(defn greet
  "Callable entry point to the application."
  [data]
  (println (str "Hello, " (or (:name data) "World") "!")))

(defn check-help [args]
  (let [hlpcmd (->> args (filter #(-> % :options :help)) first)]
    (when (some? hlpcmd)
      (-> (cli/summary cli/spec (:command hlpcmd))
          (println))
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
      ["network" "create"] [:network-create (:name arguments) (:addresses arguments) options]
      ["network" "ls"] [:network-list options]
      ["network" "rm"] [:network-remove (:name arguments) options]
      ["peer" "add"] [:peer-add (:name arguments) options]
      ["peer" "ls"] [:peer-list options]
      ["peer" "rm"] [:peer-remove (:name arguments) options])))

(defn dispatch [args]
  (prn args))

(defn -main [& args]
  (let [args (cli/parse-args cli/spec (concat ["wgctl"] args))]
    (check-help args)
    (check-error args)
    (dispatch (dispatch-array args))))
