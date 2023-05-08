(ns agorgl.wgctl.cli-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [agorgl.wgctl.cli :refer [subspec usage summary parse-args]]))

(def spec
  {:name "farmctl"
   :desc "Manage farm barns and animals"
   :opts [{:name "farm"
           :refn "FARM"
           :desc "Farm to manage"
           :alias "f"}
          {:name "version"
           :desc "Show program version"
           :alias "v"}]
   :cmds [{:name "barn"
           :desc "Manage barns"
           :cmds [{:name "add"
                   :desc "Add barn"
                   :args [{:name "name"}
                          {:name "type" :optional true}]}
                  {:name "ls"
                   :desc "List barns"}
                  {:name "rm"
                   :desc "Remove barn"
                   :args [{:name "name"}]}]}
          {:name "animal"
           :desc "Manage animals"
           :opts [{:name "barn"
                   :refn "BARN"
                   :desc "Barn to manage"
                   :alias "b"}]
           :cmds [{:name "add"
                   :desc "Add animal"
                   :args [{:name "type"}
                          {:name "name"}]}
                  {:name "ls"
                   :desc "List animals"}
                  {:name "rm"
                   :desc "Remove animal"
                   :args [{:name "name"}]}]}]})

(deftest subspec-test
  (testing "gets subspec spec"
    (is (= (subspec spec ["farmctl" "barn"])
           (get-in spec [:cmds 0])))
    (is (= (subspec spec ["farmctl" "animal" "ls"])
           (get-in spec [:cmds 1 :cmds 1])))
    (is (= (subspec spec ["farmctl" "fafa"])
           nil))
    (is (= (subspec (subspec spec ["farmctl" "barn"]) ["barn" "fafa"])
           nil))))

(deftest usage-test
  (testing "generates command usage"
    (is (= (usage spec ["farmctl"])
           "farmctl [options] [command]"))
    (is (= (usage spec ["farmctl" "barn"])
           "farmctl barn [command]"))
    (is (= (usage spec ["farmctl" "barn" "add"])
           "farmctl barn add <name> [type]"))))

(deftest summary-test
  (testing "generates command summary"
    (is (= (summary spec ["farmctl"])
           (str/join "\n" ["Manage farm barns and animals"
                           ""
                           "Usage:"
                           "  farmctl [options] [command]"
                           ""
                           "Commands:"
                           "  barn     Manage barns"
                           "  animal   Manage animals"
                           ""
                           "Options:"
                           "  -f, --farm      Farm to manage"
                           "  -v, --version   Show program version"])))))

(defn has-error? [re result]
  (->> result
      (map :errors)
      (concat)
      (flatten)
      (first)
      (re-seq re)
      (seq)))

(defn fullcmd [result]
  (-> result last :command))

(deftest parse-test
  (testing "parses and validates arguments"
    (let [result (parse-args spec ["farmctl" "barn" "add" "cozyhouse"])]
      (is (= (fullcmd result) ["farmctl" "barn" "add"]))
      (is (= (-> result last :arguments) {:name "cozyhouse"})))
    (let [result (parse-args spec ["farmctl" "barn" "add" "cozyhouse" "cowbarn"])]
      (is (= (fullcmd result) ["farmctl" "barn" "add"]))
      (is (= (-> result last :arguments) {:name "cozyhouse" :type "cowbarn"})))
    (let [result (parse-args spec ["farmctl" "animal" "-b" "cozyhouse" "ls"])]
      (is (= (fullcmd result) ["farmctl" "animal" "ls"]))
      (is (= (-> result second :options) {:barn "cozyhouse"})))
    (let [result (parse-args spec ["farmctl" "-f" "littlefarm" "animal" "-b" "cozyhouse" "add" "cow" "george"])]
      (is (= (fullcmd result) ["farmctl" "animal" "add"]))
      (is (= (-> result first :options) {:farm "littlefarm"}))
      (is (= (-> result second :options) {:barn "cozyhouse"}))
      (is (= (-> result last :arguments) {:type "cow" :name "george"})))
    (is (has-error? #"Missing command" (parse-args spec ["farmctl"])))
    (is (has-error? #"Missing command" (parse-args spec ["farmctl" "barn"])))
    (is (has-error? #"Unrecognized command" (parse-args spec ["farmctl" "fafa"])))
    (is (has-error? #"Unrecognized command" (parse-args spec ["farmctl" "barn" "foufou"])))
    (is (has-error? #"Unknown option" (parse-args spec ["farmctl" "-x"])))
    (is (has-error? #"Unknown option" (parse-args spec ["farmctl" "animal" "-x"])))
    (is (has-error? #"Incorrect number of arguments" (parse-args spec ["farmctl" "barn" "add"])))
    (is (has-error? #"Incorrect number of arguments" (parse-args spec ["farmctl" "barn" "add" "fa" "fou" "fe"])))))
