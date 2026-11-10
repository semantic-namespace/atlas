(ns atlas.adapter.claude-code-test
  (:require [clojure.test :refer [deftest is testing]]
            [atlas.adapter.claude-code :as cc]))

(def profile-export
  {:summary "QA on staging"
   :commands {:deny ["psql *prod*"] :ask ["git push *"]}
   :network #{"github.com" "clojars.org"}
   :surfaces [{:surface/env :env/staging :surface/scope :mcp-scope/qa
               :tools {"read_thing" :allow "write_thing" :ask "send_thing" :deny}}]})

(deftest exact-rules-per-tool
  (is (= {:allow ["mcp__staging-qa__read_thing"]
          :ask ["mcp__staging-qa__write_thing" "Bash(git push *)"]
          :deny ["mcp__staging-qa__send_thing" "Bash(psql *prod*)"]}
         (cc/permissions profile-export)))
  (is (= ["clojars.org" "github.com"] (get-in (cc/settings profile-export) [:sandbox :network :allowedDomains]))))

(deftest a-server-confined-to-its-partition
  (testing "a remote server's url, and a bridge's MCP_SERVER_URL, get the partition's path"
    (is (= {"type" "http" "url" "https://api.example/mcp/qa"}
           (cc/rescope {"type" "http" "url" "https://api.example/mcp"} :mcp-scope/qa)))
    (is (= ["run" "-e" "KEY=secret" "-e" "MCP_SERVER_URL=https://api.example/mcp/qa" "img"]
           (get (cc/rescope {"command" "docker" "args" ["run" "-e" "KEY=secret" "-e" "MCP_SERVER_URL=https://api.example/mcp" "img"]}
                            :mcp-scope/qa)
                "args")))
    (is (= "https://api.example/mcp" (get (cc/rescope {"url" "https://api.example/mcp/atlas"} :mcp-scope/all) "url")))
    (is (nil? (cc/rescope {"command" "local-server"} :mcp-scope/qa)) "nothing to rescope"))
  (testing "mcp-config: one server per surface, from the user's server for its environment"
    (let [entries {"staging-mcp" {"type" "http" "url" "https://staging.example/mcp"}}]
      (is (= {:mcp-config {:mcpServers {"staging-qa" {"type" "http" "url" "https://staging.example/mcp/qa"}}} :missing []}
             (cc/mcp-config profile-export {:env/staging "staging-mcp"} entries)))
      (is (= "https://staging.example/mcp"
             (get-in (cc/mcp-config profile-export {:env/staging {:server "staging-mcp" :rescope false}} entries)
                     [:mcp-config :mcpServers "staging-qa" "url"]))
          "kept on its own path when asked")
      (is (= [["staging-qa" {:surface/env :env/staging :surface/scope :mcp-scope/qa} "nope"]]
             (:missing (cc/mcp-config profile-export {:env/staging "nope"} entries)))
          "a surface with no server is reported, never dropped"))))
