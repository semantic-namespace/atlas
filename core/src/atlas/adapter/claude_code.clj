(ns atlas.adapter.claude-code
  "EXPERIMENTAL: emit a Claude Code session's settings and MCP config from an
   :atlas/llm-profile export (atlas.ontology.llm-profile/export).

   Pure data in, data out: the caller reads ~/.claude.json for the server
   entries it already has, and writes the JSON. For each of the profile's
   surfaces ({:surface/env :surface/scope}) the session gets one MCP server:
   the user's server for that environment, its URL's path set to the
   surface's partition (/mcp -> /mcp/qa), so the server serves only that
   partition; a server whose partition path can't be used is kept on its
   own path with {:server name :rescope false}. Every tool the export lists
   gets its exact rule, mcp__<server>__<tool> under allow, ask or deny; the
   profile's command patterns become Bash rules, its hosts the sandbox's
   network allowlist.

   What an agent's session also needs and a registry doesn't know (a hook,
   files never to read, the sandbox's filesystem) the caller merges in."
  (:require [clojure.string :as str]))

(defn server-name
  "The session's name for a surface's server: <env>-<scope>."
  [{:surface/keys [env scope]}]
  (str (name env) "-" (name scope)))

(defn scoped-path
  "The URL path for a scope: /mcp for :all, /mcp/<scope> otherwise."
  [scope]
  (let [s (name scope)] (if (= "all" s) "/mcp" (str "/mcp/" s))))

(defn- rescope-url [url scope]
  (str/replace url #"(https?://[^/\s]+)/mcp(/[a-z-]+)?(?=$|[?#])" (str "$1" (scoped-path scope))))

(defn rescope
  "A server entry (as ~/.claude.json holds it) pointed at scope: its url, or
   the MCP_SERVER_URL a bridge is given, its path set to the scope's. Nil
   when it names no URL to rescope."
  [entry scope]
  (cond
    (get entry "url") (update entry "url" rescope-url scope)
    (some #(str/starts-with? % "MCP_SERVER_URL=") (get entry "args"))
    (update entry "args" (fn [args] (mapv #(if (str/starts-with? % "MCP_SERVER_URL=")
                                             (str "MCP_SERVER_URL=" (rescope-url (subs % 15) scope))
                                             %)
                                          args)))))

(defn permissions
  "Claude Code permission rules for a profile's export: each tool's exact
   rule per surface server, and the command patterns as Bash rules."
  [{:keys [surfaces commands]}]
  (let [rules (fn [d] (vec (sort (for [s surfaces [tool decision] (:tools s) :when (= d decision)]
                                   (str "mcp__" (server-name s) "__" tool)))))
        bash #(mapv (fn [p] (str "Bash(" p ")")) %)]
    {:allow (rules :allow)
     :ask (vec (concat (rules :ask) (bash (:ask commands))))
     :deny (vec (concat (rules :deny) (bash (:deny commands))))}))

(defn settings
  "The session's settings for a profile's export: permissions, and the
   sandbox's network allowlist from the profile's hosts."
  [profile-export]
  {:permissions (permissions profile-export)
   :sandbox {:network {:allowedDomains (vec (sort (:network profile-export)))}}})

(defn mcp-config
  "The session's MCP servers for a profile's export: for each surface, the
   entry of the server env->server names for its environment, from entries
   (name -> entry), rescoped unless {:rescope false}. Returns {:mcp-config
   {:mcpServers ...} :missing [[server-name surface named-server]]}."
  [{:keys [surfaces]} env->server entries]
  (let [servers (for [s surfaces
                      :let [spec (get env->server (:surface/env s))
                            {from :server rescope? :rescope :or {rescope? true}} (if (map? spec) spec {:server spec})
                            e (some-> (get entries from) (cond-> rescope? (rescope (:surface/scope s))))]]
                  [(server-name s) (dissoc s :tools) from e])]
    {:mcp-config {:mcpServers (into (sorted-map) (for [[n _ _ e] servers :when e] [n e]))}
     :missing (vec (for [[n s from e] servers :when (nil? e)] [n s from]))}))
