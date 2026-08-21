(ns atlas.store.github
  "GitHub Store: a version is one commit in a private repository.

  Uses the Git Data API (blob -> tree -> commit -> ref), not the Contents API.
  Two reasons, both load-bearing:

  - **Atomicity.** Contents writes one file per call, so a 20-file version
    would land as 20 commits with no ref meaning \"complete\", and a reader
    could observe a half-written registry.
  - **Size.** Contents base64-encodes into a JSON body and is documented for
    files up to ~1MB. A real registry snapshot measured ~937KB encoded and
    grows with the codebase, so that ceiling is a deadline, not a limit.

  Bootstrap note: the Git Data API returns 409 against a repository with no
  commits. Seed one (a README via the Contents API) before first use."
  (:require
   [atlas.store.protocol :as p]
   [clojure.data.json :as json]
   [clojure.string :as str])
  (:import
   [java.net URI]
   [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers]
   [java.util Base64]))


(def ^:private client (delay (HttpClient/newHttpClient)))

(defn- request!
  [{:keys [token api]} method path body]
  (let [payload (when body (json/write-str body))
        req (cond-> (HttpRequest/newBuilder (URI. (str api path)))
              true    (.header "Authorization" (str "Bearer " token))
              true    (.header "Accept" "application/vnd.github+json")
              true    (.header "X-GitHub-Api-Version" "2022-11-28")
              payload (.header "Content-Type" "application/json")
              true    (.method method (if payload
                                        (HttpRequest$BodyPublishers/ofString payload)
                                        (HttpRequest$BodyPublishers/noBody))))
        resp (.send @client (.build req) (HttpResponse$BodyHandlers/ofString))
        status (.statusCode resp)]
    (when (>= status 300)
      (throw (ex-info (str "GitHub " method " " path " -> " status)
                      {:status status :body (.body resp) :path path})))
    (when (seq (.body resp)) (json/read-str (.body resp) :key-fn keyword))))

(defn- b64   [^String s] (.encodeToString (Base64/getEncoder) (.getBytes s "UTF-8")))
(defn- unb64 [^String s] (String. (.decode (Base64/getDecoder) (str/replace s #"\s" "")) "UTF-8"))


(defrecord GitHubStore [conn repo branch prefix]
  p/Store
  (head [_]
    (try (get-in (request! conn "GET" (str "/repos/" repo "/git/ref/heads/" branch) nil)
                 [:object :sha])
         ;; 404 is the ordinary "no commits yet" case, not an error worth raising.
         (catch clojure.lang.ExceptionInfo e
           (when-not (= 404 (:status (ex-data e))) (throw e)))))

  (read-at [this ref]
    (let [ref (or ref (p/head this))]
      (when ref
        (let [tree-sha (get-in (request! conn "GET" (str "/repos/" repo "/git/commits/" ref) nil)
                               [:tree :sha])
              entries  (->> (request! conn "GET" (str "/repos/" repo "/git/trees/" tree-sha "?recursive=1") nil)
                            :tree
                            (filter #(and (= "blob" (:type %))
                                          (str/starts-with? (:path %) prefix))))]
          (into {} (for [{:keys [path sha]} entries]
                     [(subs path (count prefix))
                      (unb64 (:content (request! conn "GET" (str "/repos/" repo "/git/blobs/" sha) nil)))]))))))

  (write! [this {:keys [message]} files]
    (let [parent (p/head this)
          ;; Everything outside our prefix is carried over verbatim. Building the
          ;; tree without base_tree is what makes deletions inside the prefix
          ;; take effect -- but a bare replacement wipes the repository. The
          ;; first version of this code deleted the README; in a shared store it
          ;; would have deleted every other project's registry.
          keep   (when parent
                   (let [tree-sha (get-in (request! conn "GET" (str "/repos/" repo "/git/commits/" parent) nil)
                                          [:tree :sha])]
                     (->> (request! conn "GET" (str "/repos/" repo "/git/trees/" tree-sha "?recursive=1") nil)
                          :tree
                          (filter #(= "blob" (:type %)))
                          (remove #(str/starts-with? (:path %) prefix))
                          (mapv #(select-keys % [:path :mode :type :sha])))))
          blobs  (doall (for [[path content] files]
                          {:path (str prefix path) :mode "100644" :type "blob"
                           :sha  (:sha (request! conn "POST" (str "/repos/" repo "/git/blobs")
                                                 {:content (b64 content) :encoding "base64"}))}))
          tree   (:sha (request! conn "POST" (str "/repos/" repo "/git/trees")
                                 {:tree (vec (concat keep blobs))}))
          commit (:sha (request! conn "POST" (str "/repos/" repo "/git/commits")
                                 (cond-> {:message message :tree tree}
                                   parent (assoc :parents [parent]))))]
      (if parent
        ;; force=false makes this a compare-and-swap: a concurrent writer that
        ;; moved the branch gets a 422 here rather than silently winning.
        (request! conn "PATCH" (str "/repos/" repo "/git/refs/heads/" branch)
                  {:sha commit :force false})
        (request! conn "POST" (str "/repos/" repo "/git/refs")
                  {:ref (str "refs/heads/" branch) :sha commit}))
      {:ref commit :preserved (count keep)})))


(defn github-store
  "`:repo` \"org/name\", `:token` a PAT or App installation token with
  contents:write, `:prefix` the path this project owns (e.g. \"org/project/\")."
  [{:keys [repo token branch prefix api]
    :or   {branch "main" prefix "" api "https://api.github.com"}}]
  (->GitHubStore {:token token :api api} repo branch prefix))
