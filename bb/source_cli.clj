(ns source-cli
  "`bb source` in an atlas checkout: atlas.source.cli, which bbin installs as
   the atlas-source command."
  (:require [atlas.source.cli :as cli]))

(def -main cli/-main)
