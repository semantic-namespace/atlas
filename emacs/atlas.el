;;; atlas.el --- Atlas semantic registry IDE integration -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: @tangrammer + LLMs
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1") (cider "1.0.0") (transient "0.3.0"))
;; Keywords: tools, clojure
;; URL: https://github.com/semantic-namespace/atlas

;;; Commentary:

;; Atlas provides IDE integration for exploring semantic registries built
;; with the Atlas library (compound identity system for software architecture).
;;
;; Main features:
;; - Browse entities and aspects
;; - Explore data flow and dependencies
;; - Business semantics mapping
;; - Protocol and component analysis
;; - Advanced architectural analysis
;; - LSP integration for source-level operations
;; - Visual explorer with AND/OR filtering
;;
;; Usage:
;;   M-x atlas (or M-F global binding)
;;
;; Ensure you have:
;; 1. A running CIDER REPL connected to your Clojure project
;; 2. The Atlas library loaded in your project
;; 3. Your registry initialized (e.g., (app/init-registry!))
;;
;; Configuration:
;;   (setq atlas-ide-ns "atlas.ide")              ; IDE namespace
;;   (setq atlas-lsp-helpers-ns "atlas.tooling.lsp-helpers")  ; LSP namespace
;;   (setq atlas-cache-ttl 30)                    ; Cache TTL in seconds

;;; Code:

(require 'transient)
(require 'cider)

;; Load all modules
(require 'atlas-core)
(require 'atlas-display)
(require 'atlas-completion)
(require 'atlas-browse)
(require 'atlas-analysis)
(require 'atlas-business)
(require 'atlas-protocols)
(require 'atlas-lsp)
(require 'atlas-explorer)
(require 'atlas-authoring)
(require 'atlas-interactive)

;;; Explorer Transient Menu

;;;###autoload
(transient-define-prefix atlas-explorer ()
  "Atlas Explorer - Dual map with AND/OR filtering."
  [["Selection"
    ("a" "Browse aspects (toggle)" atlas-explorer-list-aspects)
    ("s" "Select aspect" atlas-explorer-select-aspect)
    ("c" "Clear selection" atlas-explorer--clear-selection)]
   ["Results"
    ("f" "Show filtered entities" atlas-explorer-show-filtered)
    ("e" "Entity info" atlas-browse-entity-info)]
   ["Navigation"
    ("q" "Back to main menu" atlas)]])

;;; Main Menu (Beginner/Daily Use)

;;;###autoload
(transient-define-prefix atlas ()
  "Atlas: Compound Identity Registry Explorer.

This is the main menu for daily use. Press 'z' for advanced features."
  [["Browse"
    ("e" "List entities" atlas-browse-list-entities)
    ("a" "Find by aspect" atlas-browse-find-by-aspect)
    ("A" "List aspects" atlas-browse-list-aspects)
    ("X" "Explorer (v2)" atlas-explorer)]
   ["Entity Details"
    ("i" "Entity info" atlas-browse-entity-info)
    ("D" "Dependencies" atlas-browse-dependencies)
    ("R" "Dependents" atlas-browse-dependents)]
   ["Authoring"
    ("N" "New entity (interactive)" atlas-interactive-author-entity)
    ("+" "Add aspect to set at point" atlas-interactive-add-aspect-to-set)
    ("s" "Aspect stats" atlas-authoring-aspect-stats)
    ("S" "Aspect stats at point" atlas-authoring-aspect-stats-at-point)
    ("I" "Insert aspect" atlas-authoring-insert-aspect)
    ("D" "Insert dev-id" atlas-authoring-insert-dev-id)
    ("P" "Aspect palette" atlas-authoring-aspect-palette)
    ("~" "Similar entities at point" atlas-authoring-similar-at-point)]
   ["Business Semantics"
    ("b" "List business entities" atlas-business-list-entities)
    ("B" "Business info" atlas-business-info)
    ("M" "Business aspects of" atlas-business-aspects-of)]
   ["Protocols"
    ("@" "List protocols" atlas-protocols-list)
    ("#" "Protocol info" atlas-protocols-info)
    ("$" "Component protocols" atlas-protocols-of-component)]
   ["Data Flow"
    ("d" "Data flow trace" atlas-browse-data-flow)
    ("p" "Producers" atlas-browse-producers)
    ("u" "Consumers" atlas-browse-consumers)
    ("x" "Execution order" atlas-browse-execution-order)]
   ["Validation"
    ("c" "Check invariants" atlas-browse-check-invariants)
    ("G" "Refresh cache" atlas--invalidate-cache)
    ("!" "Toggle debug" atlas-toggle-debug)]
   ["Navigation"
    ("z" "Advanced tools →" atlas-advanced)]])

;;; Advanced Menu

;;;###autoload
(transient-define-prefix atlas-advanced ()
  "Advanced analysis and tooling for Atlas."
  [["Architecture Analysis"
    ("t" "By tier" atlas-analysis-by-tier)
    ("V" "Architecture view" atlas-analysis-architecture-view)
    ("O" "Operations view" atlas-analysis-operations-view)
    ("C" "Domain coupling" atlas-analysis-domain-coupling)]
   ["Authoring Tools"
    ("n" "Create new aspect" atlas-authoring-create-aspect)
    ("d" "Create new dev-id" atlas-authoring-create-dev-id)
    ("e" "Scaffold entity" atlas-authoring-scaffold-entity)]
   ["Business Semantics"
    ("N" "Implementations of aspect" atlas-business-implementations)]
   ["Impact & Refactoring"
    ("I" "Impact of change" atlas-analysis-impact-of-change)
    ("Y" "Aspect impact" atlas-analysis-aspect-impact)
    ("r" "Preview refactor" atlas-analysis-preview-refactor)
    ("~" "Similar entities" atlas-analysis-similar-entities)]
   ["Compliance & Quality"
    ("P" "PII surface" atlas-analysis-pii-surface)
    ("E" "Error handler coverage" atlas-analysis-error-handler-coverage)
    ("T" "Trace data flow" atlas-analysis-trace-data-flow)]
   ["Ontology Tools"
    ("S" "Suggest aspects" atlas-analysis-suggest-aspects)
    ("X" "Inspect entity" atlas-analysis-inspect-entity)
    ("K" "Aspect catalog" atlas-analysis-aspect-catalog)
    ("L" "List templates" atlas-analysis-list-templates)]
   ["LSP & Source Code"
    ("h" "Hover at point" atlas-lsp-hover-at-point)
    ("f" "Find usages" atlas-lsp-find-usages)
    ("w" "Export JSON" atlas-lsp-export)
    ("W" "Write search index" atlas-lsp-write-search-index)]
   ["Documentation & Export"
    ("g" "Generate docs" atlas-browse-generate-docs)
    ("l" "LLM context" atlas-analysis-llm-context)
    ("s" "System summary" atlas-browse-system-summary)]]
  [["Navigation"
    ("q" "<< Back to main menu" atlas)]])

;;; Global Keybinding

;;;###autoload
(define-key global-map (kbd "M-F") 'atlas)

(provide 'atlas)
;;; atlas.el ends here
