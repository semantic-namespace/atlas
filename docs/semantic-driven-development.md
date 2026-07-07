# Semantic Driven Development with Atlas

## What is SDD?

Semantic Driven Development is an approach where architectural intent — what entities mean and how they relate — is captured as a live, queryable layer alongside the codebase. Development decisions are driven by semantic declarations rather than file structure, naming conventions, or runtime behavior.

## Where Atlas fits

Existing approaches to semantics in software fall into a few families:

| Approach | Primary artifact | Relationship to code |
|---|---|---|
| Semantics-driven DSL design | Semantic domain model | Code is derived from the model |
| Model-driven development (MDD) | Ontologies + UML | Code is generated from models |
| Spec-driven development | Schemas, contracts | Code is validated against specs |
| **Atlas / SDD** | **Compound identities in a live registry** | **Code is annotated with meaning, in the same runtime** |

Atlas doesn't replace or generate code. It runs alongside it — a semantic overlay on a living codebase. The registry is an atom. Invariants check actual state. History tracks how meaning shifts across releases.

## The compound identity difference

A compound identity like `#{:domain/auth :tier/service :effect/write}` is not a type, not a spec, not an ontology class. It is a **semantic coordinate** — simultaneously queryable, composable, and diffable.

This makes possible things that none of the other approaches support directly:

- **Semantic diffing**: "Between v1.1.0 and v1.2.0, which auth entities lost dependencies?" — not a code diff, a meaning diff.
- **Multi-dimensional querying**: "Show me everything that is both `:domain/payment` and `:effect/write`" — cuts across files, namespaces, modules.
- **Vocabulary evolution**: tracking which aspects appear, grow, shrink, or retire over time — the language your architecture speaks is itself observable.

## Three levels of semantic completeness

Given the registry as ground truth, completeness can be checked at increasing levels of sophistication:

**1. Structural completeness** — deterministic invariants.
"Every execution-function with `:effect/write` must have `:audit/enabled`."
Expressible today with `dsl-axiom` / `fn-axiom`. Runs in CI.

**2. Vocabulary completeness** — asymmetric evolution detection.
"We added 5 new endpoints but no data-schemas for them."
Detectable from history diffs: vocabulary growth in one entity type without corresponding growth in another signals a gap.

**3. Semantic completeness** — LLM-assisted gap analysis.
"You have 12 `domain/payment` entities but none handle refund failure."
No single invariant catches this. It requires reasoning over the full registry with domain understanding — exactly what LLMs are good at when given rich, structured context. Compound identities provide that context.

## Why this matters

Most architectural knowledge lives in people's heads or in stale documentation. Atlas makes it live, queryable, and diffable. SDD means:

- **Onboarding**: ask the registry what exists, not a person
- **Review**: check semantic impact of changes, not just code impact
- **Evolution**: track how architectural intent shifts across releases, not just which files changed
- **Completeness**: surface what's missing, not just what's broken
