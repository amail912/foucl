# Backend Story Writing Guideline

This guideline defines how to write high-quality backend feature backlogs and stories under `docs/features`.

## Purpose
- Keep backend stories consistent across features.
- Make stories implementation-ready without turning them into low-level implementation dumps.
- Ensure behavior, contracts, and operational concerns are explicit before implementation.

## Scope
- This guideline applies to:
  - feature index files under `docs/features/<feature>/`
  - backend story files under status folders such as `to-refine`, `ready`, `done`, `canceled`, or `postponed`
- This guideline complements, but does not replace, deeper technical design docs when required (for example, schema migrations or architecture notes).

## Language
- Write feature backlogs and stories in English.
- Keep wording direct, simple, and specific.
- Prefer concrete business outcomes over vague statements such as "improve reliability".

## Backend-First Rule
- Stories are backend-focused.
- Stories should define domain behavior, invariants, contract impact, and failure handling.
- Stories may reference frontend or external consumers, but the source of truth is backend behavior.
- If a story needs substantial protocol or schema detail, keep it in a dedicated supporting document (for example `api-contract.md` or `data-model.md`) and link it from the feature index.

## Directory Structure And Lifecycle
Use a feature-centric structure under `docs/features`.

```text
docs/features/
  <feature-id>-<feature-name>/
    <feature-name>.md                  # feature index
    api-contract.md                    # optional
    data-model.md                      # optional
    sequence-diagrams/                 # optional
    to-refine/
      001-<story-slug>.md
    ready/
      002-<story-slug>.md
    done/
      003-<story-slug>.md
    canceled/
      004-<story-slug>.md
    postponed/
      005-<story-slug>.md
```

Standard lifecycle is:
- `to-refine` -> `ready` -> `done`
- alternative terminal states: `canceled` or `postponed`

When finishing a documented story, move it to the correct lifecycle folder and update key decisions in the story.

## Numbering Scheme
- Use per-feature 3-digit story IDs: `001`, `002`, `003`, ...
- Filename format: `<id>-<kebab-case-title>.md`.
- Keep IDs stable after assignment, even when a story moves between lifecycle folders.
- If a story is split late, suffix variants are allowed: `004a-...md`, `004b-...md`.
- Do not renumber older stories to "fill gaps".

## Feature Index Format
A backend feature index should:
- start with a short title and one-paragraph summary
- state a clear backend goal and business value
- list active stories with one-line descriptions
- organize stories by status (`To Refine`, `Ready`, `Done`, `Canceled`, `Postponed`)
- link supporting documents such as contracts, schemas, migrations, or diagrams

The feature index should describe backlog state and cross-story context, not duplicate full story content.

## Story Template
Each active backend story should use this structure:

```md
# Story Title

## Goal
As a ..., I want ..., so ...

## Behavior And Business Rules
...

## Data And Contracts
...

## Technical Details
...

## Testing
- ...

## Rollout And Compatibility
...
```

If `Rollout And Compatibility` is not relevant, include `Not applicable`.

## Section Quality Bar
### Goal
- Write the goal from the actor or business perspective.
- Keep it outcome-oriented.
- State why the change matters now.

### Behavior And Business Rules
- Define observable backend behavior.
- Document invariants and decision rules.
- Include edge conditions and error semantics.

### Data And Contracts
- Describe request/response or event contract impact.
- Specify persistence implications (new fields, constraints, indexes, migration touchpoints).
- State compatibility expectations for existing consumers.

### Technical Details
- Give enough implementation direction to avoid ambiguity.
- Mention main modules, boundaries, and integration points.
- Include idempotency, concurrency, authorization, and consistency considerations when relevant.

### Testing
- List precise behavior-driven test scenarios.
- Cover positive, negative, and edge cases.
- Include contract tests when interfaces change.
- Include integration tests for persistence or cross-module interactions.

### Rollout And Compatibility
- Note migration order, deploy sequencing, and fallback expectations when needed.
- State whether the change is backward-compatible, forward-compatible, or breaking.
- Include observability expectations (logs/metrics/alerts) for high-risk paths.

## Naming And Granularity
- Use short, action-oriented titles.
- Split stories when one file mixes multiple independent outcomes.
- Keep each story small enough for one engineer to implement and validate without reinterpreting intent.
- Avoid umbrella stories that combine unrelated domain changes.

## Do And Don't
### Do
- Write "Reject overlapping booking creation with a deterministic validation error code."
- Write "Guarantee idempotent retry behavior for create-payment requests using idempotency keys."
- Write tests like "Integration test for duplicate webhook delivery preserving exactly-once state transition."

### Don't
- Write "Improve backend performance." without measurable criteria.
- Leave contract-impact stories without explicit request/response or event-change notes.
- List generic tests like "Add tests" without scenario details.
- Hide breaking changes inside technical details.

## Consistency Checklist
Before finalizing a backend story set, verify that:
- titles are concise and parallel in tone
- every story follows the same section structure
- lifecycle status is explicit and current
- numbering follows per-feature 3-digit IDs (with suffixes only when justified)
- behavior, contract impact, and failure modes are explicit
- tests are specific to the story behavior
- feature index and story files do not contradict each other
