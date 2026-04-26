# NLPstudio News

## NLPstudio 0.8.0  (2026-04-26)

### NEW FEATURES

1. Added `evaluate_topic_model()`, a unified quality-evaluation interface for
   any `nlp_topic_fit` object regardless of backend engine. Metrics covering
   intrinsic corpus statistics, topic structure, and held-out predictive
   performance are computed through a single call and returned in a
   standardized long-format `data.table` (`metric`, `scope`, `topic_id`,
   `value`, `supported`). When the data required for a metric is absent the
   result is marked `supported = FALSE` rather than silently producing invalid
   values.

2. Added `select_k_topics()`, a k-grid search helper that fits and evaluates a
   topic model for each value in `k_grid`, with an optional document-level
   holdout split and PSOCK parallel execution. Returns an `nlp_k_selection`
   object with `print` and `plot` S3 methods (faceted line chart, one panel per
   metric).

## NLPstudio 0.7.0  (2026-04-24)

### NEW FEATURES

1. Added `predict_topic_model()`, a generic post-fit prediction interface that
   aligns new data to the fitted vocabulary and returns standardized DTW tables
   across **text2vec**, **topicmodels**, **seededlda**, and
   **topicmodels.etm**.

2. Extended the `nlp_topic_fit` object contract with a stored `vocab` field so
   prediction and downstream helpers no longer depend on cached TWW to recover
   fitted term order.

3. Added `get_topic_embeddings()` and `get_term_embeddings()` for
   **topicmodels.etm**, exposing ETM topic-center and term embeddings in a
   standardized `data.table` format.

4. Added `plot_topic_embeddings()`, an ETM-specific visualization that uses the
   backend UMAP summary path to display topic centers and their top associated
   words in two dimensions.

5. Post-fit document-level topic-model helpers now omit docvars by default.
   Use `docvars = TRUE` in `get_dtw()`, `get_representative_candidates()`, or
   `predict_topic_model()` when enriched outputs should include available
   document variables. Existing non-topic metadata columns in standardized DTW
   table inputs are also retained only when `docvars = TRUE`.
   `get_representative_candidates()` also omits columns matching stored docvar
   names when `docvars = FALSE`, even if those names arrive through `doc_data`.

6. Document-level topic-model outputs now use a stable column order:
   `doc_id`, document metadata, function output columns, and optional `text`
   as the final column.

7. Document-level topic-model outputs now include `topic_max_int`, the integer
   topic number corresponding to `topic_max_id`.

### CHANGES

1. `stringr` has been removed from `Imports`. The three internal call sites in
   `define_corpus()` and `singularize_tokens()` have been rewritten in base R
   (`sub()`, `paste()`, `grepl()`), trimming a transitive dependency chain
   without changing behaviour. The startup message has been updated
   accordingly.

2. `text2vec` has been moved from `Imports` to `Suggests`, bringing it in line
   with the other topic-model backends (`topicmodels`, `seededlda`,
   `topicmodels.etm`). `fit_topic_model(engine = "text2vec")` now emits an
   informative error if `text2vec` is not installed. Users who rely on the
   text2vec engine should install it explicitly:
   `install.packages("text2vec")`. The startup message now lists `text2vec`
   under optional backends.

## NLPstudio 0.6.1  (2026-04-23)

### CHANGES

1. `fit_topic_model()` now uses a single
   `control = list(model = ..., fit = ..., optimizer = ...)` argument instead
   of separate `model_control` and `fit_control` inputs.

2. The returned `nlp_topic_fit` object now stores compact `docvars`,
   optional `doc_data`, fitted `doc_ids`, and matrix-backed DTW/TWW caches
   instead of retaining the raw modeling input.

3. `get_dtw()` and `get_representative_candidates()` now align post-fit
   outputs through fitted `doc_id` values, auto-join stored docvars, and use
   `doc_data` only for explicit metadata or text enrichment.

4. `print.nlp_topic_fit()` now prints a compact summary so large topic-model
   fits can be inspected at the console without expanding huge internals.

5. `warp_lda()` has been removed from the package surface. Text2vec support is
   now available only through `fit_topic_model(engine = "text2vec", model = "lda")`.

6. `fit_topic_model()` now supports embedded topic models via
   `engine = "topicmodels.etm", model = "etm"`, with ETM controls routed
   through `control$model`, `control$fit`, and `control$optimizer`.

## NLPstudio 0.6.0  (2026-04-22)

### BREAKING CHANGES

1. `warpLDA()` has been removed from the public API.

### NEW FEATURES

1. Added `fit_topic_model()`, a unified topic-model fitting interface across
   **text2vec**, **topicmodels**, and **seededlda**.

2. Added `get_dtw()` and `get_tww()` to standardize document-topic weights
   (DTW) and topic-word weights (TWW) using the `Topic###` naming convention.

3. Added `get_representative_candidates()` to extract dominant-topic
   candidates and band them within topic using quantile or deterministic
   rank-based fallback rules.

### CHANGES

1. `get_top_terms()` and `plot_dtw()` now route through the standardized
   DTW/TWW extractor layer instead of backend-specific logic.

2. Text2vec topic modeling is routed through `fit_topic_model()` using
   `engine = "text2vec", model = "lda"`.

3. Package documentation now uses DTW/TWW terminology following Lewis and
   Grossetti (2022) and documents the returned `nlp_topic_fit` S3 wrapper.

## NLPstudio 0.5.1  (2026-04-22)

### NOTES

1. Added guarded copy-paste examples for the remaining exported, supported
   functions that previously lacked them. The new examples are written with
   `@examplesIf interactive()` so they document intended usage without being
   executed during package checks.

2. The examples release intentionally excludes deprecated
   `set_ff_industries()` and does not revisit APIs removed in v0.5.0.

## NLPstudio 0.5.0  (2026-04-21)

### BREAKING CHANGES

1. `get_json_files()` has been removed. Users should now discover JSON inputs
   directly with `list.files(..., pattern = "\\.json$", recursive = TRUE,
   full.names = TRUE)` and pass the resulting character vector to
   `from_json_to_df()`.

2. `get_sec_master_files()` has been removed. SEC master-file ingestion is now
   considered outside the current `NLPstudio` scope and should be handled
   upstream before the data enters the package workflow.

### DEPRECATIONS

1. `set_ff_industries()` is now soft-deprecated. The function remains exported
   and functional in v0.5.0, but it emits a deprecation warning and is planned
   for removal in a future release. Fama-French industry mapping is now treated
   as an upstream preprocessing step rather than part of the core package API.

### NOTES

1. This release intentionally does not include the examples expansion planned
   for a follow-up documentation-focused release.

## NLPstudio 0.4.1  (2026-04-21)

### BREAKING CHANGES

1. `library(NLPstudio)` no longer attaches `quanteda`, `quanteda.textstats`,
   `data.table`, `text2vec`, or `stringr` to the search path. Those packages
   remain in `Imports` and are fully available inside the package, but users
   who relied on the implicit attachment for their own code will need to add
   explicit `library()` calls. A startup message now states the version and
   lists the required packages.

   **Why this changed.** The previous behaviour followed the meta-package
   pattern popularised by the tidyverse: loading one package silently attaches
   several others. This is convenient at the console but has meaningful costs
   when NLPstudio is used as a library dependency rather than an interactive
   toolkit:

   - **Search-path pollution.** Every attached package adds a frame to the
     search path. Name collisions become more likely as the path grows — for
     instance, `data.table::between()` and `dplyr::between()` resolve
     differently depending on attachment order, producing bugs that are
     hard to trace.

   - **Opacity for downstream packages.** A package that `Imports` NLPstudio
     unintentionally acquires five additional namespaces on the search path,
     which can mask functions in its own dependencies without any explicit
     declaration in its `DESCRIPTION`.

   - **Redundancy.** Since v0.3.3 every call inside NLPstudio uses fully
     qualified `pkg::function()` notation. The package does not need any of
     these namespaces *attached* in order to work; it only needs them
     *loaded*, which `Imports` already guarantees.

   Users who want the packages attached for interactive work can add
   `library(quanteda); library(data.table)` etc. to their own scripts or
   `.Rprofile`. Nothing changes for code that already calls those packages
   explicitly.

### NEW FEATURES

1. `define_corpus()` gains a `default` S3 method that produces an informative
   error when the input is not a `data.table`, replacing the opaque
   "no applicable method" dispatch failure.

### BUG FIXES

1. GitHub Actions `R-CMD-check` now passes reliably across the supported CI
   environments. Internal PSOCK execution now falls back to sequential
   processing when a worker socket cannot be created, which avoids
   environment-specific failures without changing the public API.

2. Optional helper packages used only inside specific functions are no longer
   installed during CI. In particular, `pluralize` and `farr` have been
   removed from `Suggests`, while `singularize_tokens()` and
   `set_ff_industries()` continue to emit explicit runtime errors when those
   packages are not installed by the user.

### NOTES

1. **Golden tests** added for all parallel functions: `tokenize_corpus()`,
   `calculate_readability()`, `summarize_corpus()`, and `reshape_corpus()` now
   each include a test asserting that `ncores = 2` produces numerically
   identical output to `ncores = 1`. The class of silent parallelization bug
   that affected `calculate_similarity()` in v0.2.x would be caught
   immediately across any of these functions.

2. **Contract tests** added for `define_corpus()` (missing columns individually
   and in combination, non-`data.table` input, duplicate doc-ID warning, no
   temp-column leakage into the input `data.table`) and `warp_lda()` (argument
   routing via positive contracts: valid `fit_control` args, valid `lda_control`
   args, `k` not overridable via `lda_control`, `return_theta`/`return_phi`
   flags).

3. Test count: 96 (up from 66 in v0.3.x).

4. The package now includes a standard GitHub Actions `R-CMD-check` workflow
   and matching README badge.

5. Roxygen comments were normalized toward Markdown-style notation and the
   generated documentation was refreshed. Mathematical notation remains in Rd
   form where appropriate (for example `\eqn{}`).


## NLPstudio 0.3.3  (2026-04-18)

### NOTES
  
  1. Every external function call is now fully namespace-qualified (`pkg::function()`) throughout all source files. No bare unqualified calls remain for any imported package. This makes dependency resolution unambiguous and removes the need for `@importFrom` roxygen tags.

2. All `@importFrom` tags have been removed from every `.R` file. The only whole-package imports that remain are `@import data.table` (required for the `:=` and `.()` special syntax) and `@import ggplot2` (required for `+` operator dispatch on ggplot objects). The generated `NAMESPACE` is correspondingly minimal.

3. `parallel` has been removed from `Imports` in `DESCRIPTION`. `parallel` is a base R package that ships with every R installation; declaring it in `Imports` alongside `R (>= 4.3)` was redundant.

---
  
## NLPstudio 0.3.2  (2026-04-17)

### BUG FIXES
  
1. `calculate_similarity()` / `calculate_distance()`: `quanteda_options("threads")` returns a scalar, not a named list — accessing it as `$threads` raised an error on every call.

2. `warp_lda()`: constructor args and fitting args were both routed through `...`, causing "unused argument" errors. Replaced with `lda_control` and `fit_control` named lists.

3. `define_corpus()`: `item` was used without being validated, causing cryptic downstream errors when the column was absent.

4. `calculate_readability()`: bare `is.corpus()` / `corpus()` calls relied on search-path attachment not guaranteed inside a package namespace.

5. `from_json_to_df()`: `setcolorder(..., after = "filing_type")` would error when `filing_type` was absent.

### NEW FEATURES

1. `from_json_to_df()`: `max_chunk_size` promoted from a hidden `...` argument to a proper named parameter.

### NOTES

1. Dead code removed: `%||%` operator and `is.textstat_simil_symm()` from `R/utils.R`.
2. Stale `globalVariables("doc_id")` removed from `R/tokenize_corpus.R`.
3. `cli_h2()` and `cli_alert_success()` added to sequential paths of `tokenize_corpus()`, `summarize_corpus()`, `lookup_tokens()`, `reshape_corpus()`.

---
  
## NLPstudio 0.3.1  (2026-04-16)

### BUG FIXES
  
1. `summarize_corpus()`: sequential path returned column `document` while parallel path returned `doc_id`. Both paths now rename consistently.

---
  
## NLPstudio 0.3.0  (2026-04-15)

### NEW FEATURES
  
1. **Unified parallel backend.** `.run_parallel()` and `.validate_parallel_args()` (in `R/utils.R`) encapsulate all PSOCK/FORK branching, eliminating ~120 lines of duplicated boilerplate across every parallel function.

2. `calculate_similarity()` / `calculate_distance()` rewritten. The previous row-split approach produced a block-diagonal result (cross-chunk pairs were never evaluated). Replaced with quanteda's built-in OpenMP threading via `quanteda_options(threads = ncores)`.

3. Sequential fast paths added to all parallel functions — cluster creation is bypassed entirely when `ncores < 2`.

4. **Testing infrastructure** added (`tests/testthat/`, 3rd edition, 66 tests).

5. `warp_lda()` (snake_case) introduced as canonical name; `warpLDA()` retained as a deprecated alias.

### BUG FIXES

1. `calculate_similarity()` / `calculate_distance()`: `temp_matrix` undefined when `y` provided.
2. `get_sec_master_files()`: `uniqueN()` called on a list instead of the bound data.table.
3. `parse_corpus()`: `on.exit(spacy_finalize)` registered too late — moved to immediately after acquiring the function reference.

### NOTES

1. **Breaking:** `future` and `future.apply` removed from `Imports`; `parallel` (base R) used instead.
2. `glue` removed from `Imports`.
3. `warpLDA()` deprecated; will be removed in a future release.

---

## NLPstudio 0.2.0  (2025-10-01)

### NOTES

1. Dependency overhaul: `topicmodels` moved to `Suggests`; `Imports` entries sorted alphabetically.
2. Package logo updated.

---

## NLPstudio 0.1.5  (2025-09-30)

### NEW FEATURES

1. `from_json_to_df()` refactored with internal helpers; JSON parsing switched from `jsonlite` to `RcppSimdJson::fload()`.
2. Dynamic PSOCK scheduling via `clusterApplyLB()` across all four corpus-parallel functions.

### NOTES

1. `foreach` removed from `Imports`.

---

## NLPstudio 0.1.0  (2025-07-31)

### NEW FEATURES

1. `get_top_terms()` — extracts top-_n_ terms from φ in long or wide format.
2. `plot_top_terms()` — faceted bar chart of per-topic top terms.

---

## NLPstudio 0.0.7  (2025-07-29)

### NEW FEATURES

1. `warpLDA()` — WarpLDA topic model via **text2vec**; returns θ, φ, and the model object.
2. `plot_dtw()` — faceted histogram of document-topic weight distributions.

---

## NLPstudio 0.0.6  (2025-02-23)

### NEW FEATURES

1. `get_sec_master_files()` — reads and normalises SEC EDGAR master CSV files.

### NOTES

1. Documentation switched to roxygen2 Markdown rendering.

---

## NLPstudio 0.0.5  (2024-04-19)

### NEW FEATURES

1. `summarize_corpus()` — parallel corpus summarisation via `textstat_summary()`.

---

## NLPstudio 0.0.4  (2024-04-18)

### NEW FEATURES

1. `singularize_tokens()` — parallel plural-to-singular token conversion via **pluralize**.
2. Package hex logo added.

---

## NLPstudio 0.0.3  (2024-04-13)

### NOTES

1. Structured console output via **cli** added across all functions.

---

## NLPstudio 0.0.2  (2024-04-09)

### NOTES

1. Minimum **quanteda** version raised to `>= 4.0.1`.

---

## NLPstudio 0.0.1  (2024-04-08)

### NEW FEATURES

First public release. Core functions: `from_json_to_df()`, `define_corpus()`, `tokenize_corpus()`, `reshape_corpus()`, `lookup_tokens()`, `parse_corpus()`, `calculate_readability()`, `calculate_similarity()`, `calculate_distance()`, `set_ff_industries()`, `get_json_files()` (deprecated v0.1.3). Bundled financial text dictionaries.

---
