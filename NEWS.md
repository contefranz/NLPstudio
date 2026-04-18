# NLPstudio News

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