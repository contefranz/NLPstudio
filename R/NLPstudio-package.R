#' @title NLPstudio Package Overview
#' 
#' @description
#' **NLPstudio** is an R package that provides a modular and high-performance
#' framework for conducting scalable natural language processing (NLP) on both
#' structured and unstructured corpora, with particular support for financial
#' disclosures such as SEC EDGAR filings.
#' 
#' Built around the **[quanteda](https://quanteda.io/)** ecosystem,
#' the package includes parallelized workflows for transforming raw text into
#' structured corpora, and supports tasks such as tokenization and singularization, document
#' reshaping, part-of-speech parsing, corpus summarization, and similarity or
#' distance computations. These features are tightly integrated with
#' **[data.table](https://rdatatable.gitlab.io/data.table/)** for efficient
#' memory handling and with **parallel** backends for scalable multicore
#' processing.
#' 
#' In addition to core NLP functionality, **NLPstudio** provides a unified
#' topic-modeling API spanning
#' **[text2vec](https://cran.r-project.org/package=text2vec)**,
#' **[topicmodels](https://cran.r-project.org/package=topicmodels)**, and
#' **[seededlda](https://cran.r-project.org/package=seededlda)**. Utility
#' functions standardize document-topic weights (DTW), topic-word weights
#' (TWW), top-term extraction, representative-candidate retrieval, and
#' visualization across those engines.
#'
#' To support domain-specific content analysis, **NLPstudio** ships with curated,
#' pre-compiled **quanteda** dictionaries tailored to financial and regulatory
#' texts. These include vocabularies for detecting forward-looking statements,
#' firm complexity, corporate social responsibility, and sustainable development
#' themes. Workflows such as raw file discovery, SEC master-file ingestion, and
#' external industry mapping are expected to be handled upstream before data
#' enters the core package pipeline.
#' 
#' @details
#' The core implementation emphasizes:
#' 
#' - **Efficient token manipulation:**  
#'   Functions such as `tokenize_corpus()`, `singularize_tokens()`, and
#'   `reshape_corpus()` support robust preprocessing of text at scale.
#'   All use a consistent interface with `ncores` and `nchunks`, and support
#'   both `"PSOCK"` (dynamic load balancing, portable) and `"FORK"` (fast
#'   forking on Linux/macOS) backends.
#'   
#' - **Document-level analysis:**  
#'   `summarize_corpus()`, `calculate_readability()`, `calculate_similarity()`,
#'   and `calculate_distance()` provide fast document-level metrics and
#'   statistics built on `quanteda.textstats`.
#'   
#' - **Topic modeling:**  
#'   `fit_topic_model()` provides a common fitting interface across
#'   **text2vec**, **topicmodels**, and **seededlda**. Downstream helpers such
#'   as `get_dtw()`, `get_tww()`, `get_top_terms()`, `plot_top_terms()`,
#'   `plot_dtw()`, and `get_representative_candidates()` work with the
#'   standardized DTW/TWW representation regardless of the fitting backend.
#' - **Corpus ingestion:**  
#'   `from_json_to_df()` converts user-supplied SEC-style JSON filings into
#'   tidy data.tables, with chunking controlled by `nchunks` and optional
#'   `max_chunk_size` to prevent memory spikes. File discovery and auxiliary
#'   metadata preparation are intentionally managed outside the package.
#'   
#' - **Domain dictionaries:**  
#'   Pre-loaded dictionaries enable lookup-based bag-of-words analysis tailored
#'   to accounting, finance, and ESG disclosures.
#' 
#' The package is well-suited for large-scale academic, policy, and regulatory
#' text analysis in the social sciences. While originally designed for SEC filing
#' data, the framework generalizes to any structured document corpus within the
#' **quanteda** environment.
#' 
#' 
#' @name NLPstudio-package
#' @aliases NLPstudio
#' @docType package
#' @keywords internal
"_PACKAGE"


## usethis namespace: start
## usethis namespace: end
NULL
