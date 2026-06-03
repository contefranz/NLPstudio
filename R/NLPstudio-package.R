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
#' **[seededlda](https://cran.r-project.org/package=seededlda)**, with
#' optional **[stm](https://cran.r-project.org/package=stm)** support for
#' structural topic models with prevalence covariates and
#' optional **[topicmodels.etm](https://cran.r-project.org/package=topicmodels.etm)**
#' support for embedded topic models. Utility
#' functions standardize document-topic weights (DTW), topic-word weights
#' (TWW), generic topic prediction for new documents, top-term extraction,
#' representative-candidate retrieval, interpretation tables, and visualization
#' across those engines.
#' STM-specific helpers expose STM-native topic labels and prevalence-effect
#' reports while preserving the standardized NLPstudio topic identifiers.
#' When **topicmodels.etm** is available, the package also exposes ETM-specific
#' topic and term embeddings plus a dedicated topic-embedding plot.
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
#'   **text2vec**, **topicmodels**, **seededlda**, and optional
#'   **stm** and **topicmodels.etm**. Downstream helpers such as `get_dtw()`, `get_tww()`,
#'   `predict_topic_model()`, `get_top_terms()`, `plot_top_terms()`,
#'   `plot_dtw()`, `summarize_topics()`, and
#'   `get_representative_candidates()` work with the standardized DTW/TWW
#'   representation regardless of the fitting backend.
#'   Model-selection helpers evaluate candidate topic counts, summarize
#'   selection evidence, assess seed stability, and prepare compatible inputs for
#'   external OpTop workflows.
#'   STM-specific interpretation helpers include `get_stm_topic_labels()`,
#'   `summarize_stm_topics()`, and `estimate_stm_topic_effects()`.
#'   For embedded topic models, `get_topic_embeddings()`,
#'   `get_term_embeddings()`, and `plot_topic_embeddings()` expose the
#'   embedding-space structure that is specific to ETM.
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
