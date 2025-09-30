#' @title NLPstudio Package Overview
#' 
#' @description
#' **NLPstudio** is an R package that provides a modular and high-performance framework for 
#' conducting scalable natural language processing (NLP) on structured and unstructured text corpora, 
#' with particular support for financial disclosures such as SEC EDGAR filings.
#' 
#' Built around the **[quanteda](https://quanteda.io/)** ecosystem, the package includes 
#' parallelized workflows for transforming raw text into structured corpora, and supports tasks 
#' such as tokenization, document reshaping, part-of-speech parsing, corpus summarization, 
#' and similarity/distance computations. These features are tightly integrated with 
#' **[data.table](https://rdatatable.gitlab.io/data.table/)** for efficient memory handling 
#' and **[future](https://future.futureverse.org/)** for scalable parallelism across machines and platforms.
#' 
#' In addition to core NLP functionality, **NLPstudio** includes a topic modeling pipeline using 
#' the WarpLDA algorithm implemented in **[text2vec](https://cran.r-project.org/package=text2vec)**. 
#' The package provides utility functions to extract, rank, and visualize topic-word distributions 
#' in both long and wide formats, facilitating model inspection and downstream analysis.
#' 
#' To support domain-specific content analysis, **NLPstudio** also ships with a set of curated, 
#' pre-compiled **[quanteda]** dictionaries tailored to financial and regulatory texts. These 
#' include vocabularies for detecting forward-looking statements, firm complexity, corporate 
#' social responsibility, and sustainable development themes.
#' 
#' @details
#' The core implementation emphasizes:
#' 
#' - **Efficient token manipulation:** Functions such as `tokenize_corpus()`, `singularize_tokens()`, 
#' and `reshape_corpus()` support robust and scalable preprocessing of textual data, using 
#' parallel backends for speed.
#' - **Document-level analysis:** `summarize_corpus()`, `calculate_readability()`, 
#' `calculate_similarity()`, and `calculate_distance()` provide fast and flexible document-level 
#' metrics using `quanteda.textstats`.
#' - **Topic modeling with WarpLDA:** `warpLDA()` interfaces with **[text2vec]** to fit topic models, 
#' while `get_top_terms()` and `plot_top_terms()` support extraction and visualization of model 
#' outputs. The function `plot_dtw()` can be used to inspect the distribution of document-topic 
#' weights across a fitted model.
#' - **Domain dictionaries:** Pre-loaded dictionaries enable lookup-based bag-of-words analysis 
#' tailored to Accounting, Finance, and ESG disclosures.
#' 
#' The package is especially well-suited for large-scale academic, policy, or regulatory text 
#' analysis in the social sciences. While originally designed for SEC filing data, the framework 
#' generalizes well to any structured document corpus within the **[quanteda]** framework.
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
