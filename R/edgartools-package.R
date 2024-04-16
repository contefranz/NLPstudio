#' An R package for efficient and high-performance textual data analysis
#'
#' @description edgartools provides a suite of parallel, efficient, and easy-to-use functions 
#' for efficiently managing and analyzing textual data, particularly from financial disclosures. 
#' It simplifies the processes of corpus creation, text tokenization, and data transformation, 
#' leveraging optimized R and C++ code to handle large volumes of data swiftly extending the
#' already powerful \pkg{quanteda}.
#'
#' @details \pkg{edgartools} excels in constructing and manipulating textual data 
#' corpora with high efficiency. Functions such as \code{create_corpus} and 
#' \code{tokenize_corpus} allow users to quickly assemble and tokenize collections of
#' text, supporting various formats and data sources. The package is designed to 
#' optimize performance, handling even large datasets with speed, thanks to its 
#' underlying C++ integration for critical operations and explicit parallelization.
#'
#' @details Features include advanced text processing capabilities like 
#' \code{parse_corpus} for fast and parallel syntactic parsing, and 
#' \code{calculate_readability}, which applies several readability tests to 
#' textual data. The package's functionality extends to sophisticated text 
#' analysis, supporting various NLP tasks that are essential in financial data 
#' analytics.
#'
#' @details \pkg{edgartools} integrates smoothly with other R and C++ libraries, 
#' namely \pkg{data.table}, ensuring that all text handling is both fast and accurate, 
#' with support for Unicode text. Its efficient design is focused on scalability, allowing for 
#' processing of very large text datasets effectively in parallel thanks to the
#' \pkg{future} paradigm.
#'
#' @details The package also includes a range of utility functions like 
#' \code{get_sec_master_files} and \code{get_json_files} designed to streamline the 
#' workflow from data retrieval to analysis of SEC EDGAR filings.
#'
#' @details Additional utilities provide users with the flexibility to customize 
#' analyses, such as the ability to adjust document and feature definitions easily, 
#' and apply complex filtering and transformations based on user-defined criteria.
#' 
#' @seealso [quanteda] [future] [data.table]
#' 
#' @keywords internal
"_PACKAGE"


## usethis namespace: start
## usethis namespace: end
NULL
