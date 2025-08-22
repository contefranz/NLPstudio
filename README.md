
[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![release](https://img.shields.io/badge/release-v0.1.2-blue.svg)](https://github.com/contefranz/edgartools/releases/tag/0.1.2)
 [![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://en.wikipedia.org/wiki/GNU_General_Public_License)

# NLPstudio <img src="man/figures/logo.svg" align="right" height="139" />

## Overview

**NLPstudio** is an R package for scalable, parallelized natural language processing, designed to streamline the transformation of raw text—especially financial disclosures like SEC EDGAR filings—into structured, analyzable corpora. Built around the [**quanteda**](https://quanteda.io/) framework and powered by [**data.table**](https://rdatatable.gitlab.io/data.table/) and [**future**](https://future.futureverse.org/index.html), the package offers fast and intuitive functions for corpus creation, tokenization, text reshaping, summarization, and linguistic parsing.

In addition to high-performance core NLP tasks, **NLPstudio** includes integrated tools for computing readability indices, document similarity and distance measures, and performing topic modeling with WarpLDA via [**text2vec**](https://cran.r-project.org/package=text2vec). Custom utilities allow users to extract and visualize top terms per topic in both long and wide formats, making it easy to interpret and export model outputs.

To support dictionary-based bag-of-words analyses, **NLPstudio** also includes a curated set of pre-compiled domain-specific dictionaries, particularly suited to financial and regulatory texts (e.g., forward-looking statements, firm complexity, CSR disclosures, and SDG alignment).

Whether analyzing regulatory filings, research corpora, or policy documents, **NLPstudio** provides a modular and user-friendly pipeline for researchers working at scale—particularly in the social sciences, finance, and accounting domains.


#### Author

[Francesco Grossetti](https://accounting.unibocconi.eu/people/francesco-grossetti) 

_Assistant Professor of Accounting Analytics and Data Science_  
Bocconi Institute for Data Science and Analytics ([BIDSA](https://www.bidsa.unibocconi.eu/wps/wcm/connect/Site/Bidsa/Home))  
Accounting Department, Bocconi University.  
Contact Francesco at: francesco.grossetti@unibocconi.it.  
