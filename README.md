
[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![R-CMD-check](https://github.com/contefranz/NLPstudio/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/contefranz/NLPstudio/actions/workflows/R-CMD-check.yaml)
[![release](https://img.shields.io/badge/release-v0.6.0-blue.svg)](https://github.com/contefranz/NLPstudio/releases/tag/v0.6.0)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://en.wikipedia.org/wiki/GNU_General_Public_License)

# NLPstudio <img src="man/figures/logo.png" align="right" height="139" />

## Overview

**NLPstudio** is an R package that provides a high-performance, research-oriented
framework for large-scale natural language processing (NLP) on text data, 
with particular support for financial disclosures such as SEC EDGAR filings.

Built on the [**quanteda**](https://quanteda.io/) ecosystem and powered by
[**data.table**](https://rdatatable.gitlab.io/data.table/) for efficient memory
management, the package implements multicore parallelism PSOCK (portable, dynamically load-balanced) 
and FORK (Linux/macOS) backends from base R's
[**parallel**](https://stat.ethz.ch/R-manual/R-devel/library/parallel/html/00Index.html)
package.

In addition to preprocessing tasks such as tokenization, singularization,
reshaping, summarization, and similarity computations, **NLPstudio** includes a
unified topic-modeling API spanning [**text2vec**](https://cran.r-project.org/package=text2vec),
[**topicmodels**](https://cran.r-project.org/package=topicmodels), and
[**seededlda**](https://cran.r-project.org/package=seededlda). The package
standardizes document-topic weights (DTW), topic-word weights (TWW),
representative-candidate extraction, and downstream visualization across those
engines.

The package also provides curated **quanteda** dictionaries tailored to
financial and regulatory text, including forward-looking statements, firm
complexity, corporate social responsibility, and sustainable development themes.

Whether analyzing regulatory filings, academic corpora, or policy documents,
**NLPstudio** offers a fast and user-friendly pipeline for researchers in the
social sciences, finance, and accounting domains.

### Installation

You can install **NLPstudio** using either **devtools** or **remotes**:

```r
# with devtools
install.packages("devtools")
devtools::install_github("contefranz/NLPstudio")

# or with remotes (a lighter dependency)
install.packages("remotes")
remotes::install_github("contefranz/NLPstudio")
```

---

#### Author

[Francesco Grossetti](https://accounting.unibocconi.eu/people/francesco-grossetti) 

_Assistant Professor of Accounting Analytics and Data Science_  
Department of Accounting, Bocconi University  
Fellow at Bocconi Institute for Data Science and Analytics ([BIDSA](https://www.bidsa.unibocconi.eu/wps/wcm/connect/Site/Bidsa/Home))  
Contact: francesco.grossetti@unibocconi.it
