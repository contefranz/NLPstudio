
[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![R-CMD-check](https://github.com/contefranz/NLPstudio/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/contefranz/NLPstudio/actions/workflows/R-CMD-check.yaml)
[![release](https://img.shields.io/badge/release-v0.8.0-blue.svg)](https://github.com/contefranz/NLPstudio/releases/tag/v0.8.0)
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
[**seededlda**](https://cran.r-project.org/package=seededlda), with optional
[**topicmodels.etm**](https://cran.r-project.org/package=topicmodels.etm)
support for embedded topic models. The package
standardizes document-topic weights (DTW), topic-word weights (TWW),
representative-candidate extraction, generic topic prediction for new
documents, and downstream visualization across those engines. v0.8.0 adds a
model-evaluation layer — `evaluate_topic_model()` for coherence (UMass, NPMI),
diversity, exclusivity, training NLL/perplexity, and held-out
NLL/perplexity, returned at aggregate level by default — and
`select_k_topics()` for automated grid search over candidate values of K with
an optional holdout split.

Embedded topic models are available through the optional **topicmodels.etm** and
**torch** packages when those backends are installed locally.
When ETM support is available, **NLPstudio** also exposes ETM-specific topic and
term embeddings plus a two-dimensional topic-embedding plot built on the ETM
backend UMAP summary path.

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

Optional ETM backend support requires both **topicmodels.etm** and a working
**torch** backend. On a clean machine, install both optional R packages and
then install the torch backend before fitting ETM models:

```r
install.packages(c("topicmodels.etm", "torch"))
torch::install_torch()
torch::torch_is_installed()
```

---

#### Author

[Francesco Grossetti](https://accounting.unibocconi.eu/people/francesco-grossetti) 

_Assistant Professor of Accounting Analytics and Data Science_  
Department of Accounting, Bocconi University  
Fellow at Bocconi Institute for Data Science and Analytics ([BIDSA](https://www.bidsa.unibocconi.eu/wps/wcm/connect/Site/Bidsa/Home))  
Contact: francesco.grossetti@unibocconi.it
