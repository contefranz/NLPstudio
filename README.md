[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)](https://lifecycle.r-lib.org/)
[![R-CMD-check](https://github.com/contefranz/NLPstudio/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/contefranz/NLPstudio/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/contefranz/NLPstudio/graph/badge.svg?token=P8P9KYGZ5F)](https://app.codecov.io/gh/contefranz/NLPstudio)
[![release](https://img.shields.io/badge/release-v1.0.2-blue.svg)](https://github.com/contefranz/NLPstudio/releases)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://en.wikipedia.org/wiki/GNU_General_Public_License)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.20556350.svg)](https://doi.org/10.5281/zenodo.20556350)

# NLPstudio <img src="man/figures/logo.png" align="right" height="139" />

**NLPstudio** is an R package for scalable text analysis in research workflows.
It is built around **quanteda**, **data.table**, and portable parallel backends,
with particular attention to reproducible social science workflows, including
financial disclosures, regulatory filings, and other structured document
collections.

The package has two main workflows:

- Corpus preparation and document-level text analysis, from SEC-style JSON files
  to `quanteda` corpora, tokens, dictionaries, readability, similarity, and
  export-ready tables.
- A consistent topic-model API for fitting, adopting, evaluating, selecting,
  diagnosing, summarizing, and exporting topic models across supported R
  backends.

The detailed reference manual and vignettes are published at
[contefranz.github.io/NLPstudio](https://contefranz.github.io/NLPstudio/).

## Release Status

NLPstudio is a stable public release intended for reproducible social science
text-analysis workflows, with frozen output schemas for the core corpus and
topic-model APIs. Repository archiving and DOI minting through Zenodo are handled
from each public GitHub release.

The full output-schema contract for the topic-model API — the frozen result
classes and the standardized evaluation/selection columns — is documented in the
[Topic Model API](https://contefranz.github.io/NLPstudio/articles/topic-model-api.html)
vignette under *Public API Stability*.

## Installation

Install NLPstudio from GitHub with **pak**:

```r
install.packages("pak")
pak::pkg_install("contefranz/NLPstudio")
```

Some modeling backends are optional. Install backend packages only when you need
them; for example, STM support requires **stm**, and embedded topic models
require both **topicmodels.etm** and a working **torch** backend.

## Quick Example

```r
library(NLPstudio)
library(quanteda)

docs <- data.frame(
  doc_id = paste0("doc", 1:6),
  text = c(
    "Revenue growth improved after subscription demand increased.",
    "Operating margin expanded as cloud costs declined.",
    "Audit committee oversight focused on internal controls.",
    "Risk disclosures emphasized liquidity and refinancing pressure.",
    "Customer retention supported recurring software revenue.",
    "Debt covenants and interest expense shaped capital allocation."
  )
)

corp <- quanteda::corpus(docs, text_field = "text", docid_field = "doc_id")
toks <- quanteda::tokens(corp, remove_punct = TRUE)
toks <- quanteda::tokens_tolower(toks)
toks <- quanteda::tokens_remove(toks, pattern = quanteda::stopwords("en"))
dfm <- quanteda::dfm(toks)

fit <- fit_topic_model(
  dfm,
  engine = "topicmodels",
  model = "lda",
  method = "Gibbs",
  k = 2,
  control = list(fit = list(seed = 1L, iter = 50L, burnin = 0L, thin = 1L))
)

get_top_terms(fit, n = 4)
evaluate_topic_model(
  fit,
  training = dfm,
  metrics = c("diversity", "exclusivity", "coherence_umass"),
  top_n = 4L
)
```

For complete workflows, see:

- [Corpus Preparation and Text Analysis](https://contefranz.github.io/NLPstudio/articles/corpus-workflow.html)
- [Topic Model API and Usage](https://contefranz.github.io/NLPstudio/articles/topic-model-api.html)

## Citation

If you use **NLPstudio** in academic work, please cite the package. Citation
metadata is available from R:

```r
citation("NLPstudio")
```

## Author

[Francesco Grossetti](https://accounting.unibocconi.eu/faculty/francesco-grossetti)<br>
Assistant Professor of Accounting Analytics and Data Science<br>
Department of Accounting, Bocconi University<br>
Fellow at Bocconi Institute for Data Science and Analytics ([BIDSA](https://bidsa.unibocconi.eu/))<br>
Contact: francesco.grossetti@unibocconi.it
