
[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![release](https://img.shields.io/badge/release-v0.0.3-blue.svg)](https://github.com/contefranz/edgartools/releases/tag/0.0.3)
 [![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://en.wikipedia.org/wiki/GNU_General_Public_License)

# edgartools: Convert SEC Filings Into A Corpus

__edgartools__ is an experimental R package that aims at minimizing the effort of converting
raw JSON files containing SEC filings data into a more manageable corpus of documents within the
[__quanteda__](https://quanteda.io/) framework. The rationale behind this package is to establish a 
robust framework for wrangling SEC filings and preparing them for further analysis through 
Natural Language Processing techniques.


## Core Functions

The set of available functions is subject to change: 

- `get_json_files()`: Convenient function to gather all the JSON files in a given folder and build
a container list with the local pointers.

- `from_json_to_df()`: Efficient function to convert the JSON files into [__data.table__](https://rdatatable.gitlab.io/data.table/) 
structures.

- `create_corpus()`: Workhorse function to create a [__quanteda__](https://quanteda.io/) corpus.

- `reshape_corpus()`: Reshape a [__quanteda__](https://quanteda.io/) corpus in parallel via the 
[__future__](https://future.futureverse.org/index.html) paradigm. 

- `tokenize_corpus()`: Tokenize a [__quanteda__](https://quanteda.io/) corpus in parallel via the 
[__future__](https://future.futureverse.org/index.html) paradigm. 

- `calculate_readability()`: Calculate readability measures with [__quanteda.textstats__](https://github.com/quanteda/quanteda.textstats) in parallel via 
the [__future__](https://future.futureverse.org/index.html) paradigm. 

### Additional Utility Functions

- `get_sec_master_files()`: Convenient function to collect SEC EDGAR master files from local directory. 

## Available Dictionaries

__edgartools__ comes with a set of pre-compiled __quanteda__ dictionaries. They can be used to 
execute bag-of-words analyses on a `tokens` object. They build on the collection of dictionaries 
available with [__quanteda.sentiment__](https://github.com/quanteda/quanteda.sentiment). 
At the moment, they are: 

- Loughran & McDonald firm complexity dictionary.

- Li's forward looking statements dictionary. 

- Bozanic, Roulstone, and VanBuskirk forward looking statements dictionary. 

## Optimal Usage

What follows is a simple example of _good practice_. The script processes one year only on purpose as
this pipeline can easily be inserted in a bigger codebase and vectorized. Or, one can control 
the initial parameter and launch the code via the RStudio Background Jobs tab or with the regular
`rscript` terminal command. 

```r
library(data.table)
library(edgartools)
library(stringr)
library(quanteda)
library(cli)


# SET THE PARAMETERS --------------------------------------------------------------------------

root_path = "edgar-crawler/datasets/"
filing_year = 2007
ncores = 2
corpus_folder = "quanteda_corpus"
tokens_folder = "quanteda_tokens"


# CORPUS CREATION PIPELINE --------------------------------------------------------------------

# 1. Initial collection of all the JSON files processed by extract_filings.py
# This returns a list whose length is equal to the follow up time passed to fyear.
# Each list element contains the pointers to the raw json files as detected in each fyear directory.
cli_h1("Collecting JSON files")
json_container = get_json_files(root_path, pattern = "ITEMS", fyear = filing_year)

# 2. This reads the json files and converts them to data.table. It returns a list.
# Each list element is a data.table containing information about the filing in addition to the raw text.
cli_h1("Converting JSONs to data.table objects")
df_container = from_json_to_df(json_list = json_container, ncores = ncores, bind = TRUE)

# 3. create the corpus
cli_h1("Creating the quanteda corpus")
current_corpus = create_corpus(df_container = df_container)

if ( !dir.exists(corpus_folder) ) {
  dir.create(corpus_folder)
}

cli_h1("Saving the corpus")
fileout = str_c("quanteda_corpus_", filing_year, ".rds")
saveRDS(current_corpus, file = file.path(corpus_folder, fileout))

cli_alert_success("Corpus correctly saved!")

# TOKENIZATION --------------------------------------------------------------------------------

# 4. Tokenize the corpus
toks = tokenize_corpus(x = current_corpus,
                       ncores = 2,
                       remove_separator = FALSE,
                       remove_punct = TRUE,
                       remove_symbols = TRUE,
                       remove_numbers = FALSE)

cli_h1("Saving the corpus")
fileout = str_c("quanteda_tokens_", filing_year, ".rds")
saveRDS(toks, file = file.path(tokens_folder, fileout))

cli_alert_success("Corpus correctly saved!")

# END OF SCRIPT
```

## Authors

* [Francesco Grossetti](https://accounting.unibocconi.eu/people/francesco-grossetti) 

  Assistant Professor of Accounting Analytics and Data Science  
  Bocconi Institute for Data Science and Analytics ([BIDSA](https://www.bidsa.unibocconi.eu/wps/wcm/connect/Site/Bidsa/Home))  
  Accounting Department, Bocconi University.  
  Contact Francesco at: francesco.grossetti@unibocconi.it.  

* [Piergiorgio Di Pasquale](https://www.linkedin.com/in/piergiorgio-di-pasquale-a0059319a/)

  Data Scientist at Accenture  
  Contact Pier at: dipasquale.piergiorgio@gmail.com.  
