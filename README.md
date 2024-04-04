
[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://www.tidyverse.org/lifecycle/#maturing)
 [![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://en.wikipedia.org/wiki/GNU_General_Public_License)

# edgartools: Convert SEC Filings Into A Corpus

__edgartools__ is an experimental R package that aims at minimizing the effort of converting
raw JSON files containing SEC filings data into a more manageable corpus of documents within the
[__quanteda__](https://quanteda.io/) framework. The rationale behind this package is to establish a 
robust framework for wrangling SEC filings and preparing them for further analysis through 
Natural Language Processing techniques.


## Functions

The set of available functions is subject to change: 

- `get_json_files()`: Convenience function to gather all the JSON files in a given folder and build
a container list with the local pointers.

- `from_json_to_df()`: Efficient function to convert the JSON files into [__data.table__](https://rdatatable.gitlab.io/data.table/) 
structures.

## Authors

* [Francesco Grossetti](https://accounting.unibocconi.eu/people/francesco-grossetti) 

  Assistant Professor of Accounting Analytics and Data Science  
  Bocconi Institute for Data Science and Analytics ([BIDSA](https://www.bidsa.unibocconi.eu/wps/wcm/connect/Site/Bidsa/Home))  
  Accounting Department, Bocconi University.  
  Contact Francesco at: francesco.grossetti@unibocconi.it.  

* [Piergiorgio Di Pasquale](https://www.linkedin.com/in/piergiorgio-di-pasquale-a0059319a/)

  Data Scientist at Accenture  
  Contact Pier at: dipasquale.piergiorgio@gmail.com.  
