# SEC-Style JSON Examples

This directory contains five small public 10-K JSON filings used by the
package vignettes and examples. They follow the SEC-style structure produced by
the upstream filing crawler: one top-level JSON object per filing, filing
metadata as scalar fields, and extracted 10-K sections as top-level `item_*`
fields.

The files are intentionally stored under `inst/extdata/json/` so installed
packages can locate them with:

```r
system.file("extdata", "json", package = "NLPstudio")
```

They are example inputs for `from_json_to_df()` and are not package datasets
loaded by `data()`.
