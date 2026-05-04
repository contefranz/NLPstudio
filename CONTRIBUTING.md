# Contributing to NLPstudio

## Local Test Workflow

Run the package tests before pushing:

```r
testthat::test_local()
```

Run local coverage with the same test scope used by CI:

```r
cov <- covr::package_coverage(type = "tests", quiet = FALSE)
covr::percent_coverage(cov)
```

Coverage should be at least 95% before pushing changes. If coverage drops,
prefer focused tests for public behavior and high-risk internals over broad
tests for cosmetic branches.

For a local package check:

```r
rcmdcheck::rcmdcheck(args = "--no-manual")
```
