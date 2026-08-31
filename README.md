
<!-- README.md is generated from README.Rmd. Please edit that file -->

# namematcher

<!-- badges: start -->

<!-- badges: end -->

`namematcher` evaluates whether two names describe the same person or
entity.

## Installation

You can install the development version of namematcher from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("rorygoodwith/namematcher")
```

## Basic Usage

The package currently has one function, `js_divergence`, which measures
how much subcomponents of two names overlap.

``` r
library(namematcher)

# Evalute scalars. Returns 0: no divergence
js_divergence("Jon Smith", "Jon Smith")
#> [1] 0

# Calculates divergence pairwise across vectors
js_divergence(
  c("Jon Smith", "Elizabeth Howell"),
  c("Jon B Smith", "Leanne Holmes")
)
#> [1] 0.1990067 0.9196763
```
