
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

``` r
library(namematcher)
js_divergence("Jon Smith", "John Smith")
#> [1] 0.1990067
js_divergence("Jon Smith", "Elizabeth Howell")
#> [1] 0.9016112
```
