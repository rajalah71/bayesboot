README
================
2023-12-23

This package provides a function to perform Bayesian bootstrap for a
given statistic. The function is based on the algorithm described by
Rubin (1981), <https://www.jstor.org/stable/2240875>.

## 1. Installation

    devtools::install_github("rajalah71/bayesboot")

``` r
library(bayesboot)
#par(mfrow = c(2,1))
```

## 2. Examples

``` r
# Integer data
data = c(1,2,2,4,9)
boot_means = bayesboot(data, mean, b = 10000, plot = TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
# Binary data
data = c(0,1,1,0,1,1,1,0,1,1,1,1,1,1,1,1,1,1,0,1)
boot_means = bayesboot(data, f = mean, b = 10000, plot = TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->