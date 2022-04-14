
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hcup- Implementation of HCUP Software Tools in R

<!-- badges: start -->

<!-- badges: end -->

Adapts the software provided by the [Healthcare Cost and Utilization
Project (HCUP)](https://www.hcup-us.ahrq.gov/tools_software.jsp) for
analyses of administrative database using International Classification
of Diseases (ICD) codes within the R statistical environment.

## Installation

You can install the development version of hcup from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("HunterRatliff1/hcup")
```

## Classify ICD-10 codes to CCSR categories

The simple case is for procedures, as each procedure has one CCSR
category

``` r
library(hcup)
classify_ccsr_pr("0016070")
#> [1] "CNS010"

icd_10_pr <- c("04L04DZ", "009Y40Z", "0F7C7DZ", "GZ2ZZZZ")
classify_ccsr_pr(icd_10_pr)
#> [1] "CAR010" "CNS003" "HEP008" "MHT004"

# Vectorized using `mutate()`
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

dplyr::tibble(icd_10 = icd_10_pr) %>%
  mutate(CCSR = classify_ccsr_pr(icd_10),
         explained = explain_ccsr(CCSR))
#> # A tibble: 4 × 3
#>   icd_10  CCSR   explained                                                      
#>   <chr>   <chr>  <chr>                                                          
#> 1 04L04DZ CAR010 Ligation and embolization of vessels                           
#> 2 009Y40Z CNS003 Spinal canal and spinal cord drainage (excluding lumbar punctu…
#> 3 0F7C7DZ HEP008 Common bile duct sphincterotomy and stenting                   
#> 4 GZ2ZZZZ MHT004 Crisis intervention for mental health
```

CCSR diagnosis codes…
