
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

## Procedures with ICD-10

Although I’m not a fan of procedures myself (clinical practice), I am
aficionado of ICD-10 procedure codes (ICD-10-PCS). Compared to their
predecessor (ICD-9), ICD-10-PCS is much more eloquent and specific, so
it’s only appropriate that we start there\!

### Procedure classification

The most straightforward of the HCUP software tools included in this
package are the [Procedure
Classes](https://www.hcup-us.ahrq.gov/toolssoftware/procedureicd10/procedure_icd10.jsp).
Procedure classes identify procedures as \[1\] diagnostic or therapeutic
and \[2\] whether they major (expected to be performed in an operating
room) or minor.

``` r
library(hcup)
classify_pr("0PS443Z") # Spine surgery
#> [1] "Major Therapeutic"
classify_pr("009U3ZX") # Lumbar puncture
#> [1] "Minor Diagnostic"
```

This function can be vectorized (e.g. using `dplyr::mutate`) as shown
below:

``` r
# Using the dplyr workflow
library(dplyr)
tibble(
  Patients = c("A",       "B",       "C"),
  ICD_10   = c("0PS443Z", "3E05305", "F13ZHZZ")) %>%
  mutate(pr_class = classify_pr(ICD_10))
#> # A tibble: 3 × 3
#>   Patients ICD_10  pr_class         
#>   <chr>    <chr>   <chr>            
#> 1 A        0PS443Z Major Therapeutic
#> 2 B        3E05305 Minor Therapeutic
#> 3 C        F13ZHZZ Minor Diagnostic
```

Although applications using ICD-9 codes aren’t covered here, this
function also works the same for ICD-9 procedure codes.

``` r
classify_pr("0015") # Infusion of IL-2
#> [1] "Minor Therapeutic"
```

### CCSR for ICD-10-PCS Procedures

Much more powerful is the [Clinical Classifications Software Refined
(CCSR)](https://www.hcup-us.ahrq.gov/toolssoftware/ccsr/prccsr.jsp)
categories, which aggregates the expansive number of ICD-10-PCS
procedure codes into a smaller number of clinically meaningful
categories.

Similar to the procedure classes (and all functions starting with
`classify_`), this categorizes ICD codes into mutually exclusive
categories.

``` r
classify_ccsr_pr("0016070")
#> [1] "CNS010"
```

Again, this can be vectorized for use within data frames. The helper
function `explain_ccsr` can convert these categories into human-readable
labels.

``` r
library(dplyr)
tibble(icd_10 = c("04L04DZ", "009Y40Z", "0F7C7DZ", "GZ2ZZZZ")) %>%
  mutate(CCSR      = classify_ccsr_pr(icd_10),
         explained = explain_ccsr(CCSR))
#> # A tibble: 4 × 3
#>   icd_10  CCSR   explained                                                      
#>   <chr>   <chr>  <chr>                                                          
#> 1 04L04DZ CAR010 Ligation and embolization of vessels                           
#> 2 009Y40Z CNS003 Spinal canal and spinal cord drainage (excluding lumbar punctu…
#> 3 0F7C7DZ HEP008 Common bile duct sphincterotomy and stenting                   
#> 4 GZ2ZZZZ MHT004 Crisis intervention for mental health
```

You can also use the “detailed” option within `explain_ccsr(ccsr,
detailed=TRUE)` to return additional data about the CCSR category. This
returns a named list, which can be helpful in conjunction with the
[purrr package](https://purrr.tidyverse.org/).

``` r
library(purrr)
# NOTE: The results returned when `detailed = TRUE` depends on the
#       type of CCSR category. See the `CCSR_PR_categories` or 
#       `CCSR_DX_categories` in the `hcup.data` package for details
tibble(CCSR_PR = c("URN001", "RES010", "CAR004")) %>%
  mutate(Explained = explain_ccsr(CCSR_PR),
         details   = explain_ccsr(CCSR_PR, detailed = TRUE),
         domain    = map_chr(details, "clinical_domain"))
#> # A tibble: 3 × 4
#>   CCSR_PR Explained                                      details   domain       
#>   <chr>   <chr>                                          <list>    <chr>        
#> 1 URN001  Cystoscopy and ureteroscopy (including biopsy) <chr [7]> Urinary Syst…
#> 2 RES010  Tracheostomy                                   <chr [7]> Respiratory …
#> 3 CAR004  Percutaneous coronary interventions (PCI)      <chr [7]> Cardiovascul…
```
