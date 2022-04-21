
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

## Classify diagnosis codes by chronicity (CCI)

This works for ICD-9 or ICD-10 diagnosis codes, although the categories
differ across ICD versions

``` r
library(hcup)
# classify_chronic(icd_dx = "G4730", icd_version = "dx10")
# classify_chronic(icd_dx = "56081", icd_version = "dx9")


# Vectorized version
icd_codes <- c("G4730", "L563", "M25151")
# classify_chronic(icd_codes, icd_version = "dx10")
```

## Classify procedures by class

Identifies procedures by [Procedure
Classes](https://www.hcup-us.ahrq.gov/toolssoftware/procedureicd10/procedure_icd10.jsp),
which can be \[1\] diagnostic or therapeutic and \[2\] whether they
major (expected to be performed in an operating room) or minor. This
works with both ICD-9-CM procedure codes and ICD-10-PCS procedure codes.

``` r
classify_pr("0PS443Z") # Spine surgery
#> [1] "Major Therapeutic"
classify_pr("009U3ZX") # Lumbar puncture
#> [1] "Minor Diagnostic"
classify_pr("0015")    # Infusion of IL-2
#> [1] "Minor Therapeutic"
```

## Classify ICD-9 into CCS

This function categorizes ICD codes for both diagnoses and procedures
into mutually exclusive [Clinical Classifications Software (CCS)
categories](https://www.hcup-us.ahrq.gov/toolssoftware/ccs/ccs.jsp).
This was designed for use with ICD-9 codes, but the HCUP used to provide
a [beta
version](https://www.hcup-us.ahrq.gov/toolssoftware/ccsr/ccsr_archive.jsp)
that works with ICD-10

``` r
library(dplyr)
## Look up ICD-9 DX codes
classify_ccs("8442", code_type = "dx9", level = "single")
#> [1] "DX232"

# Vectorized
tibble(ICD = c("8442", "1403", "9682",  "36463", "3595")) %>%
  mutate(CCS      = classify_ccs(ICD, code_type = "dx9", "single"),
         CCS_expl = explain_ccs(CCS))
#> # A tibble: 5 × 3
#>   ICD   CCS   CCS_expl                                
#>   <chr> <chr> <chr>                                   
#> 1 8442  DX232 Sprains and strains                     
#> 2 1403  DX11  Cancer of head and neck                 
#> 3 9682  DX242 Poisoning by other medications and drugs
#> 4 36463 DX91  Other eye disorders                     
#> 5 3595  DX95  Other nervous system disorders

## Works the same for ICD-9 PR codes
classify_ccs("066", code_type = "pr9")
#> No value provided for `level`. Using `level = single` as the default
#> This message is displayed once every 8 hours.
#> [1] "PR10"
```

As mentioned above, `classify_ccs()` will also work with ICD-10 using a
beta version that is no longer updated, but the CCSR (below) is
preferred. However, if you’re working with a mix of ICD-9 and ICD-10
codes, you can use classify\_ccs() understanding the limitations

``` r
# Fake patient data
tibble::tribble(
  ~patient, ~pr_vers,     ~code,
       "A",       10, "0TY10Z1",
       "B",       10, "0RJM4ZZ",
       "C",       10, "0210088",
       "D",        9,    "3352",
       "E",        9,    "3323",
       "F",        9,    "3639"
  ) %>%
  
  mutate(CCS = case_when(
    pr_vers==9  ~ classify_ccs(code, code_type = "pr9"),
    pr_vers==10 ~ classify_ccs(code, code_type = "pr10"))) %>%
  mutate(expln = explain_ccs(CCS))
#> # A tibble: 6 × 5
#>   patient pr_vers code    CCS   expln                                           
#>   <chr>     <dbl> <chr>   <chr> <chr>                                           
#> 1 A            10 0TY10Z1 PR105 Kidney transplant                               
#> 2 B            10 0RJM4ZZ PR149 Arthroscopy                                     
#> 3 C            10 0210088 PR44  Coronary artery bypass graft (CABG)             
#> 4 D             9 3352    PR176 Organ transplantation (other than bone marrow c…
#> 5 E             9 3323    PR37  Diagnostic bronchoscopy and biopsy of bronchus  
#> 6 F             9 3639    PR44  Coronary artery bypass graft (CABG)
```

## Classify ICD-10 into CCSR

The [Clinical Classifications Software Refined (CCSR)
software](https://www.hcup-us.ahrq.gov/toolssoftware/ccsr/ccs_refined.jsp)
for ICD-10 works similar to CCS for procedures, but capitalizes on
ICD-10’s specificity

``` r
classify_ccsr_pr("0016070")
#> [1] "CNS010"

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

For ICD-10 diagnostic codes, the specificity actually creates a bit of a
problem as CCSR doesn’t categorize diagnoses into mutually exclusive
categories (see `vignette("ccsr-dx")`).

``` r
ccsr_dx("I110") # Hypertensive heart disease with heart failure
#> [1] "CIR008" "CIR019"
ccsr_dx("I110") %>% explain_ccsr()
#> [1] "Hypertension with complications and secondary hypertension"
#> [2] "Heart failure"
```

This is wonderful in some senses, because it captures more information,
but makes it difficult to answer some questions (e.g. do admissions for
heart failure cost more than admissions for hypertension?). To address
this, the principal diagnosis can still be categorized into a single
CCSR category.

``` r
tibble::tribble(
  ~Pt_id, ~DX_NUM,     ~ICD,
     "A",  "DX_1",   "I110",
     "A",  "DX_2", "F17210",
     "B",  "DX_1",   "I150",
     "B",  "DX_2",   "Z370",
     "B",  "DX_3",   "E876"
  ) %>%
  
  # Limit to the principal diagnosis
  filter(DX_NUM == "DX_1") %>%
  mutate(prin_CCSR      = classify_ccsr_dx1(ICD, setting="ip"),
         principal_CCSR = explain_ccsr(prin_CCSR)) 
#> `setting = ip` only applies to the principal/first listed diagnosis
#> This message is displayed once every 8 hours.
#> # A tibble: 2 × 5
#>   Pt_id DX_NUM ICD   prin_CCSR principal_CCSR                                   
#>   <chr> <chr>  <chr> <chr>     <chr>                                            
#> 1 A     DX_1   I110  CIR019    Heart failure                                    
#> 2 B     DX_1   I150  CIR008    Hypertension with complications and secondary hy…
```
