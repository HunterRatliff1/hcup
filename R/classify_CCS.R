#' Classify codes into CCS categories
#'
#' Converts a vector of ICD diagnosis codes into a vector of Clinical
#' Classifications Software (CCS) categories using the HCUP's software
#'
#' @param icd_codes Vector of ICD codes, without decimals
#' @param code_type One of `"dx9", "dx10", "pr9", or "pr10"`, corresponding
#'   to the type and version of ICD codes used
#' @param level The level of CCS to use. The default is to use the
#'   single-level CCS category (`"single"`). See Details section for use
#'   with mutli-level CCS
#'
#' @return A vector of the same length as `icd_codes`. If `level="all"`,
#'   then a tibble will be returned, with each ICD code per row
#' @export
#' @details
#'
#' ## Single vs Multi-level CCS categories
#'
#' Unlike the CCS (CCS Refined), the original CCS classification
#' has "single-level" and "multi-level" categories.
#'
#' This system classifies all diagnoses and procedures into unique groups.
#' The single-level CCS aggregates diagnoses into 285 mutually exclusive
#' categories and procedures into 231 mutually exclusive categories.
#'
#' The multi-level CCS expands the single-level CCS into a hierarchical
#' system. The multi-level system has four levels for diagnoses and three
#' levels for procedures, which provide the opportunity to examine general
#' groupings or to assess very specific conditions and procedures. The
#' multi-level CCS groups single-level CCS categories into broader
#' body systems or condition categories (e.g., "Diseases of the Circulatory
#' System," "Mental Disorders," and "Injury").
#'
#' The levels allowed in the multi-level CCS depends on the version/type
#' of ICD codes used:
#'
#' * __dx9__ (ICD-9-CM diagnosis): Allows levels 1 - 4
#' * __pr9__ (ICD-9-CM procedure): Allows levels 1 - 3
#' * __dx10__ and __pr10__ (ICD-10): Allows levels 1 - 2
#'
#' An example using CCS for diagnosis codes is shown below:
#'
#' \tabular{lll}{
#'   **CCS_lvl1** \tab **CCS_level2** \tab **CCS (single-level)** \cr
#'     DX-1       \tab   DX-1.1       \tab `DX1, DX2, DX3, DX9`   \cr
#'     DX-1       \tab   DX-1.2       \tab `DX4`                  \cr
#'     DX-1       \tab   DX-1.3       \tab `DX5, DX6, DX7`        \cr
#'     DX-1       \tab   DX-1.4       \tab `DX8`                  \cr
#'     DX-1       \tab   DX-1.5       \tab `DX10`
#' }
#'
#' It also splits single-level CCS categories to provide more detail.
#' For example, the single-level CCS diagnosis category `4` (_Mycoses_)
#' can be further split into 1.2.1 (_Candidiasis of the mouth_) and
#' 1.2.2 (_Other mycoses_).
#'
#' Further details can be found on the
#' \href{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/ccsfactsheet.jsp}{CCS fact sheet}.
#'
#' ## Problems with the original notation
#' The notation used in the original CCS categories has a few limitations
#' in how it names categories.
#'
#' First, the CCS category `CCS = '3'` maps to "_Other bacterial
#' infections_" for diagnostic codes, but the same category (`CCS = '3'`)
#' maps to "Laminectomy" for procedures. Second, the CCS category is
#' supposed to be treated as a string (because HCUP designs their software
#' for SAS), but R will appropriately assume these categories are numbers.
#'
#' The third issue is the ambiguity of _single-level_ and _multi-level_ CCS
#' categories. In the original software, the first level of the _multi-level_
#' CCS uses the same syntax as the _single-level_ categories. For example,
#' "4" represents "Mycoses" as a _single-level_ category, but maps to
#' "Diseases of the blood and blood-forming organs" as a _multi-level_ category!
#'
#' This all turns out to be incredibly confusing as the same number `"3"` could
#' represent:
#' * _"Other bacterial infections"_ if it's the single-level category
#' for a diagnosis
#' * _"Endocrine; nutritional; and metabolic diseases and immunity disorders"_ if
#' it's the multi-level category for a diagnosis
#' * _"Laminectomy"_ if it's the single-level category for a procedure
#' * _"Operations on the eye"_ if it's the multi-level category for a procedure
#'
#'
#' ## Notation used in this package
#' To address these issues, this package prepends **"DX"**
#' or **"PR"** before the *default CCS category* (e.g. `3` becomes `DX3` or `PR3`
#' for diagnoses or procedures, respectively). For the *multi-level categories*,
#' the prefixes are **"DX-"** and **"PR-"**.
#'
#' Although this is a trivial change for most applications, it is mentioned
#' here because (for the purposes of reproducibility) this notation should
#' be changed back to the original format for any publications or uses beyond this
#' package.
#'
#' @seealso
#' See \code{\link{explain_ccs}} to convert the CCS category into a
#' description of the category
#'
#' Use \code{\link{ccsr_dx}} to use CCSR categories, which are prefered
#' for ICD-10 data
#'
#' \code{\link[hcup.data:CCS_dx9_map]{CCS_dx9_map}},
#' \code{\link[hcup.data:CCS_pr9_map]{CCS_pr9_map}},
#' \code{\link[hcup.data:CCS_dx10_map]{CCS_dx10_map}}, and
#' \code{\link[hcup.data:CCS_pr10_map]{CCS_pr10_map}} in the
#' `hcup.data` package for the datasets
#'
#' @source
#' HCUP page for \href{https://www.hcup-us.ahrq.gov/toolssoftware/ccs/ccs.jsp}{CCS with ICD-9}
#'
#' HCUP page for \href{https://www.hcup-us.ahrq.gov/toolssoftware/ccsr/ccsr_archive.jsp}{CCS beta with ICD-10}
#'
#' @examples
#' library(dplyr)
#' ## Look up ICD-9 DX codes
#' classify_ccs("8442", code_type = "dx9", level = "single")
#'
#' # Vectorized
#' tibble(ICD = c("8442", "1403", "9682",  "36463", "3595")) %>%
#'   mutate(CCS      = classify_ccs(ICD, code_type = "dx9"),
#'          CCS_expl = explain_ccs(CCS))
#'
#' ## Works the same for ICD-9 PR codes
#' classify_ccs("066", code_type = "pr9")
#'
#' ## Also works for ICD-10, but CCSR is prefered
#' classify_ccs("M61019",  "dx10", level="single")
#' classify_ccs("0TY10Z1", "pr10", "single")
#'
#' ## Specify level to use multi-level CCS
#' c("0223","1100") %>%
#'   classify_ccs(code_type = "dx9", level="1")
#'
#' # If you want to see all levels use `level="all"`
#' # which returns a tibble
#' df <- c("0223","1100") %>%
#'   classify_ccs(code_type = "dx9", level="all")
#' df
#'
#' if(rlang::is_installed("dplyr", version = "1.0.0")){
#'   df %>% # requires dplyr::across()
#'     mutate(across(c(CCS, CCS_lvl1), explain_ccs))
#' }
classify_ccs <- function(icd_codes, code_type, level=NULL) {
  check_icd_format(icd_codes)

  # Check to see that a valid `code_type` was given, then base on that
  # code type, check that a valid level was given. Finally, match codes
  # to their reference data.frame
  code_type <- rlang::arg_match(code_type, c("dx9", "dx10", "pr9", "pr10"))


  # If level is not provided, then default to single
  if(is.null(level)){
    rlang::inform(message = glue::glue("No value provided for `level`. Using `level = single` as the default"),
                  .frequency="regularly", .frequency_id=paste0("classify_ccs",code_type))
  }
  level <- level %||% "single"
  level <- as.character(level)


  if(code_type=="dx9"){
    CCS_ref_df <- hcup.data::CCS_dx9_map   # 4 levels

    level <- rlang::arg_match(level, c("single", "1", "2", "3", "4", "all"))

    CCS <- lookup_table(icd_codes = icd_codes,
                        ref_df    = CCS_ref_df,
                        data_col  = "I9_DX")
  }
  if(code_type=="pr9") {
    CCS_ref_df <- hcup.data::CCS_pr9_map   # 3 levels

    level <- rlang::arg_match(level, c("single", "1", "2", "3", "all"))

    CCS <- lookup_table(icd_codes = icd_codes,
                        ref_df    = CCS_ref_df,
                        data_col  = "I9_PR")

  }
  if(code_type=="dx10") {
    CCS_ref_df <- hcup.data::CCS_dx10_map  # 2 levels

    level <- rlang::arg_match(level, c("single", "1", "2", "all"))

    CCS <- lookup_table(icd_codes = icd_codes,
                        ref_df    = CCS_ref_df,
                        data_col  = "I10_DX")
  }
  if(code_type=="pr10") {
    CCS_ref_df <- hcup.data::CCS_pr10_map

    level <- rlang::arg_match(level, c("single", "1", "2", "all"))

    CCS <- lookup_table(icd_codes = icd_codes,
                        ref_df    = CCS_ref_df,
                        data_col  = "I10_PR")
  }

  # If told to return all levels, provide the whole data.frame
  if(level=="all"){
    return(CCS)
  }

  # Change the user argument to match the columns in the table
  level <- switch (level,
                   "single" = "CCS",
                   "1"      = "CCS_lvl1",
                   "2"      = "CCS_lvl2",
                   "3"      = "CCS_lvl3",
                   "4"      = "CCS_lvl4",
                   "all"    = "all")

  # Return the value
  CCS[[level]]

}

if(F){
  load_all()



  # ## Look up ICD-9 DX codes
  # classify_ccs("8442", code_type = "dx9", level = "single")
  #
  # # Vectorized
  # dplyr::tibble(ICD = c("8442", "1403", "9682",  "36463", "3595")) %>%
  #   mutate(CCS      = classify_ccs(ICD, code_type = "dx9"),
  #          CCS_expl = explain_ccs(CCS))

  dplyr::tibble(ICD = c("0223","1100")) %>%
    mutate(CCS      = classify_ccs(ICD, "dx9", level = "single"),
           CCS_lvl2 = classify_ccs(ICD, "dx9", level = "1"),
           CCS_expl = explain_ccs(CCS),
           CCS_expl2 = explain_ccs(CCS_lvl2))

  c("0223","1100") %>%
    classify_ccs(code_type = "dx9", "all")

  # c("8442", "01001", "9682",  "36463", "3595") %>%
  #   classify_ccs(code_type = "dx9", 2) %>%
  #   explain_ccs()

  ## Look up ICD-9 PR codes
  c("066", "2642", "1911", "8395", "1152") %>%
    classify_ccs(code_type = "pr9") %>%
    explain_ccs()


  c("M61019", "S68610A", "M80041P", "S12330A", "S76299D") %>%
    classify_ccs(code_type = "dx10") %>%
    explain_ccs()




  c("0TY10Z1", "0RJM4ZZ", "0UT07ZZ", "0210088", "02HK32Z") %>%
    classify_ccs(code_type = "pr10") %>%
    explain_ccs()




}
