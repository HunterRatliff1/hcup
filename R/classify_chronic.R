#' Classify ICD diagnosis codes into Chronic Condition Indicator categories
#'
#' Converts a vector of ICD diagnosis codes into a vector of Chronic
#' Condition Indicator (CCI) categories using the HCUP's software
#'
#' @param icd_dx A vector of ICD diagnosis codes (without decimals)
#' @param icd_version The version of ICD codes to be used. Can be
#'   either ICD-10 (`icd_version = 10`) or ICD-9 (`icd_version = 9`)
#'
#' @return A vector of CCI classifications. The levels returned depend on
#'   the version of ICD codes. See Details section
#' @export
#'
#' @seealso
#' \code{\link[hcup.data:CCI_icd9]{CCI_icd9}} and
#' \code{\link[hcup.data:CCI_icd10]{CCI_icd10}} in the
#' `hcup.data` package for the datasets
#'
#' @details
#' Starting in v2021.1 (beta version), the CCI tool for ICD-10-CM was expanded
#' to identify four types of conditions. Therefore, running the
#' function with `icd_version = 10` will categorize ICD-10-CM codes into one of
#' the four conditions below:
#' * __Acute__: Examples include aortic embolism, bacterial infection, pregnancy,
#'  and an initial encounter for an injury
#' * __Chronic__: Examples include malignant cancer, diabetes, obesity,
#' hypertension, and many mental health conditions
#' * __Both__: Examples include persistent asthma with (acute) exacerbation,
#' acute on chronic heart failure, and kidney transplant rejection
#' * __Not Applicable__: Examples include external cause of morbidity codes,
#' injury sequela codes, and codes starting with the letter Z for screening
#' or observation
#'
#' In previous version (using ICD-9-CM), CCI classified conditions as __Chronic__
#' or __NonChronic__, so caution should be used when appying the CCI on data
#' using both ICD-9 and ICD-10.
#'
#' @source
#' HCUP page for \href{https://www.hcup-us.ahrq.gov/toolssoftware/chronic/chronic.jsp}{CCI with ICD-9}
#'
#' HCUP page for \href{https://www.hcup-us.ahrq.gov/toolssoftware/chronic_icd10/chronic_icd10.jsp}{CCI with ICD-10}
#' @examples
#' ## Take an ICD dx code and return the CCI
#' classify_chronic(icd_dx = "G4730", icd_version = 10)
#' classify_chronic(icd_dx = "56081", icd_version = 9)
#'
#' # Vectorized version
#' icd_codes <- c("G4730", "L563", "M25151")
#' classify_chronic(icd_codes, icd_version = 10)
#'
#' # This works well in the dplyr workflow
#' library(dplyr)
#' pt_data <- tibble(
#'   Patients = c("A",     "B",    "C"),
#'   ICD_10   = c("G4730", "L563", "M25151")
#' )
#'
#' pt_data %>%
#'   mutate(CCI = classify_chronic(ICD_10, icd_version = 10))
classify_chronic <- function(icd_dx, icd_version){

  warn_decimals(icd_dx)

  icd_version <- as.character(icd_version) # in case provided as numeric
  icd_version <- rlang::arg_match(icd_version, c("9", "10"))

  if(icd_version=="9"){
    CCI_df  <- hcup.data::CCI_icd9

    CCI <- lookup_table(icd_codes = icd_dx,
                        ref_df   = CCI_df,
                        data_col = "I9_DX")
  }
  if(icd_version=="10"){
    CCI_df <- hcup.data::CCI_icd10

    CCI <- lookup_table(icd_codes = icd_dx,
                        ref_df   = CCI_df,
                        data_col = "I10_DX")
  }

  CCI[["CCI"]]
}
