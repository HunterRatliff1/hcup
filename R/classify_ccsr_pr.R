#' Classify ICD-10-PCS procedures into CCSR a category
#'
#' Converts a vector of ICD-10 procedure codes into a vector of
#' Clinical Classifications Software Refined categories
#' for procedures using the HCUP's software
#'
#' @param icd_pr A vector of ICD-10-PCS procedure codes, without decimals
#'
#' @return A vector of CCSR categories for procedures
#' @export

#' @source
#' HCUP page for \href{https://www.hcup-us.ahrq.gov/toolssoftware/ccsr/prccsr.jsp}{CCSR for procedures}
#'
#' @seealso
#' See \code{\link{explain_ccsr}} to convert the CCSR category into a
#' description of the category
#'
#' \code{\link[hcup.data:CCSR_PR_mapping]{CCSR_PR_mapping}} in the
#' `hcup.data` package for the dataset

#' @examples
#' classify_ccsr_pr("0016070")
#'
#' icd_10_pr <- c("04L04DZ", "009Y40Z", "0F7C7DZ", "GZ2ZZZZ")
#' classify_ccsr_pr(icd_10_pr)
#'
#' # Vectorized using `mutate()`
#' library(dplyr)
#'
#' dplyr::tibble(icd_10 = icd_10_pr) %>%
#'   mutate(CCSR = classify_ccsr_pr(icd_10),
#'          explained = explain_ccsr(CCSR))
classify_ccsr_pr <- function(icd_pr){
  check_icd_format(icd_pr)

  CCSR_ref <- hcup.data::CCSR_PR_mapping

  CCSR_PR <- lookup_table(icd_codes = icd_pr,
                          ref_df    = CCSR_ref,
                          data_col  = "I10_PR")

  # CCSR_PR
  CCSR_PR[["CCSR"]]

}
