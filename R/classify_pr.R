#' Classify Procedure Classes
#'
#' Converts a vector of ICD procedure codes into a vector of Procedure Classes
#' (for ICD-9) or Procedure Classes Refined (for ICD-10) using the HCUP's
#' software
#'
#' @param icd_pr A vector of ICD-9-CM or ICD-10-PCS codes for procedures. You do
#'   not need to specify if the codes are ICD-9 or ICD-10.
#'
#' @return A vector of Procedure Classes, falling into one of four categories
#'   listed in the details section
#' @details
#' The HCUP classifies procedures into the following four categories:
#' * __Minor Diagnostic__: Nonoperating room procedures that are diagnostic
#' * __Minor Therapeutic__: Nonoperating room procedures that are therapeutic
#' * __Major Diagnostic__: Procedures that are considered operating room
#' procedures that are performed for diagnostic reasons
#' * __Major Therapeutic__: Procedures that are considered operating room
#' procedures that are performed for therapeutic reasons
#'
#' @section Troubleshooting:
#' This has not been tested extensively with ICD-9 codes in
#' practice, so users experiencing difficulty may want to implement their own
#' solutions. Please see `?hcup.data::proc_class_icd9` for the data used here, and use
#' `data(proc_class_icd9)` to adjust to your needs.
#'
#' @export
#' @source HCUP page for
#' \href{https://www.hcup-us.ahrq.gov/toolssoftware/procedure/procedure.jsp}{Procedure
#' Classes with ICD-9}
#'
#' HCUP page for
#' \href{https://www.hcup-us.ahrq.gov/toolssoftware/procedureicd10/procedure_icd10.jsp}{Procedure
#' Classes Refined with ICD-10}
#'
#' @seealso
#' \code{\link[hcup.data:proc_class_icd10]{proc_class_icd10}} and
#' \code{\link[hcup.data:proc_class_icd9]{proc_class_icd9}} in the
#' `hcup.data` package for the datasets
#'
#' @examples
#' classify_pr("00993ZX")
#' classify_pr("0015")
#'
#' # Using the dplyr workflow
#' library(dplyr)
#' pt_data <- tibble(
#'   Patients = c("A",       "B",       "C"),
#'   ICD_10   = c("0PS443Z", "3E05305", "F13ZHZZ")
#' )
#'
#' pt_data %>%
#'   mutate(pr_class = classify_pr(ICD_10))
#' \dontrun{
#' # Set of wrapper functions can be helpful for filtering
#' is_diagnostic_pr("F13ZHZZ")
#' is_major_pr("0PS443Z")
#'
#' pt_data %>% filter(is_therapeutic_pr(ICD_10))
#' }
#'
#' @importFrom dplyr %>%
classify_pr <- function(icd_pr){

  warn_decimals(icd_pr)

  proc_class_df <- dplyr::bind_rows(
    dplyr::select(hcup.data::proc_class_icd9, ICD=.data$I9_PR, proc_class),
    dplyr::select(hcup.data::proc_class_icd10, ICD=.data$I10_PR, proc_class)
  )

  proc_class <- lookup_table(icd_codes = icd_pr,
                             ref_df    = proc_class_df,
                             data_col  = "ICD")

  proc_class[["proc_class"]]
}





