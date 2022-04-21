#' Identify CCSR categories for ICD-10-CM codes
#'
#' Given a vector of ICD-10-CM diagnosis codes, this function returns
#' a list of the Clinical Classifications Software Refined (CCSR)
#' categories associated with these codes
#'
#' @importFrom rlang !!
#' @importFrom rlang :=
#' @param icd_10_dx A vector of ICD-10-CM diagnosis codes
#'
#' @return A list charachter vectors with a of `length(icd_10_dx)`.
#' @export
#'
#' @details
#'
#' The Clinical Classifications Software Refined (CCSR) software
#' provides both one-to-one mapping (see \code{\link{classify_ccsr_dx1}})
#' and one-to-many (see \code{\link{ccsr_dx}}) mapping of ICD-10-CM
#' diagnosis codes to CCSR categories. The one-to-many mapping is
#' necessary because many ICD-10 codes span multiple meaningful
#' categories, and the identification of all conditions related to
#' a diagnosis code would be lost by categorizing some ICD-10-CM
#' codes into a single category.
#'
#' ## One-to-many
#'
#' For example, consider the code **I11.0** (_Hypertensive heart
#' disease with heart failure_) which encompasses both **heart
#' failure** (CCSR category CIR019) and **hypertension with
#' complications** (CIR008). Classifying this code as heart
#' failure alone would miss meaningful information, but on the
#' other hand, some analyses require mutually exclusive
#' categorization.
#'
#' This function addresses identifying all CCSR categories associated
#' with an ICD code. For example, consider a data.frame with an ICD-10
#' code per row:
#'
#' \tabular{lc}{
#'   **pt_id** \tab **`ICD10dx`**  \cr
#'   A         \tab `K432`         \cr
#'   A         \tab `A401`         \cr
#'   B         \tab ...
#' }
#'
#' The corresponding CCSR codes for these two codes are below. Note that
#' `K432` only has one CCSR category, while `A401` has two.
#'
#' \tabular{lccc}{
#'   **`ICD10`**   \tab **CCSR1** \tab **CCSR2** \tab **CCSR**(n)  \cr
#'     `K432`      \tab   DIG010  \tab     ---   \tab   ...      \cr
#'     `A401`      \tab   INF002  \tab   INF003  \tab   ...      \cr
#'     ...         \tab    ...    \tab     ...   \tab   ...
#' }
#'
#' Running this function for patient A's two codes, results in **three**
#' rows because `A401` has two categories (plus the single row for `K432`).
#' See \href{https://r4ds.had.co.nz/tidy-data.html}{the tidy data section}
#' of R for Data Science or the \href{https://doi.org/10.18637/jss.v059.i10}{corresponding paper}
#' for more on this conceptual approach.
#'
#'
#'
#' \tabular{lcc}{
#'   **pt_id** \tab **`ICD10dx`** \tab **CCSR** \cr
#'   A         \tab   `K432`      \tab   DIG010 \cr
#'   A         \tab   `A401`      \tab   INF002 \cr
#'   A         \tab   `A401`      \tab   INF003 \cr
#'   B         \tab     ...       \tab    ...
#' }
#'
#' ## CCSR vs CCS
#'
#' There are numerous differences between CCSR and CCS (the predecessor to CCSR).
#' The CCS classifies codes into multi-level categories in a hierarchical manner,
#' which allows users of CCS to use varying levels of specificity in their
#' classification (see \code{\link{classify_ccs}}), while the CCSR does _not_ have
#' multiple classification levles. Additionally, CCSR does not classify codes into
#' mutually exclusive categories (for ICD-10 diagnosis codes) and the categories
#' used in CCSR aren't the same as the old categories used in CCS.
#'
#' See Appendix A of the \href{https://www.hcup-us.ahrq.gov/toolssoftware/ccsr/DXCCSR-User-Guide-v2022-1.pdf}{CCSR user guide}
#' for more details on the differences between CCSR and CCS.
#'
#' @seealso
#' \code{\link{classify_ccsr_dx1}} for identifying a single CCSR
#' category based on the principal diagnosis
#'
#' \code{\link{classify_ccs}} for the legacy CCS categories
#'
#' @examples
#' library(dplyr)
#' library(tibble) # for tribble fxn
#' library(tidyr)  # for unnest
#'
#' ## Using a single ICD code
#' ccsr_dx("K432")
#' ccsr_dx("A401")
#'
#' ## When vectorized, returns list
#' ccsr_dx(c("K432", "A401")) %>% str()
#'
#' ## Use unnest to make it tidy
#' tibble::tribble(
#'   ~pt_id,  ~ICD10,
#'      "A",  "K432",
#'      "A",  "A401") %>%
#'
#'   mutate(CCSR = ccsr_dx(ICD10)) %>%
#'   unnest_longer(CCSR)
#'
ccsr_dx <- function(icd_10_dx){
  check_icd_format(icd_10_dx)

  if(!rlang::inherits_any(icd_10_dx, "character")) {
    bad_classes <- glue::glue_collapse(glue::backtick(class(icd_10_dx)), sep = ", ", last = " and ")
    msg <- glue::glue("`icd_10_dx` should be a character, but provided variable is {bad_classes}")
    rlang::abort(msg)
  }

  # Lookup table
  CCSR_ref_df <- hcup.data:::CCSR_DX_tidy %>% dplyr::select(-.data$CCSR_n)

  # lookup fxn
  look_up <- function(code){
    CCSR <- lookup_table(icd_codes = code,
                         ref_df    = CCSR_ref_df,
                         data_col  = "I10_DX")
    return(CCSR[["CCSR"]])
  }

  # If has length = 1, don't make it a list
  if(rlang::has_length(icd_10_dx, n = 1)){
    return(look_up(icd_10_dx))
  }

  # Otherwise, use map to return list
  purrr::map(icd_10_dx, look_up)
}


#' Classify principal diagnosis into CCSR category
#'
#' Converts a vector of ICD-10 **principal** diagnosis codes to the
#' default Clinical Classifications Software Refined (CCSR) category
#' based on the inpatient or outpatient setting
#'
#' @param icd_10_dx1 A vector of **principal** ICD diagnosis codes
#'   (without decimals)
#' @param setting Either "`inpatient`", (default) or "`outpatient`"
#'   Note that this single default CCSR category only applies to
#'   the principal diagnosis (for `inpatient`) and first-listed
#'   diagnosis (for `outpatient`). These can also be abbreviated
#'   as "ip" or "op".
#'
#' @return A vector of the default CCSR categories
#'
#' @details
#'
#' The Clinical Classifications Software Refined (CCSR) software
#' provides both one-to-one mapping (see \code{\link{classify_ccsr_dx1}})
#' and one-to-many (see \code{\link{ccsr_dx}}) mapping of ICD-10-CM
#' diagnosis codes to CCSR categories. The one-to-many mapping is
#' necessary because many ICD-10 codes span multiple meaningful
#' categories, and the identification of all conditions related to
#' a diagnosis code would be lost by categorizing some ICD-10-CM
#' codes into a single category.
#'
#' For example, consider the code **I11.0** (_Hypertensive heart
#' disease with heart failure_) which encompasses both **heart
#' failure** (CCSR category CIR019) and **hypertension with
#' complications** (CIR008). Classifying this code as heart
#' failure alone would miss meaningful information, but on the
#' other hand, some analyses require mutually exclusive
#' categorization.
#'
#' CCSR accomplishes this by providing a "default" CCSR category
#' based on the principal diagnosis (for inpatient data) and the
#' first-listed diagnosis (for outpatient data); see page 16 of
#' \href{https://www.hcup-us.ahrq.gov/toolssoftware/ccsr/DXCCSR-User-Guide-v2022-1.pdf}{the CCSR DX user guide} for details.
#'
#' This function addresses identifying the default CCSR category
#' based on the principal/first-listed diagnosis. For applications
#' utilizing all ICD-10-CM codes, such as risk adjustment and
#' identification of comorbidities, please use the
#' \code{\link{ccsr_dx}} function.
#'
#' @seealso
#' \code{\link{ccsr_dx}} for identifying all CCSR categories in a
#' non-mutually exclusive method
#'
#' @export
#'
#' @examples
#' classify_ccsr_dx1("I110", setting = "inpatient")
#'
#' library(dplyr)
#' dplyr::tibble(ICD10 = c("G4730", "L563", "A001")) %>%
#'   mutate(CCSR_ip = classify_ccsr_dx1(ICD10, setting="inpatient"),
#'          CCSR_op = classify_ccsr_dx1(ICD10, setting="outpatient"))
classify_ccsr_dx1 <- function(icd_10_dx1, setting = NULL) {
  check_icd_format(icd_10_dx1)

  setting <- setting %||% heuristic_msg("setting", "inpatient")
  setting <- rlang::arg_match(setting, c("inpatient", "outpatient", "ip", "op"))


  rlang::inform(message = stringr::str_glue("`setting = {setting}` only applies to the principal/first listed diagnosis"),
                .frequency="regularly", .frequency_id="CCSR DX")

  CCSR_ref_df <- hcup.data::CCSR_DX_mapping

  CCSR <- lookup_table(icd_codes = icd_10_dx1,
                       ref_df    = CCSR_ref_df,
                       data_col  = "I10_DX")

  ## Now that we have the tibble, select the column of interest
  if(setting=="ip"|setting=="inpatient"){
    return(CCSR$default_CCSR_IP)
  }
  if(setting=="op"|setting=="outpatient"){
    return(CCSR$default_CCSR_OP)
  }

}


#' Identify CCSR categories for ICD-10-CM codes
#'
#' Given a data.frame with ICD-10-CM diagnosis codes, this function
#' appends a column of the corresponding Clinical Classifications
#' Software Refined (CCSR) categories, in tidy format
#'
#' @importFrom rlang !!
#' @importFrom rlang :=
#' @param .data Data frame with column named `dx_col`
#' @param dx_col The (unquoted) name of the column in `.data` containing
#'   ICD-10-CM codes.
#'
#' @return An object of the same type as `.data`. The returned data.frame
#'   will have a new column named `CCSR` containing the CCSR categories
#'   assocaited with the ICD code in the `dx_col` column. For ICD codes
#'   with multiple CCSR categories, multiple rows will be returned
#'   (in a \href{https://r4ds.had.co.nz/tidy-data.html#fig:tidy-gather}{longer, tidy format}).
#' @keywords internal
#' @export
#'
#' @details
#' `r lifecycle::badge('experimental')`
#' The Clinical Classifications Software Refined (CCSR) software
#' provides both one-to-one mapping (see \code{\link{classify_ccsr_dx1}})
#' and one-to-many (see \code{\link{ccsr_dx}}) mapping of ICD-10-CM
#' diagnosis codes to CCSR categories. The one-to-many mapping is
#' necessary because many ICD-10 codes span multiple meaningful
#' categories, and the identification of all conditions related to
#' a diagnosis code would be lost by categorizing some ICD-10-CM
#' codes into a single category.
#'
#' ## One-to-many
#'
#' For example, consider the code **I11.0** (_Hypertensive heart
#' disease with heart failure_) which encompasses both **heart
#' failure** (CCSR category CIR019) and **hypertension with
#' complications** (CIR008). Classifying this code as heart
#' failure alone would miss meaningful information, but on the
#' other hand, some analyses require mutually exclusive
#' categorization.
#'
#' This function addresses identifying all CCSR categories associated
#' with an ICD code. For example, consider a data.frame with an ICD-10
#' code per row:
#'
#' \tabular{lc}{
#'   **pt_id** \tab **`ICD10dx`**  \cr
#'   A         \tab `K432`         \cr
#'   A         \tab `A401`         \cr
#'   B         \tab ...
#' }
#'
#' The corresponding CCSR codes for these two codes are below. Note that
#' `K432` only has one CCSR category, while `A401` has two.
#'
#' \tabular{lccc}{
#'   **`ICD10`**   \tab **CCSR1** \tab **CCSR2** \tab **CCSR**(n)  \cr
#'     `K432`      \tab   DIG010  \tab     ---   \tab   ...      \cr
#'     `A401`      \tab   INF002  \tab   INF003  \tab   ...      \cr
#'     ...         \tab    ...    \tab     ...   \tab   ...
#' }
#'
#' Running this function for patient A's two codes, results in **three**
#' rows because `A401` has two categories (plus the single row for `K432`).
#' See \href{https://r4ds.had.co.nz/tidy-data.html}{the tidy data section}
#' of R for Data Science or the \href{https://doi.org/10.18637/jss.v059.i10}{corresponding paper}
#' for more on this conceptual approach.
#'
#'
#'
#' \tabular{lcc}{
#'   **pt_id** \tab **`ICD10dx`** \tab **CCSR** \cr
#'   A         \tab   `K432`      \tab   DIG010 \cr
#'   A         \tab   `A401`      \tab   INF002 \cr
#'   A         \tab   `A401`      \tab   INF003 \cr
#'   B         \tab     ...       \tab    ...
#' }
#'
#' ## CCSR vs CCS
#'
#' There are numerous differences between CCSR and CCS (the predecessor to CCSR).
#' The CCS classifies codes into multi-level categories in a hierarchical manner,
#' which allows users of CCS to use varying levels of specificity in their
#' classification (see \code{\link{classify_ccs}}), while the CCSR does _not_ have
#' multiple classification levles. Additionally, CCSR does not classify codes into
#' mutually exclusive categories (for ICD-10 diagnosis codes) and the categories
#' used in CCSR aren't the same as the old categories used in CCS.
#'
#' See Appendix A of the \href{https://www.hcup-us.ahrq.gov/toolssoftware/ccsr/DXCCSR-User-Guide-v2022-1.pdf}{CCSR user guide}
#' for more details on the differences between CCSR and CCS.
#'
#' @seealso
#' \code{\link{classify_ccsr_dx1}} for identifying a single CCSR
#' category based on the principal diagnosis
#'
#' \code{\link{classify_ccs}} for the legacy CCS categories
#'
#' @examples
#' library(dplyr)
#' library(tibble) # for tribble fxn
#'
#' ## Using a df to return a tidy df
#' tibble::tribble(
#'   ~pt_id,  ~ICD10,
#'      "A",  "K432",
#'      "A",  "A401") %>%
#'
#'   mutate_ccsr_dx(dx_col = ICD10)
#'
#' df <- tibble::tribble(
#'   ~pt_id,  ~ICD10,
#'   "A",     "K432",
#'   "A",     "A401",
#'   "B",     "I495",
#'   "B",    "E8771",
#'   "C",    "A5442",
#'   "C",     "A564"
#' )
#' df %>%
#'
#'  mutate_ccsr_dx(dx_col = ICD10) %>%
#'  mutate(CCSR_expl = explain_ccsr(CCSR))
mutate_ccsr_dx <- function(.data, dx_col){

  if(missing(dx_col)) rlang::abort("Please specify the column of ICD codes (`dx_col`)")
  if(!rlang::inherits_any(.data, "data.frame")) {
    bad_classes <- glue::glue_collapse(glue::backtick(class(.data)), sep = ", ", last = " and ")
    msg <- glue::glue("`.data` should be a data.frame, but provided variable is {bad_classes}")
    rlang::abort(msg)
  }


  dx_col <- rlang::enquo(dx_col)
  if(! rlang::as_label(dx_col) %in% names(.data)){
    rlang::abort(message = glue::glue("`{rlang::as_label(dx_col)}` was not found as a column in data frame!"))
  }

  # warn_decimals(dplyr::pull(.data, dx_col_raw))
  ccsr_tidy <- hcup.data:::CCSR_DX_tidy %>% dplyr::select(-.data$CCSR_n)
  j <- "I10_DX"
  names(j) <- rlang::as_label(dx_col)

  dplyr::left_join(.data, ccsr_tidy, by=j)
}
