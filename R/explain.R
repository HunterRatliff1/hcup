#' Look up CCSR category descriptions
#'
#' @param ccsr Vector of CCSR codes to be explained. Can be a mix of diagnosis
#'   and/or procedure CCSR groups.
#' @param detailed Single logical value which indicates if more details should
#'   be returned. The default is `FALSE`, in which case only the description of
#'   the CCSR category is returned. If set to `TRUE`, it will return a list of
#'   named character vectors
#'
#' @export
#'
#' @examples
#' # Explain the CCSR for diagnosis categories
#' explain_ccsr("SKN003")
#'
#' # The same function works for procedures
#' explain_ccsr("LYM007")
#'
#' # Vectorized using `mutate()`
#' library(dplyr)
#' dplyr::tibble(
#'   CCSR = c("NVS011", "DIG021", "MBD028", "URN001", "RES010", "CAR004"),
#'   Type = c("DX", "DX", "DX", "PR", "PR", "PR")) %>%
#' mutate(explained = explain_ccsr(CCSR))
#'
#' ## Examples using the detailed option
#' detailed <- tibble(CCSR_PR = c("URN001", "RES010", "CAR004")) %>%
#'   mutate(Explained = explain_ccsr(CCSR_PR),
#'          details   = explain_ccsr(CCSR_PR, detailed = TRUE))
#'
#' library(purrr) # use purrr to extract one option
#' detailed %>%
#'   mutate(domain = map_chr(details, "clinical_domain"))
#'
#' library(tidyr) ## use unnest_wider to see all details
#' detailed %>%
#'   unnest_wider(details)
explain_ccsr <- function(ccsr, detailed=FALSE){
  CCSR_DX <- hcup.data::CCSR_DX_categories
  CCSR_PR <- hcup.data::CCSR_PR_categories

  # If given single NA value, return NA
  if(length(ccsr)==1){
    if(is.na(ccsr)) return(NA)
  }

  # Regular lookup unless told to return details
  if(!detailed){
    CCSR_ref <- dplyr::bind_rows(
      dplyr::select(CCSR_DX, CCSR, .data$CCSR_desc),
      dplyr::select(CCSR_PR, CCSR, .data$CCSR_desc)
    )

    CCSR <- lookup_table(icd_codes = ccsr,
                         ref_df    = CCSR_ref,
                         data_col  = "CCSR")

    # Return NULL if nothing is found
    if(nrow(CCSR)==0) {
      rlang::warn(glue::glue("No CCSR category found for {ccsr}"))
      return(NULL)
    }
    return(CCSR[["CCSR_desc"]])
  }

  # If told to return details, return the row for that CCSR category,
  # and transpose it to make a named list
  if(detailed){
    ccsr %>%
      purrr::map(function(category){

        if(is.null(category)|is.na(category)) {
          return(category)
        }


        if(category %in% CCSR_DX$CCSR) df <- CCSR_DX[CCSR_DX$CCSR==category,]
        if(category %in% CCSR_PR$CCSR) df <- CCSR_PR[CCSR_PR$CCSR==category,]

        # Return NULL if nothing is found
        if(nrow(df)==0) {
          rlang::warn(glue::glue("No CCSR category found for {category}"))
          return(NULL)
        }

        t(df)[,1]

      })
  }

}

#' Look up CCS category descriptions
#'
#' @param ccs Vector of CCS codes to be explained. Can be a mix of diagnosis
#'   and/or procedure CCS groups.
#' @param detailed Single logical value which indicates if more details should
#'   be returned. The default is `FALSE`, in which case only the description of
#'   the CCS category is returned. If set to `TRUE`, it will return a list of
#'   named character vectors
#'
#' @export
#'
#' @examples
#' # Explain the CCS for diagnosis categories
#' explain_ccs("DX37")
#'
#' # The same function works for procedures
#' explain_ccsr("PR28")
#'
#' # Vectorized using `mutate()`
#' library(dplyr)
#' dplyr::tibble(
#'   CCS = c("DX37", "PR28", "PR-5.6", "DX-9.6.3"),
#'   Type = c("DX", "PR", "PR", "DX")) %>%
#' mutate(explained = explain_ccs(CCS))
explain_ccs <- function(ccs, detailed=FALSE){
  CCSR_DX <- hcup.data::CCS_dx10_label
  CCSR_PR <- hcup.data::CCSR_PR_categories

  # Regular lookup unless told to return details
  if(!detailed){
    CCSR <- lookup_table(icd_codes = ccs,
                         ref_df    = hcup.data::CCS_labels,
                         data_col  = "CCS")
    return(CCSR[["CCS_label"]])
  }

  # If told to return details, return the row for that CCSR category,
  # and transpose it to make a named list
  if(detailed){
    ref_df <- hcup.data::CCS_labels
    ccs %>%
      purrr::map(function(category){
        df <- ref_df[ref_df$CCS==ccs,]

        t(df)[,1]
      })
  }

}
# DX37, PR28, PR-5.6, DX-9.6.3
