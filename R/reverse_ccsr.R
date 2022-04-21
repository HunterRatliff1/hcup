#' Look up ICD-10 codes based on CCSR categories
#'
#' Provided with a vector of CCSR categories, this will identify the ICD-10
#' codes that match that category. It returns a named list of ICD-10 codes
#' that can be used with the \code{\link[icd:comorbid]{icd package}} to
#'
#'
#'
#' @param CCSR_codes A vector of CCSR codes
#'
#' @return A named list with a depth of one and the same length as `CCSR_codes`, with
#'   names corresponding to the provided `CCSR_codes`. Each element of the list is a
#'   character vector of ICD-10 codes corresponding to the CCSR_code. Invalid or missing
#'   `CCSR_codes` are set to NA
#' @export
#'
#' @examples
#' reverse_ccsr("DIG001")
#' reverse_ccsr("CAR009")
#'
#' reverse_ccsr(c("DIG001", "CAR009"))
#' \dontrun{
#' library(icd)
#' library(dplyr)
#'
#' # Example data from ICD package
#' df <- icd::uranium_pathology %>%
#'   mutate(icd10 = decimal_to_short(icd10),
#'          icd10 = as.character(icd10)))
#'
#' # Categories of interest
#' CCSR_categories <- c("RSP008", "CIR009", "DIG019", "NEO070")
#' explain_ccsr(CCSR_categories)
#'
#' # Create mapping
#' CCSR_map <- reverse_ccsr(CCSR_categories)
#'
#' # Map comorbidities
#' result <- icd::comorbid(df, CCSR_map, return_df = T)
#' }
reverse_ccsr <- function(CCSR_codes){
  if(!rlang::inherits_any(CCSR_codes, "character")) {
    bad_classes <- glue::glue_collapse(glue::backtick(class(CCSR_codes)), sep = ", ", last = " and ")
    msg <- glue::glue("`CCSR_codes` should be a character, but provided variable is {bad_classes}")
    rlang::abort(msg)
  }


  # Make look up table to identify which CCSR is used
  CCSR_DX <- hcup.data::CCSR_DX_categories %>%
    dplyr::select(dplyr::starts_with("CCS")) %>%
    dplyr::mutate(vers = "dx10")
  CCSR_PR <- hcup.data::CCSR_PR_categories %>%
    dplyr::select(dplyr::starts_with("CCS")) %>%
    dplyr::mutate(vers = "pr10")

  version_tbl <- dplyr::bind_rows(CCSR_DX, CCSR_PR)
  rm(CCSR_DX, CCSR_PR)

  lookup_codes <- function(category){

    # Pass message notifying user that there were NA values
    if(is.na(category)) {
      rlang::inform("Provided NA values, returning NA")
      return(NA)
    }

    # Look up the code from the version_tbl to identify
    # the version to use
    x <- version_tbl[version_tbl$CCSR==category,]

    # Warn if code couldn't be found
    if(nrow(x)==0){
      msg <- glue::glue("The CCSR category '{category}' could not be found!")
      rlang::inform(msg)
      if(grepl("^(PR)|(DX)", category)){
        msg <- glue::glue("Code '{category}': Did you mistakenly provide a CCS category?")
        rlang::warn(msg)
      }
      return(NA)
    }

    vers <- x[["vers"]]

    if(vers == "dx10"){
      codes <- hcup.data:::CCSR_DX_tidy %>%
        dplyr::filter(.data$CCSR==category) %>%
        dplyr::pull(.data$I10_DX) %>%
        base::unique()
    }

    if(vers == "pr10"){
      codes <- hcup.data::CCSR_PR_mapping %>%
        dplyr::filter(.data$CCSR==category) %>%
        dplyr::pull(.data$I10_PR) %>%
        base::unique()
    }

    # rlang::`%@%<-`(codes, name=CCSR_desc, value=x[["CCSR_desc"]])
    return(codes)
  }

  l <- purrr::map(CCSR_codes, lookup_codes)
  names(l) <- CCSR_codes
  l
}
if(FALSE){

  load_all()
  c("DIG001", "CAR009") %>% reverse_ccsr()
  c("DIG001", NA, "CAR009") %>% reverse_ccsr()
  c("DIG001", "DIG001", "CAR009") %>% reverse_ccsr()
  c("DIG001", "PR-1", "CAR009") %>% reverse_ccsr()
  reverse_ccsr("DIG001")
  reverse_ccsr("CAR009")
  # dplyr::tibble(codes = reverse_ccsr("DIG001")) %>%
  #   dplyr::mutate(CCSR = ccsr_dx(codes)) %>%
  #   View()

  c("RSP008", "CIR009", "DIG019", "NEO070") %>% explain_ccsr()



}



