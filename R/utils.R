
#' @importFrom rlang .data
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x
# https://design.tidyverse.org/def-short.html


#' @param icd_codes Vector of ICD codes
#' @param ref_df Data.frame with use to look up
#' @param data_col Single string matching the name of the column in
#'   `ref_df` that should contain the vector of codes `icd_codes`
#'
#' @importFrom rlang !!
#' @importFrom rlang :=
#' @keywords internal
lookup_table <- function(icd_codes, ref_df, data_col){
  stopifnot(rlang::inherits_any(ref_df, "data.frame"))
  stopifnot(data_col %in% names(ref_df))

  ## If was given a single value, use a filter, otherwise,
  ## look up using left-join
  if(length(icd_codes)==1) {
    # not sure if this actually helps with speed...
    ref_df[ref_df[[data_col]]==icd_codes,]
  } else({
    d <- data.frame(x = icd_codes)
    names(d)[1] <- data_col
    dplyr::left_join(d, ref_df, by = data_col)
  })
}



#' Warn if decimals in ICD codes
#'
#' Internal function to warn users if their ICD codes aren't in the
#' right format (i.e. have decimals)
#'
#' @param icd_codes Vector of ICD codes that should NOT have decimals
#' @keywords internal
warn_decimals <- function(icd_codes){
  has_decimals <- any(grepl(pattern = "\\.", x = icd_codes))
  if(has_decimals){
    warning("It looks like your ICD codes might be in decimal format. ",
            "Please change them to short format.\n",
            "You can use `icd::decimal_to_short()` to do this", call. = F)
  }
}


#' Provide a message if heuristic is used
#'
#' @param var_name Variable that was not supplied
#' @param default The default value to be used
#' @keywords internal
heuristic_msg <- function(var_name, default){
  # message("No default value provided for `", var_name, "`. Using ")
  rlang::inform(message = stringr::str_glue("No value provided for `{var_name}`. Using `{var_name} = {default}` as the default"))
  rlang::inform(message = stringr::str_glue("You can specify `{var_name} = {default}` to silence this alert"),
                .frequency="regularly", .frequency_id=var_name)
  default
}

