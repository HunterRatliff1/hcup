
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

  if(is.factor(icd_codes)) {
    rlang::warn("ICD codes should be characters, not factors")
    icd_codes <- as.character(icd_codes)
  }

  ## If was given a single value, use a filter, otherwise,
  ## look up using left-join
  if(length(icd_codes)==1) {
    # not sure if this actually helps with speed...
    df <- ref_df[ref_df[[data_col]]==icd_codes,]

  } else({
    d <- data.frame(x = icd_codes)
    names(d)[1] <- data_col
    df <- dplyr::left_join(d, ref_df, by = data_col)
  })

  # Use waldo to check that new df matches old
  comp <- waldo::compare(icd_codes, df[[data_col]],
                         x_arg = "ICD codes provided",
                         y_arg = "Matches in lookup table")

  # Abort if not matching
  if(length(comp)>0){
    msg <- glue::glue("The look up table didn't come back right ",
                      "when they were cross-referenced to the ",
                      "{rlang::expr_label(base::substitute(data_col))} ",
                      "column of the ",
                      "{rlang::expr_label(base::substitute(ref_df))}")
    rlang::inform(comp)
    rlang::warn(msg)
  }



  df
}



#' Check that the ICD codes are in the correct format
#'
#' Internal function to warn users if their ICD codes aren't in the
#' right format (i.e. have decimals, not character)
#'
#' @param icd_codes Vector of ICD codes that should NOT have decimals
#' @keywords internal
check_icd_format <- function(icd_codes){
  # formerly known as `warn_decimals`

  # labellise the function argument `icd_codes`
  labellised <- rlang::expr_label(base::substitute(icd_codes))

  # Check conditions
  has_decimals <- any(grepl(pattern = "\\.", x = icd_codes))
  is_vct       <- rlang::is_vector(icd_codes)
  is_char      <- rlang::is_character(icd_codes)
  is_fct       <- rlang::inherits_any(icd_codes, "factor")

  # If not a vector, fail
  if(!is_vct){
    msg <- glue::glue("{labellised} is not a vector, it's a {class(icd_codes)}")
    rlang::abort(msg)
  }

  # If not char or factor, fail
  if(!is_char & !is_fct){
    msg <- glue::glue("{labellised} is not a character vector, it's a {class(icd_codes)}")
    rlang::abort(msg)
  }

  # If is a factor, warn & coerce to character
  if(is_fct){
    msg <- glue::glue("{labellised} is a factor, coercing to character")
    rlang::warn(msg)
    icd_codes <- as.character(icd_codes)
  }

  # If has decimals, warn & regex them out
  if(has_decimals){
    rlang::warn("ICD codes should not be in decimal format")
    msg <- glue::glue("Will attempt to coerce decimals to short format, but ",
                      "you may want to use `icd::decimal_to_short()` to do this")
    rlang::inform(msg, .frequency = "regularly", .frequency_id="decimals")
    icd_codes <- purrr::map_chr(icd_codes, stringr::str_remove_all, "\\.")
  }
  icd_codes
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

