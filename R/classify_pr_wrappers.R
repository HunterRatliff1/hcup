#' #' @describeIn classify_pr Returns `TRUE` if the
#' #'   procedure code is for a diagnostic procedure
#' #' @export
#' is_diagnostic_pr <- function(icd_pr){
#'   grepl(pattern = "Diagnostic", x = classify_pr(icd_pr))
#' }
#'
#' #' @describeIn classify_pr Returns `TRUE` if the
#' #'   procedure code is for a therapeutic procedure
#' #' @export
#' is_therapeutic_pr <- function(icd_pr){
#'   grepl(pattern = "Therapeutic", x = classify_pr(icd_pr))
#' }
#'
#' #' @describeIn classify_pr Returns `TRUE` if the
#' #'   procedure code is for a major procedure
#'
#' is_major_pr <- function(icd_pr){
#'   grepl(pattern = "Major", x = classify_pr(icd_pr))
#' }
#'
#' #' @describeIn classify_pr Returns `TRUE` if the
#' #'   procedure code is for a minor procedure
#' is_minor_pr <- function(icd_pr){
#'   grepl(pattern = "Minor", x = classify_pr(icd_pr))
#' }
