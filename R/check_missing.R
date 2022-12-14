#' Check missing rate in variables.
#'
#' Check missing (\code{NA}) proportion or counts of variables. This function
#' works like \code{\link[dplyr]{summarize_at}} where the missing rate or
#' count for the selected columns are returned.
#'
#'
#' @inheritParams common_params
#' @param ... Pass to tidyselect. See [dplyr::dplyr_tidy_select] for details.
#' @param ret_prop Whether to return the rate of missing (default) or counts.
#'
#' @author Min Ma
#' @export
check_missing <- function(data, ..., ret_prop = TRUE) {
  if (ret_prop) {
    fun <- function(x) sum(is.na(x)) / n()
  } else {
    fun <- function(x) sum(is.na(x))
  }

  dplyr::summarise(data, dplyr::across(c(...), fun))
}

#' @importFrom dplyr n
#' @describeIn check_missing SE version of check_missing.
#' @export
check_missing_ <- function(data, ..., .dots, ret_prop = TRUE) {
  .Defunct("check_missing")
}
