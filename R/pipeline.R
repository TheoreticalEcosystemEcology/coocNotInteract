#' Entire pipeline
#'
#'
#' @export

pipeline <- function() {
  #
  info_msg("Argument 1")
  scr_arg1()
  success_msg("Figure 1 done!")
  #
  invisible(NULL)
}