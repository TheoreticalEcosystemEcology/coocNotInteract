#' Argument 2: environment and interact
#'
#' Code to reproduce analysis for 1st argument and figure 2.
#'
#' @export


pipeline <- function() {
  #
  info_msg("Argument 1")
  scr_arg1()
  success_msg("Figure 1 done!")
  #
  info_msg("Argument 2")
  scr_arg2()
  success_msg("Figure 2 done!")
  #
  invisible(NULL)
}