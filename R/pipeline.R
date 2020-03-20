#' Argument 2: environment and interact
#'
#' Code to reproduce analysis for 1st argument and figure 2.
#'
#' @export


pipeline <- function() {
  #
  info_msg("Argument 1")
  scr_arg1()
  #
  info_msg("Argument 2")
  scr_arg2()
  #
  info_msg("Argument 3")
  scr_arg3()
  #
  info_msg("Argument 6")
  scr_arg6()
  invisible(NULL)
}