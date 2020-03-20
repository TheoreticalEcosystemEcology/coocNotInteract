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
  info_msg("Argument 3")
  scr_arg3()
  success_msg("Figure 3 done!")
  #
  info_msg("Argument 6")
  scr_arg6()
  success_msg("Figure 6 done!")
  invisible(NULL)
}