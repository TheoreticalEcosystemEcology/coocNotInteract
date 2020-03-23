#' Argument 2: environment and interact
#'
#' Code to reproduce analysis for 1st argument and figure 2.
#'
#' @export


pipeline <- function() {
  #
  set.seed(7891)
  #
  msgInfo("Argument 1")
  scr_arg1()
  #
  msgInfo("Argument 2")
  scr_arg2()
  #
  msgInfo("Argument 3")
  scr_arg3()
  #
  msgInfo("Argument 4")
  scr_arg4()
  #
  msgInfo("Argument 6")
  scr_arg6()
  invisible(NULL)
}