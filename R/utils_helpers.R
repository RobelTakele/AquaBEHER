#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

docall <- function(fun, args) {
  if ((is.character(fun) && length(fun) == 1) || is.name(fun)) {
    fun <- get(as.character(fun), envir = .GlobalEnv, mode = "function")
  }
  do.call("fun", lapply(args, enquote))
}
