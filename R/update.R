#' @include extendr-wrappers.R
NULL

#' @export Update
NULL

#' @export
print.Update <- function(x, ...) {
  cat(x$to_string(), "\n", sep = "")
  invisible(x)
}
