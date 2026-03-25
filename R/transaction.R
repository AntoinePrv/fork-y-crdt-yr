#' @include extendr-wrappers.R
NULL

#' @export Transaction
NULL

#' @export Origin
NULL

#' @export
print.Origin <- function(self, ...) {
  cat(self$to_string(), "\n", sep = "")
  invisible(self)
}

#' @export
`==.Origin` <- function(e1, e2) {
  e1$equal(e2)
}

#' @export
`<.Origin` <- function(e1, e2) {
  e1$less_than(e2)
}

#' @export
`<=.Origin` <- function(e1, e2) {
  e1$less_than_equal(e2)
}

#' @export
`>.Origin` <- function(e1, e2) {
  e2$less_than(e1)
}

#' @export
`<=.Origin` <- function(e1, e2) {
  e2$less_than_equal(e1)
}
