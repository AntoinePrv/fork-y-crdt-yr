#' @include extendr-wrappers.R
NULL

#' @export Doc
NULL

#' @export
print.Doc <- function(self, ...) {
  cat(self$to_string(), "\n", sep = "")
  invisible(self)
}

Doc$with_transaction <- function(callback, mutable = FALSE, origin = NULL) {
  transaction <- Transaction$lock(self, mutable = mutable, origin = origin)
  on.exit(transaction$unlock())
  callback(transaction)
}
