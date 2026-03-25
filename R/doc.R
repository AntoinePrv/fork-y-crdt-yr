#' @include extendr-wrappers.R
NULL

#' @export
print.Doc <- function(self, ...) {
  cat(self$to_string(), "\n", sep = "")
  invisible(self)
}

Doc$with_transaction <- function(callback, mutable = FALSE) {
  transaction <- Transaction$lock(self, mutable = mutable)
  on.exit(transaction$unlock())
  callback(transaction)
}
