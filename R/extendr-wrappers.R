# nolint start

#' @keywords internal
#' @useDynLib yar, .registration = TRUE
"_PACKAGE"

#' Return string `"Hello world!"` to R.
#' @export
hello_world <- function() .Call(wrap__hello_world)

# nolint end
