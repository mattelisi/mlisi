#' Miscellaneous helpers
#'
#' Find most repeated value in a vector
#' @export

mostrepeated <- function(x) as(names(which.max(table(x))), mode(x)) 