#' Generate standard error for continuous variable
#'
#' Generate standard error for continuous variable
#' @param x variable name
#' @export

se <- function(x, na.rm=T) {
  sd(x)/sqrt(length(x))
}
