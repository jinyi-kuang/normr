#' reapply labels to SPSS dataset
#'
#' reapply labels to SPSS dataset
#' @param x spss dataset
#' @export

relabelSpssVariable <- function(x) {
  a <- base::attr(x = x, "labels")
  if(!is.null(a)) {
    labels = base::names(a)
    levels = base::as.character(a)
    base::factor(x = x, levels = levels, labels = labels, ordered = TRUE)
  } else {
    warning("x is not label. No relabel
            ling done.")
    x
  }
}
