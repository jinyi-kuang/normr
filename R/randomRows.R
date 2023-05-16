#' Generate random rows from a dataframe
#'
#' Generate random rows from a dataframe
#' @param n number of random rows


randomRows = function(df,n){
  return(df[sample(nrow(df),n),])
}
