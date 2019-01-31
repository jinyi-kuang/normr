#' Compute average marginal effects
#'
#' \code{AvgMargin} computes average predicted values based on the given data at specified level of voi
#'
#' @param data A dateframe with voi
#' @param model lm or glm model
#' @param voi Variable of interest to be calculated.
#' @param range The value of voi to be predicted. For factor, use levels(data$voi) or specify factor levels
#' @return dataframe of average predicted value at sepecified levels
#' @keywords margin

AvgMargin<- function(data, model, voi, range =c(0:10)) {

  # require pkg
  require("dplyr")
  require("prediction")

  
  # quote variable
  dv <- enquo(dv)
  
  # create df--------------------------------------------------
  df <- data %>%
    dplyr::select(!!voi)  %>%
    dplyr::mutate( dv = !!voi)
    
  # make prediction  --------------------------------------------------

  dfPrediction <- prediction::prediction(model, at = list(dv = range))
  
  # get mean  --------------------------------------------------

  dfMargin <- usePrediction %>%
    group_by(dv) %>%
    summarize(avg = mean(fitted),se = mean(se.fitted,na.rm=T))

 return(dfMargin)
  
}
