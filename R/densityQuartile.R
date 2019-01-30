#' Value of VOI at the Quartile of Its Density Distribution
#'
#' \code{densityQuartile} calculate value of VOI at the quartile of density.
#' It is written for supplying value to be specified in the boxplot.
#' The boxplot can be combined with density plot.
#' The calculation following 3 steps:
#'   Step1 find the density distribution.
#'   Step2 find the value of the data point at 0 25 50 75 100 percentile of the distribution.
#'   Step3 take the value back to the density plot.
#'
#' @param data A dataset with var "psutype" and "psuname".
#' @param dv Variable of interest to be calculated.
#' @param bandwidth see \link[stat]{density} bw.
#' @param geotype Use factor lable of "psutype" variable.
#' @return A martix contrains value of VOI at the qualtile of density.
#' @keywords density 

densityQuartile <- function(data, dv , bandwidth=1, geotype = "Urban") {

  # quote variable
  require("dplyr")
   dv <- enquo(dv)

  # create df--------------------------------------------------
  w2 <- data %>%
    dplyr::select(!!dv,"psutype", "psuname")  %>%
    dplyr::mutate(ee = !!dv)

  # psutype
  # 1	M.corp (Urban)
  # 2	Town Panchayat (Peri-urban)
  # 3	Gram Panchayat (Rural)



  # subset ---------------------------------------
  w2 <- subset(w2, psutype == geotype)

  # subset ---------------------------------------
  dense <- density(w2$ee,bw=bandwidth)
  densetable <- data.frame(dense$x,dense$y/sum(dense$y),cumsum(dense$y/sum(dense$y)))
  colnames(densetable) <- c("x","y","cum")
  value0  <- min(w2$ee)
  value25 <- densetable[which(abs(densetable$cum-0.25) == min(abs(densetable$cum-.25))),"x"]
  value50 <- densetable[which(abs(densetable$cum-0.5) == min(abs(densetable$cum-.5))),"x"]
  value75 <- densetable[which(abs(densetable$cum-0.75) == min(abs(densetable$cum-.75))),"x"]
  value100 <- max(w2$ee)
  mtx <- cbind(value0, value25,value50, value75, value100)
  return(mtx)

}
