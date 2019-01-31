#' A margin plot show the VOI density distribution of a individual measure by groups
#'
#' It is written after getting the average predicted value from \link[normr]{AvgMargin}
#'
#' @param dfMargin A dateframe with margin value from \link[normr]{AvgMargin} dataframe.
#' @return A margin plot with specified value
#' @keywords plot

MarginPlot <- function(dfMargin, group = NULL,  ylabel = "Expected Probability", xlabel = "Level of voi") {
  

  
  # require pkg
  require("dplyr")
  require("ggplot2")
  require("ggthemes")

  df <- dfMargin
  # jk suggestion red-blue-gray
  mycolors <- c("#2081f9", "#fbac49", "#bbbbbb")
  # create df--------------------------------------------------
  
g <- ggplot(df, aes(x= x, y = predicted, group = group)) +
     geom_line(aes(color = group)) +
     geom_ribbon(mapping = aes(ymin = conf.low, ymax = conf.high, fill = NULL), alpha = 0.1) +
    scale_y_continuous(expand = c(0, 0), limits = c(0,1)) +
    scale_colour_manual(name = group, values  = mycolors) +
    scale_fill_manual(name = group, values = mycolors) +
    labs(title="", 
        y = ylabel,
        x = xlabel) +
    theme_few() +
    theme(plot.margin = unit(c(1,1,1,0), "cm"))

 return(g)
  
}
