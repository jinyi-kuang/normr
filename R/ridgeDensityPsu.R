#' A ridge plot show the VOI density distribution of a PSU level measure by groups
#'
#' \code{ridgeDensityPsu} A ridge plot show the VOI density distribution by groups
#' It is written for supplying value to be specified in the boxplot.
#' The boxplot value is calculated by the \link[normr]{densityQualtile}.
#'
#' @param data A dateframe with var "psutype" and "psuname".
#' @param dfBoxplot A dataframe contain value calcualted from \link[normr]{densityQualtile}..
#' @param dv Variable of interest to be calculated.
#' @return A plot show the VOI density ridge and boxplot with specified value
#' @keywords plot

ridgeDensityPsu <- function(data, dfBoxplot, dv, xlabel = "Proportion of Sampling Unit Who Used a Toilet For Last Defecation", captiontext = "Figure 2. Kernel density estimates and box plots with individual data points of sampling unit level prevalence \nof toilet use for last defecation. Data disaggregated by geography type.") {

  
  # quote variable
  require("dplyr")
  require("ggplot2")
  require("ggthemes")
  dv <- enquo(dv)
  
  # create df--------------------------------------------------
  w2 <- data %>%
    dplyr::select(!!dv,"psuname", "psutype")  %>%
    dplyr::mutate(voi = !!dv)  
  
  
  
  # plot ----------------------------------------------------
  
  
  
  g <- ggplot(data = w2, aes(y = voi, x = psutype, fill = psutype)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, bw = 0.1, trim = F) +
    geom_boxplot(inherit.aes = F,data = dfBoxplot, aes(x = psutype, fill = psutype, ymin = value0, lower = value25, middle = value50, upper = value75, ymax = value100), stat = "identity", width = .1, outlier.shape = NA, alpha = 0.1) + 
    geom_point(aes(y = voi, color = psutype), position = position_jitter(width = .00, height = 0.00), size = 3, shape = 108, alpha = 1) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
    scale_x_discrete(expand = c(0,0.2)) +
    
    guides(fill = FALSE) +
    guides(color = FALSE) +
    scale_colour_manual(values  = mycolors) +
    scale_fill_manual(name = "Geography",  
                      values = mycolors) +
    labs(title="", 
         y = xlabel ,
         x = "" ,
         caption = captiontext
         ) +
    
    coord_flip() +
    theme_few() +
    theme(plot.caption=element_text(hjust=0,  margin = margin(t=10))) +
    theme(plot.margin = unit(c(1,2,1,0), "cm"))
  
  return(g)
  
}
