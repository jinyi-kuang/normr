densePlotIdv <- function(data, dfBoxplot, dv, xlabel = "Proportion of Community Whom Respondent Believes Uses a Toilet") {
  # This function return a ggplot2 object show the ee density ridge plot  
  
  
  # for boxplot
  # 1. find the density distribution 
  # 2. find the value of the data point 25 50 75 percentail of the distribution (not the data) 
  # 3. take the value back to the density plot
  
  # quote variable
  require("dplyr")
  require("ggplot2")
  dv <- enquo(dv)
  
  # create df--------------------------------------------------
  w2 <- data %>%
    dplyr::select(!!dv,"psuname", "psutype")  %>%
    dplyr::mutate(O4 = (!!dv)/10)  

  
  
  
  # plot ----------------------------------------------------
  
  
  df <- w2 %>% # for individual
    group_by(psutype) %>%
    dplyr::summarize(mee = mean(O4,na.rm = T), n=n())
  
  
  g <- ggplot(data = w2, aes(y = O4, x = psutype, fill = psutype)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, bw = 0.1, trim = F) +
    geom_boxplot(inherit.aes = F,data = dfBoxplot, aes(x = psutype, fill = psutype, ymin = value0, lower = value25/10, middle = value50/10, upper = value75/10, ymax = value100/10), stat = "identity", width = .1, outlier.shape = NA, alpha = 0.1) + 
    # geom_point(aes(y = O4/10, color = psutype), position = position_jitter(width = .04, height = 0.05), size = .3, alpha = 0.2) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
    
    guides(fill = FALSE) +
    guides(color = FALSE) +
    scale_colour_manual(values  = mycolors) +
    scale_fill_manual(name = "Geography",  
                      values = mycolors) +
    labs(title="", 
        y = xlabel ,
        x = ""
       ) +
    
    coord_flip() +
    theme_few() +
    theme(plot.margin = unit(c(1,1,1,0), "cm"))
  
 return(g)
  
}
