densityPlotUnit <- function(data,level, unit, x,title = NULL, xlab = NULL, caption = NULL, legendTitle = NULL, legendLabel = NULL) {
  #` Plot the probability density in Unit level (e.g., psu) with the level(e.g., geography type) mean as vline
  
  # Sez suggested color red-orange-gray
  reportcolor <- c("#f04c40", "#fbac49", "#666666")
  # jk suggestion red-blue-gray 
  mycolor <- c("#2081f9", "#fbac49", "#bbbbbb")  
  
  
  # enquo columns
  require("dplyr")
  require("ggplot2")
  require("ggthemes")
  
  level <- enquo(level)
  unit <- enquo(unit)
  x <- enquo(x)
  df <- data %>% select(!!level,!!unit, !!x) 
  
  
  df1 <- df %>% # for unit mean - x
    group_by(level,unit) %>% 
    summarize(m = mean(x,na.rm = T)) 
  
  df2 <- df1 %>% # for level mean - vline
    group_by(level) %>%
    summarize(m = mean(m,na.rm = T))
  
  
  df <- data %>% 
    # !! tells dplyr not to compute the object as a quosure, not execute 
    select(!!group,!!x)  
  
  gg <- ggplot(df1, aes(m, fill = level, colour = level)) +
    geom_density(alpha = 0.4, adjust = 0.75) +
    geom_vline(data = df2, aes(xintercept=m),color = mycolor) +
    guides(color=FALSE) +
    scale_x_continuous(limits = c(0,1), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_colour_manual(values  = mycolors) +
    scale_fill_manual(name = legendTitle,  
                      labels = legendLabel,
                      values = mycolor) +
    labs(title=title, 
         x = xlab,
         y = "Density", 
         caption = caption) +
    theme_few() +
    theme(plot.caption=element_text(hjust=0,  margin = margin(t=10))) 
  
  return(gg)
} 
