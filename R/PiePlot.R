#' PiePlot a var by group in df
#'
#' \code{PiePlot} take the categorical factor variables output a ggplot pie chart by category.
#'
#' This function is built upon ggplot2 pie chart function.

#` @param data A dataset.
#` @param group A var.
#` @param var A var.
#` @return a pie chart of \code{var} by \code{group}. `


pie.plot <- function(data, group, var) {



  # quote variable
  require("dplyr")
  group <- enquo(group)
  var <- enquo(var)


  # !! tells dplyr not to compute the object as a quosure

  # 1 prepare for plot
  df <- data %>%
    mutate(group = !!group, var=!!var)%>%
    select(group, var) %>%
    drop_na(var) %>%
    group_by(group,var) %>%
    count() %>%
    ungroup() %>%
    group_by(group)%>%
    mutate(per=`n`/sum(`n`)) %>%
    arrange(desc(var))

 # 2 plot pie chart
  gg <- ggplot(df) +
    geom_bar(aes(x="", y=per, fill=var), alpha=0.7, stat="identity", width = 1, position = "fill")+
    coord_polar("y", start=0)+
    theme(axis.text.x=element_blank()) +
    geom_text(aes(x=1, y = per, label=percent(per)), position = position_fill(vjust = 0.5)) +
    labs(title = attributes(df$var)$label,
         x = "", y= "") +
    scale_fill_discrete(guide=guide_legend(title="")) +
    facet_grid(.~group)

  return(gg)
}
