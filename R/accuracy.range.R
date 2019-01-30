accuracy.range <- function(data, group, EE, behavior,time=10000) {
  # This function calculate the upperbround, actual and lower bound of deviation from Empirical Expectation 
  
  # calculate avg by group 
  require("dplyr")
  group <- enquo(group)
  EE <- enquo(EE)
  behavior <- enquo(behavior)
  df <- data %>% 
    # !! tells dplyr not to compute the object as a quosure
    select(!!group,!!EE,!!behavior)  %>%
    group_by(!!group) %>% 
    mutate(avg = mean(!!behavior,na.rm = TRUE)) %>% 
    ungroup()
  
  # calculate actual deviation -> add to df
  df <- df %>% mutate(dv = !!EE/10 - avg)  # convert to [0,1] scale
  
  
  # calculate actual absolute deviation -> add to df
  df <- df %>% mutate(dvab = abs(dv))  
  
  # calculate upper bound
  dvab_upper <- rep(NA,time)
  for(i in 1:time)
  {
    dvab_upper[i] <- abs(sample(0:10,1)/10 - sample(df$avg,1))
  }
  
  
  # calculate lower bound
  lower.df <- df %>%
    group_by(!!group) %>%
    summarise(count =n(), avg = mean(!!behavior,na.rm = TRUE)) %>%
    mutate(dv_lower = sqrt(avg * (1 - avg) / count))
  
  
  # summary table of mean median 
  row.names <- c("lower bound_ab","actual_ab","upper bound_ab","actual")
  col.names <- c("mean","median")
  matrix.names <-("summary of accuracy range")
  list <- c(mean(lower.df$dv_lower),
            mean(df$dvab),
            mean(dvab_upper),
            mean(df$dv), 
            median(lower.df$dv_lower),
            median(df$dvab),
            median(dvab_upper),
            median(df$dv))
  summary <- as.table(array(list, dim = c(4,2), dimnames = list(row.names, col.names)))
  
  return(summary)
} 
