accuracy.ci <- function(data, group, EE, behavior,time=10000) {
  # This function calculate the upperbround, actual [with ci] and lower bound of deviation from Empirical Expectation clustered by psu 
  
  # calculate avg by group 
  require("dplyr")
  group <- enquo(group)
  EE <- enquo(EE)
  behavior <- enquo(behavior)
  df <- data %>% 
    # !! tells dplyr not to compute the object as a quosure
    select(!!group,!!EE,!!behavior)  %>%
    mutate(cluster = !!group) %>%
    group_by(!!group) %>% 
    mutate(avg = mean(!!behavior,na.rm = TRUE)) %>% 
    ungroup()
  
  # calculate actual deviation -> add to df
  df <- df %>% mutate(dv = !!EE/10 - avg)  # convert to [0,1] scale
  
  
  # calculate actual absolute deviation -> add to df
  df <- df %>% mutate(dvab = abs(dv))  
  
  
  #calculate actual CI for dv and dvab -> one stage cluster design
  library(survey)
  mydesign <- svydesign(ids = ~cluster, data = df)  # initiate one-stage clustering survey design object
  svy <- svymean(~dv, mydesign)  # mean and standard error of dv dvab
  svya <- svymean(~dvab, mydesign)
  cidv <- svyciprop(~dv, mydesign, method = "me", level = 0.95) # CI dv
  cidvab <- svyciprop(~dvab, mydesign, method = "me", level = 0.95) # CI dvab
  n <- unwtd.count(~dvab, mydesign)
  npsu <- degf(mydesign) + 1
  
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
  
  
  # summary table of mean  ci  
  row.names <- c("lower_bound","upper_bound","mean_deviant","svy","cilo","ciup","mean_absolute_deviant","svya", "acilo","aciup", "npsu","n")
  col.names <- (names(EE))
  matrix.names <- ("summary of accuracy range")
  
  list <- c(mean(lower.df$dv_lower),
            mean(dvab_upper),
            mean(df$dv),
            as.numeric(svy),
            as.numeric(as.data.table(attributes(cidv)["ci"])[1]),
            as.numeric(as.data.table(attributes(cidv)["ci"])[2]),
            mean(df$dvab),
            as.numeric(svya),
            as.numeric(as.data.table(attributes(cidvab)["ci"])[1]),
            as.numeric(as.data.table(attributes(cidvab)["ci"])[2]),
            as.numeric(npsu),
            as.numeric(n)
            )
  
  summary <- array(list, dim = c(12,1), dimnames = list(row.names, col.names))
  
  return(summary)
} 
