accuracy.idv <- function(data, group, EE, behavior,norm) {
  # This function calculate the individual level ee accuracy 
  
  # calculate avg by group 
  require("dplyr")
  group <- enquo(group)
  EE <- enquo(EE)
  behavior <- enquo(behavior)
  df <- data %>% 
    # !! tells dplyr not to compute the object as a quosure
    select("UniqueID",!!group,!!EE,!!behavior, )  %>%
    mutate(cluster = !!group) %>%
    group_by(!!group) %>% 
    mutate(avg = mean(!!behavior,na.rm = TRUE)) %>% 
    ungroup()
  
  # calculate actual deviation -> add to df
  df <- df %>% mutate(dv = !!EE/10 - avg)  # convert to [0,1] scale
  
  
  # calculate actual absolute deviation -> add to df
  df <- df %>% mutate(dvab = abs(dv))
  
  # normalize the abdv
  df <- df %>% mutate(z = (dvab - mean(dvab, na.rm = T))/sd(dvab, na.rm = T))
  
  # calculate absolute value of z
  df <- df %>% mutate(zab = abs(z))

  # rename column name
  colnames(df) <- paste(norm, colnames(df), sep = "_")
  names(df)[1] <- "UniqueID"
  return(df)
}
