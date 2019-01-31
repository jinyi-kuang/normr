distance.summary <- function(df, col_name, value) {
  # This function calculate the distance between neighbors within psu 
  # Input: a df with var name long , lat 
  # output: matrix with distance summary for each hh 
  require("dplyr")
  require("geosphere")
  require("lazyeval")
  
  filter_criteria <- interp(~y == x, .values=list(y = as.name(col_name), x = value))
 
  dist <-  df %>%
    filter_(filter_criteria) %>%
    dplyr::select ("long", "lat") 
    
 # distm use dhav by default to calculate distance matrix
  dhav <- distHaversine(c(0,0), coords)
#  The unit is the same as radius of the earth in meters default = 6378137 m
  dhav <- distm(dist)
#  calculate mean, median,
  dstat <- as.data.frame(dhav)
  dstat[dstat == 0] <- NA

  hh_dist  <- dstat %>%
    summarise_at(vars(names(dstat)),
                 funs(n(),
                   sum(.,na.rm = T),
                   median(.,na.rm=T),
                   max(.,na.rm = T),
                   min(., na.rm = T),
                   quantile(., 0.90, na.rm = T))) %>%
    gather() %>%
    separate(key, into =c("id", "stat"),sep="\\_") %>%
    spread(stat, value) %>%
    mutate(avg = sum/(n-1))
  
  return(hh_dist)
  
  # call: psu1 <- correlogram.dist(shapefile=sp, col_name="psuname",value=1 )

}

