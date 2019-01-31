correlogram.dist <- function(shapefile, psuname ){
  
  # This function calculate the distance between neighbors within psu 
  # Input: a df with var name long , lat 
  # output: matrix with distance summary for each hh 
  require("dplyr")
  require("spdep")
  require("lazyeval")
  require("magicfor")
  require("ggplot2")
  require("sp")

  psu <- subset(shapefile, "psuname" == psuname)
  
  dlist <- seq(from = 0.02, to = 0.32, by = 0.005)
  # lexical scoping - when use function within a function call for parent's env
  # in  this case is the globle env. therefore the df supplied to the function is 
  # the name from the global env
  with(parent.frame(), {  
    

    magic_for()
    for (i in dlist) {
      coeff <- coeff.dist(psu,distance=i)[1]
      cilow <- coeff.dist(psu,distance=i)[2]
      cihigh <- coeff.dist(psu,distance=i)[3]
      distance <- coeff.dist(psu,distance=i)[4]
    
      put(coeff, cilow, cihigh, distance)
    }
  
  
    gg <- magic_result_as_dataframe() %>%
      mutate(distance = round(i,4)) %>%
      ggplot(.) +
        geom_point(aes(x=distance, y= coeff)) +
        geom_errorbar(aes(x = distance, ymin = cilow, ymax=cihigh)) +
        theme(strip.text = element_text(size = 7, margin = margin(0.2,0.2,0.2,0.2))) +
        theme(strip.background = element_rect(colour = 'gray', fill = 'gray')) +
        scale_y_continuous(labels=scaleFUN, limits = c(-1,1)) +
        scale_x_continuous(labels=scaleFUN) +
        labs( x = "Distance Band", y = "Correlation Coefficent") +
        theme(legend.title=element_blank()) 
        
      
  
    magic_free()
  })
  
  gg <- gg +
    labs(title =paste0("Slum ", psuname))
  #  labs(title =paste0(" Correlogram of Household x Neighborhoods Use on Slum ", psuname))
  
  return(gg)
  
  # example call: gg_slum1 <- correlogram.dist(sp,1)
}