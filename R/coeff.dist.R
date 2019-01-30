coeff.dist <- function(data, distance=0.01) {
  
  # This function calculate the distance between neighbors within psu 
  # Input: a df with var name long , lat 
  # output: matrix with distance summary for each hh 
  require("dplyr")
  require("spdep")
  require("lazyeval")
  
  psu <- data
  
  # where n1 is the minimum distance threshold (commonly set to 0) and n2 is the maximum distance at which polygons will be considered neighbors.
  
  # create coords object 
  coords <- coordinates(psu)
  
  # start with given distance
  db_nb <- dnearneigh(coords, d1 = 0, d2 = distance , longlat = T)
  
  # compute weight of neighbor inversly proportion to distance
  
  db_weight <- nbdists(db_nb, coords)
  
  # convert nb to spatial weight object, assign equal weight
  db_lw <- nb2listw(db_nb, style = "W", zero.policy=T)
  
  # calculate the spatial lagged value (average neighbor's behavior (od=1) X weight) - this is the proportion of people od in their neighbors 
  db_lag <- lag.listw(db_lw, psu$use, NAOK=T)
  
  # test for correlation coefficent
  d50 <- cor.test(psu$use, db_lag, na.action = na.omit, method="pearson")
  
  #coefficient 
  coeff <- d50$estimate[1][1]
  # CI low
  cilow<-d50$conf.int[1:2][1]
  # CI high
  cihigh<-d50$conf.int[1:2][2]
  
  # out is a matrix 
  out <-cbind(distance,coeff,cilow,cihigh)
  
  out <- cbind(d50$estimate[1][1],
               d50$conf.int[1:2][1],
               d50$conf.int[1:2][2],
               distance)
  
  return(out)
}