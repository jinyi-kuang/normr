glm.alt <- function(data, y, qlo=0.25,qhi=0.75) {
  # This function calculate the effect size of logit  model 
  # personal normative belief ~ alter attibutes
  # pnb is codes as 1=wrong, 0 = not wrong 
  # effect size default is  (75 percentile - 25 percentile)
  # the output is numeric vector
  
  
# quote variable
  require("dplyr")
  y <- enquo(y)
  
  
# !! tells dplyr not to compute the object as a quosure

# 1 prepare for lm
  df <- data %>%
    select(!!y,"agemedian", "female",  "richer", "sc","hindu","livetogether", "seeeveryday","n", "od_od", "geography","state", "od_psuname")  %>%  # select regressors
    mutate(pnb = !!y)  # convert to [0,1] scale

  
# 2 regression model
  glm <- glm(pnb ~ agemedian + female  +richer + sc+ hindu + livetogether + seeeveryday+ n + od_od + od_psuname, data = df, family = binomial(link  = "logit"),na.action=na.omit)

# 3 calcualte qlo / qhi
# 4 margin
require(ggeffects)
effect <- sapply(df[2:8], function(x){
      lo <- as.numeric(quantile(x, probs = qlo))
      hi <- as.numeric(quantile(x, probs = qhi))
      sapply(names(df)[2:8],function(z){
          marginhi <- as.numeric(ggaverage(glm,terms = z)[hi+1,2])
          marginlo <- as.numeric(ggaverage(glm,terms = z)[lo+1,2])
          eff <- marginhi - marginlo
          }
      )
    }
  )[,1]

  return(effect)
}
