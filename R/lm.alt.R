lm.alt <- function(data, y, qlo=0.25,qhi=0.75) {
# This function calculate the effect size of linear regression model
# empirical expectation / normative expectation ~ alter attributes
# ee is scaled [0,1]
# effect size default is  (75 percentile - 25 percentile)
# the output is numeric vector

# quote variable
  require("dplyr")
  y <- enquo(y)


# !! tells dplyr not to compute the object as a quosure

# 1 prepare for lm
  df <- data %>%
    select(!!y,"agemedian", "female", "richer", "sc","hindu","livetogether", "seeeveryday","n", "od_od", "geography","state", "od_psuname")  %>%  # select regressors
    mutate(ee = !!y/10)  # convert to [0,1] scale


# 2 regression model
  lm <- lm(ee ~ agemedian + female  +richer + sc+ hindu + livetogether + seeeveryday+ n + od_od  + od_psuname, data = df, na.action=na.omit)

# 3 calcualte qlo / qhi
# 4 margin
require(ggeffects)
effect <- sapply(df[2:8], function(x){
      lo <- as.numeric(quantile(x, probs = qlo))
      hi <- as.numeric(quantile(x, probs = qhi))
      sapply(names(df)[2:8],function(z){
          marginhi <- as.numeric(ggaverage(lm,terms = z)[hi+1,2])
          marginlo <- as.numeric(ggaverage(lm,terms = z)[lo+1,2])
          eff <- marginhi-marginlo
          }
      )
    }
  )[,1]

  return(effect)
}
