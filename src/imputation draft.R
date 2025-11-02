library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(didimputation)

##Imputation Estimator
df_state <- df1 %>%
  group_by(STATEFIP, YEAR, treat_start_year) %>%
  summarise(
    birth_rate = weighted.mean(birth_lastyear, ASECWT, na.rm = TRUE),
    AGE   = weighted.mean(AGE,   ASECWT, na.rm = TRUE),
    EDUC  = weighted.mean(EDUC,  ASECWT, na.rm = TRUE),
    RACE  = weighted.mean(RACE,  ASECWT, na.rm = TRUE),
    MARST = weighted.mean(MARST, ASECWT, na.rm = TRUE),
    .groups = "drop"
  )

bjs_results <- did_imputation(
  data        = df_state,
  yname       = "birth_rate",
  tname       = "YEAR",
  gname       = "treat_start_year",
  idname      = "STATEFIP",
  cluster_var = "STATEFIP"
)

summary(bjs_results)
