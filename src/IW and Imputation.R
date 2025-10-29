library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(fixest)
library(didimputation)

##IW Estimator
df <- readRDS("../cleaned_data/finalmerged.rds")
df1 <- df %>%
  filter(is.na(repeal_year) | YEAR <= repeal_year)
df1$treat_start_year[is.na(df1$treat_start_year)] <- 0

m_sa <- feols(
  birth_lastyear ~ sunab(treat_start_year, YEAR) + AGE + EDUC + RACE + MARST |
    STATEFIP + YEAR,
  data = df1,
  weights = ~ASECWT,
  cluster = ~STATEFIP
)

summary(m_sa)

iplot(m_sa, ref.line = 0,
      xlab = "Event Time (Years)",
      main = "Event-Study Estimates (Sun & Abraham, 2021)")

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
