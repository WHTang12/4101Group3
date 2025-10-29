# Import packages
library(tidyverse)
library(lubridate)
library(did)

# Import cleaned data
df <- readRDS("../cleaned_data/finalmerged.rds")

# Remove all observations where year > repeal year
df1 <- df %>%
  filter(is.na(repeal_year) | YEAR <= repeal_year)

colnames(df1)

# For never-treated units, we set treat_start_year to be 0, to be consistent with how the package wants it
df1$treat_start_year[is.na(df1$treat_start_year)] <- 0

# Group-time ATT
att_cs <- att_gt(
  yname   = "birth_lastyear",
  tname   = "YEAR",
  gname   = "treat_start_year",
  xformla = ~ AGE + NCHILD + EDUC + RACE + MARST + INCWAGE + ELDCH,
  control_group = "nevertreated",
  weightsname = "ASECWT",
  data = df1,
  panel = F, # we are using repeated cross-sectional data
  anticipation = 0,
  clustervars = "STATEFIP"
)

summary(att_cs)

# Group aggregation
agg_cs <- aggte(att_cs, type = "group")
summary(agg_cs)

# Dynamic aggregation
agg_dyn <- aggte(att_cs, type = "dynamic")
summary(agg_dyn)

# Event-study plot
ggdid(agg_dyn)


ggdid(agg_dyn)