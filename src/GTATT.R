# Import packages
library(tidyverse)
library(lubridate)
library(did)

# Import cleaned data
df <- readRDS("../cleaned_data/finalmerged.rds")

# Remove all observations where year > repeal year
df1 <- df %>%
  filter(is.na(repeal_year) | YEAR <= repeal_year) %>%
  filter(ASECWT != 0) %>%
  mutate(RACE = factor(RACE))

colnames(df1)

# For never-treated units, we set treat_start_year to be 0, to be consistent with how the package wants it
df1$treat_start_year[is.na(df1$treat_start_year)] <- 0

# Group-time ATT for never-treated
att_cs_NT <- att_gt(
  yname   = "birth_lastyear",
  tname   = "YEAR",
  gname   = "treat_start_year",
  xformla = ~ AGE + NCHILD + EDUC + MARST + ELDCH + UNEMPLOYMENTRATE + median_weekly_wage,
  control_group = "nevertreated",
  weightsname = "ASECWT",
  data = df1,
  panel = F, # we are using repeated cross-sectional data
  anticipation = 0,
  clustervars = "STATEFIP",
  est_method = "reg"
)

summary(att_cs_NT)

agg_dyn_NT <- aggte(att_cs_NT, type = "dynamic")
summary(agg_dyn_NT)

ggdid(agg_dyn_NT)

# Group-time ATT for never-treated
att_cs_NYT <- att_gt(
  yname   = "birth_lastyear",
  tname   = "YEAR",
  gname   = "treat_start_year",
  xformla = ~ AGE + NCHILD + EDUC + MARST + ELDCH + factor(RACE),
  control_group = "notyettreated",
  weightsname = "ASECWT",
  data = df1,
  panel = F, # we are using repeated cross-sectional data
  anticipation = 0,
  clustervars = "STATEFIP",
  est_method = "dr"
)

summary(att_cs_NYT)



# Group aggregation
agg_cs <- aggte(att_cs, type = "group")
summary(agg_cs)

# Dynamic aggregation
agg_dyn <- aggte(att_cs, type = "dynamic")
summary(agg_dyn)

# Event-study plot
ggdid(agg_dyn)


ggdid(agg_dyn)

df2 <- df1 %>%
  select(YEAR, STATE, ever_treated, treat_start_year, treated, UNEMPLOYMENTRATE, median_weekly_wage, everything())


