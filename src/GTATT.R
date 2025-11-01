# Import packages
library(tidyverse)
library(lubridate)
library(did)

# Import cleaned data
df <- readRDS("../cleaned_data/finalmerged.rds")

# Remove all observations where year > repeal year
df1 <- df %>%
  filter(is.na(repeal_year) | YEAR <= repeal_year) %>%
  filter(ASECWT != 0)

colnames(df1)

# For never-treated units, we set treat_start_year to be 0, to be consistent with how the package wants it
df1$treat_start_year[is.na(df1$treat_start_year)] <- 0

##### Main model (Using never-treated as control group) #####
# Group-time ATT for never-treated
att_cs_NT <- att_gt(
  yname   = "birth_lastyear",
  tname   = "YEAR",
  gname   = "treat_start_year",
  xformla = ~ AGE + NCHILD + EDUC + MARST + ELDCH + black + white + lag_unemployment_rate + lag_weekly_median_wage,
  control_group = "nevertreated",
  weightsname = "ASECWT",
  data = df1,
  panel = F, # we are using repeated cross-sectional data
  anticipation = 0,
  clustervars = "STATEFIP",
  est_method = "reg"
)

att_cs_NT <- readRDS("../modelresults/cs/att_cs_NT.rds") # instead of re-running, load this
summary(att_cs_NT)

agg_dyn_NT <- aggte(att_cs_NT, type = "dynamic")
agg_dyn_NT <- readRDS("../modelresults/cs/agg_dyn_NT.rds") # instead of re-running, load this
summary(agg_dyn_NT)
ggdid(agg_dyn_NT)

agg_simple_NT <- aggte(att_cs_NT, type = "simple")
agg_simple_NT <- readRDS("../modelresults/cs/agg_simple_NT.rds")
summary(agg_simple_NT)

# Saving so we don't have to rerun it and wait everytime
#saveRDS(att_cs_NT, "../modelresults/cs/att_cs_NT.rds")
#saveRDS(agg_dyn_NT, "../modelresults/cs/agg_dyn_NT.rds")
#saveRDS(agg_simple_NT, "../modelresults/cs/agg_simple_NT.rds")


##### Secondary model (Using not-yet-treated as control group) #####
# Group-time ATT for not-yet-treated
att_cs_NYT <- att_gt(
  yname   = "birth_lastyear",
  tname   = "YEAR",
  gname   = "treat_start_year",
  xformla = ~ AGE + NCHILD + EDUC + MARST + ELDCH + black + white + lag_unemployment_rate + lag_weekly_median_wage,
  control_group = "notyettreated",
  weightsname = "ASECWT",
  data = df1,
  panel = F, # we are using repeated cross-sectional data
  anticipation = 0,
  clustervars = "STATEFIP",
  est_method = "reg"
)

att_cs_NYT <- readRDS("../modelresults/cs/att_cs_NYT.rds") # instead of re-running, load this
summary(att_cs_NYT)

agg_dyn_NYT <- aggte(att_cs_NYT, type = "dynamic")
agg_dyn_NYT <- readRDS("../modelresults/cs/agg_dyn_NYT.rds")
summary(agg_dyn_NYT)
ggdid(agg_dyn_NYT)

agg_simple_NYT <- aggte(att_cs_NYT, type = "simple")
agg_simple_NYT <- readRDS("../modelresults/cs/agg_simple_NYT.rds")
summary(agg_simple_NYT)

#saveRDS(att_cs_NYT, "../modelresults/cs/att_cs_NYT.rds")
#saveRDS(agg_dyn_NYT, "../modelresults/cs/agg_dyn_NYT.rds")
#saveRDS(agg_simple_NYT, "../modelresults/cs/agg_simple_NYT.rds")

##### Testing sensitivity to different anticipatory behavior #####

# Creating a helper function to run models with different anticipation 
model_k <- function(k) {
  # the model
  att <- att_gt(
    yname   = "birth_lastyear",
    tname   = "YEAR",
    gname   = "treat_start_year",
    xformla = ~ AGE + NCHILD + EDUC + MARST + ELDCH + black + white + lag_unemployment_rate + lag_weekly_median_wage,
    control_group = "nevertreated",
    weightsname   = "ASECWT",
    data          = df1,
    panel         = FALSE,
    anticipation  = k,
    clustervars   = "STATEFIP",
    est_method    = "reg"
  )
  # dynamic aggregation
  agg <- aggte(att, type = "dynamic")
  
  # store every att and standard errors, for plotting later
  tibble(
    event_time = agg$egt,
    att        = agg$att.egt,
    se         = agg$se.egt,
    anticipation = k
  )
}

# Run model with anticipation = 1 to 5
k <- 1:5 
res <- map_dfr(k, model_k)

#saveRDS(res, "../modelresults/anticipation.rds")

res <- readRDS("../modelresults/cs/anticipation.rds") # instead of re-running, load this

anticipationResults <- res %>%
  mutate(
    upperbound = att + 1.96 * se,
    lowerbound = att - 1.96 * se,
    anticipation = factor(anticipation, levels = sort(unique(anticipation)))
  )

ggplot(anticipationResults, aes(event_time, att, color = anticipation, group = anticipation)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(ymin = lowerbound, ymax = upperbound, fill = anticipation), alpha = 0.15, color = NA) +
  geom_line(size = 1) +
  geom_point(size = 1.6) +
  labs(x = "Event time (years since treatment)",
       y = "ATT(g,t) — dynamic",
       color = "Anticipation (k)",
       fill  = "Anticipation (k)",
       title = "Event-study ATTs across anticipation windows") +
  theme_minimal(base_size = 12)

##### According to pre-trends diagnostics, we see that there is a considerable change in pre-trends for the 1997 group
# Therefore, we consider getting rid of 1997 group and see how the results differ

dfwithout1997 <- df1 %>%
  filter(treat_start_year != 1997)

att_cs_NT_without1997 <- att_gt(
  yname   = "birth_lastyear",
  tname   = "YEAR",
  gname   = "treat_start_year",
  xformla = ~ AGE + NCHILD + EDUC + MARST + ELDCH + black + white + lag_unemployment_rate + lag_weekly_median_wage,
  control_group = "nevertreated",
  weightsname = "ASECWT",
  data = dfwithout1997,
  panel = F, # we are using repeated cross-sectional data
  anticipation = 0,
  clustervars = "STATEFIP",
  est_method = "reg"
)

att_cs_NT_without1997 <- readRDS("../modelresults/cs/att_cs_NT_without1997.rds") # instead of re-running, load this
summary(att_cs_NT_without1997)

agg_dyn_NT_without1997 <- aggte(att_cs_NT_without1997, type = "dynamic")
agg_dyn_NT_without1997 <- readRDS("../modelresults/cs/agg_dyn_NT_without1997.rds") # instead of re-running, load this
summary(agg_dyn_NT_without1997)
ggdid(agg_dyn_NT_without1997)


##### Employment #####
##### Main model (Using never-treated as control group) #####
# Group-time ATT for never-treated
att_cs_NT_employment <- att_gt(
  yname   = "employed",
  tname   = "YEAR",
  gname   = "treat_start_year",
  xformla = ~ AGE + NCHILD + EDUC + MARST + ELDCH + black + white + lag_unemployment_rate + lag_weekly_median_wage,
  control_group = "nevertreated",
  weightsname = "ASECWT",
  data = df1,
  panel = F, # we are using repeated cross-sectional data
  anticipation = 0,
  clustervars = "STATEFIP",
  est_method = "reg"
)

summary(att_cs_NT_employment)

agg_dyn_NT_employment <- aggte(att_cs_NT_employment, type = "dynamic")
summary(agg_dyn_NT_employment)
ggdid(agg_dyn_NT_employment)

# Saving so we don't have to rerun it and wait everytime
#saveRDS(att_cs_NT_employment, "../modelresults/att_cs_NT_employment.rds")
#saveRDS(agg_dyn_NT_employment, "../modelresults/agg_dyn_NT_employment.rds")

##### Secondary model (Using not-yet-treated as control group) #####
# Group-time ATT for not-yet-treated
att_cs_NYT_employment <- att_gt(
  yname   = "employed",
  tname   = "YEAR",
  gname   = "treat_start_year",
  xformla = ~ AGE + NCHILD + EDUC + MARST + ELDCH + black + white + lag_unemployment_rate + lag_weekly_median_wage,
  control_group = "notyettreated",
  weightsname = "ASECWT",
  data = df1,
  panel = F, # we are using repeated cross-sectional data
  anticipation = 0,
  clustervars = "STATEFIP",
  est_method = "reg"
)

summary(att_cs_NYT_employment)

agg_dyn_NYT_employment <- aggte(att_cs_NYT_employment, type = "dynamic")
summary(agg_dyn_NYT_employment)
ggdid(agg_dyn_NYT_employment)

#saveRDS(att_cs_NYT_employment, "../modelresults/cs/att_cs_NYT_employment.rds")
#saveRDS(agg_dyn_NYT_employment, "../modelresults/cs/agg_dyn_NYT_employment.rds")

##### Testing sensitivity to different anticipatory behavior #####

# Creating a helper function to run models with different anticipation 
model_k_employment <- function(k) {
  # the model
  att <- att_gt(
    yname   = "employed",
    tname   = "YEAR",
    gname   = "treat_start_year",
    xformla = ~ AGE + NCHILD + EDUC + MARST + ELDCH + black + white + lag_unemployment_rate + lag_weekly_median_wage,
    control_group = "nevertreated",
    weightsname   = "ASECWT",
    data          = df1,
    panel         = FALSE,
    anticipation  = k,
    clustervars   = "STATEFIP",
    est_method    = "reg"
  )
  # dynamic aggregation
  agg <- aggte(att, type = "dynamic")
  
  # store every att and standard errors, for plotting later
  tibble(
    event_time = agg$egt,
    att        = agg$att.egt,
    se         = agg$se.egt,
    anticipation = k
  )
}

# Run model with anticipation = 1 to 5
k <- 1:5 
res_employment <- map_dfr(k, model_k_employment)

#saveRDS(res_employment, "../modelresults/cs/anticipation_employment.rds")


anticipationResults_employment <- res_employment %>%
  mutate(
    upperbound = att + 1.96 * se,
    lowerbound = att - 1.96 * se,
    anticipation = factor(anticipation, levels = sort(unique(anticipation)))
  )

ggplot(anticipationResults_employment, aes(event_time, att, color = anticipation, group = anticipation)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(ymin = lowerbound, ymax = upperbound, fill = anticipation), alpha = 0.15, color = NA) +
  geom_line(size = 1) +
  geom_point(size = 1.6) +
  labs(x = "Event time (years since treatment)",
       y = "ATT(g,t) — dynamic",
       color = "Anticipation (k)",
       fill  = "Anticipation (k)",
       title = "Event-study ATTs across anticipation windows") +
  theme_minimal(base_size = 12)
