# packages needed
library(tidyverse)
library(lubridate)
library(fixest)

# data preparation
## import cleaned data
df <- readRDS("../cleaned_data/finalmerged.rds")

## remove observations where policy has been repealed and ASECWT = 0
df1 <- df %>%
  filter(is.na(repeal_year) | YEAR <= repeal_year) %>%
  filter(ASECWT != 0)

## set treat_start_year = 0 for never treated units
df1$treat_start_year[is.na(df1$treat_start_year)] <- 0

# OUTCOME: FERTILITY
# main model: interaction-weighted model, control group = never-treated
## run the model
iw_sa_NT <- feols(
  birth_lastyear ~ sunab(treat_start_year, YEAR) + AGE + NCHILD + EDUC + MARST + 
    ELDCH + black + white + lag_unemployment_rate + lag_weekly_median_wage |
    STATEFIP + YEAR,
  data = df1,
  weights = ~ASECWT,
  cluster = ~STATEFIP
)

## summary of the results
summary(iw_sa_NT)

## plot of the results
iplot(iw_sa_NT, xlab = "Years Since Adoption (τ)", ylab = "ATT(τ)",
      main = "Dynamic Effects on Fertility, Control = Not Treated", col = "steelblue")
abline(h = 0, lty = 2, col = "grey40")

## aggregate dynamic ATT
agg_dyn_NT <- aggregate(iw_sa_NT, agg = "ATT", type = "dynamic")
summary(agg_dyn_NT)

## aggregate simple ATT
agg_simple_NT <- aggregate(iw_sa_NT, agg = "ATT", type = "simple")
summary(agg_simple_NT)

# secondary model: interaction-weighted model, control group = not yet-treated
## remove never-treated units
df2 <- subset(df1, treat_start_year > 0)

## run the model
iw_sa_NYT <- feols(
  birth_lastyear ~ sunab(treat_start_year, YEAR) +
    AGE + NCHILD + EDUC + MARST + ELDCH + black + white +
    lag_unemployment_rate + lag_weekly_median_wage |
    STATEFIP + YEAR,
  data = df2,
  weights = ~ASECWT,
  cluster = ~STATEFIP
)

## summary of the results
summary(iw_sa_NYT)

## plot of the results
iplot(iw_sa_NYT, xlab = "Years Since Adoption (τ)", ylab = "ATT(τ)",
      main = "Dynamic Effects on Fertility, Control = Not Yet Treated", col = "steelblue")
abline(h = 0, lty = 2, col = "grey40")

## aggregate dynamic ATT
agg_dyn_NYT <- aggregate(iw_sa_NYT, agg = "ATT", type = "dynamic")
summary(agg_dyn_NYT)

## aggregate simple ATT
agg_simple_NYT <- aggregate(iw_sa_NYT, agg = "ATT", type = "simple")
summary(agg_simple_NYT)

# testing sensitivity to anticipation
## defining the model
iw <- feols(
  birth_lastyear ~ sunab(treat_start_year, YEAR) + AGE + NCHILD + EDUC + MARST + 
    ELDCH + black + white + lag_unemployment_rate + lag_weekly_median_wage | 
    STATEFIP + YEAR,
  data = df1,
  weights = ~ASECWT,
  cluster = ~STATEFIP
)

iw_sa_NT <- feols(
  birth_lastyear ~ sunab(treat_start_year, YEAR) + AGE + NCHILD + EDUC + MARST + 
    ELDCH + black + white + lag_unemployment_rate + lag_weekly_median_wage |
    STATEFIP + YEAR,
  data = df1,
  weights = ~ASECWT,
  cluster = ~STATEFIP
)

## helper function that runs models with different anticipation, k
run_iw_k <- function(k){
  # aggregate simple ATT
  ag <- aggregate(iw, agg = "ATT", type = "simple", anticipation = k)
  # store every ATT and standard errors
  tibble(
    anticipation = k,
    att = ag[1, "Estimate"],
    se  = ag[1, "Std. Error"]
  )
}

## run model for k = 0:5
res_iw <- map_dfr(0:5, run_iw_k)
print(res_iw)

## compute confidence intervals and plot
anticipationResults_sa <- res_iw %>%
  mutate(
    upper = att + 1.96 * se,
    lower = att - 1.96 * se
  )
ggplot(anticipationResults_sa,
       aes(x = anticipation, y = att, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(color = "steelblue", size = 0.8) +
  labs(
    x = "Anticipation window (k years)",
    y = "Average ATT",
    title = "ATT sensitivity to anticipation (Fertility)"
  ) +
  theme_minimal(base_size = 13)

# OUTCOME: EMPLOYMENT
# main model: interaction-weighted model, control group = never-treated
## run the model
iw_sa_NT_emp <- feols(
  employed ~ sunab(treat_start_year, YEAR) + AGE + NCHILD + EDUC + MARST + 
    ELDCH + black + white + lag_unemployment_rate + lag_weekly_median_wage |
    STATEFIP + YEAR,
  data = df1,
  weights = ~ASECWT,
  cluster = ~STATEFIP
)

## summary of results
summary(iw_sa_NT_emp)

## plot of the results
iplot(iw_sa_NT_emp, xlab = "Years Since Adoption (τ)", ylab = "ATT(τ)",
      main = "Dynamic Effects on Employment, Control = Not Treated", col = "steelblue")
abline(h = 0, lty = 2, col = "grey40")

## aggregate dynamic ATT
agg_dyn_NT_emp <- aggregate(iw_sa_NT_emp, agg = "ATT", type="dynamic")
summary(agg_dyn_NT_emp)

## aggregate simple ATT
agg_simple_NT_emp <- aggregate(iw_sa_NT_emp, agg = "ATT", type="simple")
summary(agg_simple_NT_emp)

# secondary model: interaction-weighted model, control group = not yet-treated
## remove never-treated units
df2 <- subset(df1, treat_start_year > 0)

## run the model
iw_sa_NYT_emp <- feols(
  employed ~ sunab(treat_start_year, YEAR) +
    AGE + NCHILD + EDUC + MARST + ELDCH + black + white +
    lag_unemployment_rate + lag_weekly_median_wage |
    STATEFIP + YEAR,
  data = df2,
  weights = ~ASECWT,
  cluster = ~STATEFIP
)

## summary of results
summary(iw_sa_NYT_emp)

## plot the results
iplot(iw_sa_NYT_emp, xlab = "Years Since Adoption (τ)", ylab = "ATT(τ)",
      main = "Dynamic Effects on Employment, Control = Not Yet Treated", col = "steelblue")
abline(h = 0, lty = 2, col = "grey40")

## aggregate dynamic ATT
agg_dyn_NYT_emp <- aggregate(iw_sa_NYT_emp, agg = "ATT", type = "dynamic")
summary(agg_dyn_NYT_emp)

## aggregate simple ATT
agg_simple_NYT_emp <- aggregate(iw_sa_NYT_emp, agg = "ATT", type = "simple")
summary(agg_simple_NYT_emp)


# testing sensitivity to anticipation
## defining the model
iw_emp <- feols(
  employed ~ sunab(treat_start_year, YEAR) + AGE + NCHILD + EDUC + MARST + 
    ELDCH + black + white + lag_unemployment_rate + lag_weekly_median_wage | 
    STATEFIP + YEAR,
  data = df1,
  weights = ~ASECWT,
  cluster = ~STATEFIP
)

## helper function that runs models with different anticipation, k
run_iw_k_emp <- function(k){
  ag <- aggregate(iw_emp, agg = "ATT", anticipation = k)
  tibble(
    anticipation = k,
    att = ag[1, "Estimate"],
    se  = ag[1, "Std. Error"]
  )
}

## run model for k = 0:5
res_iw_emp <- map_dfr(0:5, run_iw_k_emp)
print(res_iw_emp)

## compute confidence intervals and plot
anticipationResults_emp <- res_iw_emp %>%
  mutate(
    upper = att + 1.96 * se,
    lower = att - 1.96 * se
  )
ggplot(anticipationResults_emp,
       aes(x = anticipation, y = att, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(color = "steelblue", size = 0.8) +
  labs(
    x = "Anticipation window (k years)",
    y = "Average ATT",
    title = "ATT sensitivity to anticipation (employment)"
  ) +
  theme_minimal(base_size = 13)
