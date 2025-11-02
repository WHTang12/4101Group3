# Import packages
library(tidyverse)
library(lubridate)
library(didimputation)
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

#-------------------------------
# Birth
#-------------------------------
# Main Model 
imp_birth <- did_imputation(
  data        = df1,
  yname       = "birth_lastyear",           # individual-level outcome
  gname       = "treat_start_year",         # treatment start year
  tname       = "YEAR",                      # calendar year
  idname      = "STATEFIP",                  # cluster/unit ID
  wname       = "ASECWT",                    # population weight
  first_stage = ~ AGE + NCHILD + EDUC + MARST + ELDCH +
    black + white + lag_unemployment_rate + lag_weekly_median_wage | 
    STATEFIP + YEAR,           # fixed effects
  horizon     = TRUE,                        # event-study: 5 years before & after treatment
  pretrends   = TRUE,                       # pre-treatment trends check
  cluster_var = "STATEFIP"                   # clustering
)

summary(imp_birth)

imp_birth_plot <- imp_birth %>% 
  filter(!term %in% c("AGE","EDUC","MARST","ELDCH","black","white","lag_unemployment_rate","lag_weekly_median_wage")) 

ggplot(imp_birth_plot, aes(x = as.numeric(term), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  labs(
    x = "Event Time (Years Relative to Treatment)",
    y = "Estimated Effect on Birth Probability",
    title = "Event-Time Plot from Imputation DiD (Birth)"
  ) +
  theme_minimal()

# Placebo Test 
df_placebo <- df1 %>%
  mutate(fake_treat_start_year = pmax(0, treat_start_year - 7)) %>% 
  filter(YEAR < treat_start_year | treat_start_year == 0)  # keep pre-treatment/control years

birth_placebo <- did_imputation(
  data        = df_placebo,
  yname       = "birth_lastyear",
  gname       = "fake_treat_start_year",
  tname       = "YEAR",
  idname      = "STATEFIP",
  wname       = "ASECWT",
  first_stage = ~ AGE + NCHILD + EDUC + MARST + ELDCH + black + white + lag_unemployment_rate + lag_weekly_median_wage | 
    STATEFIP + YEAR,
  horizon     = TRUE,
  pretrends   = TRUE,
  cluster_var = "STATEFIP"
)

imp_birth_placebo_plot <- birth_placebo %>%
  filter(!term %in% c("AGE","EDUC","MARST","ELDCH","black","white","lag_unemployment_rate","lag_weekly_median_wage")) 

ggplot(imp_birth_placebo_plot, aes(x = as.numeric(term), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  labs(
    x = "Event Time (Years Relative to Treatment)",
    y = "Estimated Effect on Birth Probability",
    title = "Event-Time Plot for Placebo Test (Birth)"
  ) +
  theme_minimal()

# Anticipation test
# Helper function for different anticipation windows
model_anticipation_birth <- function(k) {
  
  # Shift treatment k years earlier
  df_anticipation <- df1 %>%
    mutate(fake_treat_start_year = ifelse(treat_start_year > 0, treat_start_year - k, 0))
  
  # Run did_imputation on shifted treatment
  imp_birth_anticipation <- did_imputation(
    data        = df_anticipation,
    yname       = "birth_lastyear",
    gname       = "fake_treat_start_year",
    tname       = "YEAR",
    idname      = "STATEFIP",
    wname       = "ASECWT",
    first_stage = ~ AGE + NCHILD + EDUC + MARST + ELDCH +
      black + white + lag_unemployment_rate + lag_weekly_median_wage | STATEFIP + YEAR,
    horizon     = TRUE,
    pretrends   = TRUE,
    cluster_var = "STATEFIP"
  )
  
  # Extract horizon results (event-study)
  tibble(
    term           = imp_birth_anticipation$term,
    estimate       = imp_birth_anticipation$estimate,
    std_error      = imp_birth_anticipation$std.error,
    lower          = imp_birth_anticipation$conf.low,
    upper          = imp_birth_anticipation$conf.high,
    anticipation_k = k
  )
}

# Run for k = 1 to 5
k_values <- 1:5
anticipation_results_birth <- map_dfr(k_values, model_anticipation_birth)

anticipation_results_birth <- anticipation_results_birth %>%
  mutate(
    anticipation_k = factor(anticipation_k) # for coloring
  )

ggplot(anticipation_results_birth, aes(x = term, y = estimate, color = anticipation_k, group = anticipation_k)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = anticipation_k), alpha = 0.15, color = NA) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(
    x = "Event horizon (years relative to treatment)",
    y = "Estimated ATT (dynamic)",
    color = "Anticipation k",
    fill  = "Anticipation k",
    title = "Event-study ATT across anticipation windows"
  ) +
  theme_minimal(base_size = 12)


#-------------------------------
# Main model - Employment
#-------------------------------
imp_employment <- did_imputation(
  data        = df1,
  yname       = "employed",           # individual-level outcome
  gname       = "treat_start_year",         # treatment start year
  tname       = "YEAR",                      # calendar year
  idname      = "STATEFIP",                  # cluster/unit ID
  wname       = "ASECWT",                    # population weight
  first_stage = ~ AGE + NCHILD + EDUC + MARST + ELDCH +
    black + white + lag_unemployment_rate + lag_weekly_median_wage | 
    STATEFIP + YEAR,           # fixed effects
  horizon     = TRUE,                        # event-study: 5 years before & after treatment
  pretrends   = TRUE,                       # pre-treatment trends check
  cluster_var = "STATEFIP"                   # clustering
)

summary(imp_employment)

imp_employment_plot <- imp_employment %>% 
  filter(!term %in% c("AGE","EDUC","MARST","RACE","ELDCH","black","white","lag_unemployment_rate","lag_weekly_median_wage")) 

ggplot(imp_employment_plot, aes(x = as.numeric(term), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  labs(
    x = "Event Time (Years Relative to Treatment)",
    y = "Estimated Effect on Employment Probability",
    title = "Event-Time Plot from Imputation DiD (Employment)"
  ) +
  theme_minimal()

# Placebo Test 
employment_placebo <- did_imputation(
  data        = df_placebo,
  yname       = "employed",
  gname       = "fake_treat_start_year",
  tname       = "YEAR",
  idname      = "STATEFIP",
  wname       = "ASECWT",
  first_stage = ~ AGE + NCHILD + EDUC + MARST + ELDCH + black + white + lag_unemployment_rate + lag_weekly_median_wage | 
    STATEFIP + YEAR,
  horizon     = TRUE,
  pretrends   = TRUE,
  cluster_var = "STATEFIP"
)

imp_employed_placebo_plot <- employment_placebo %>%
  filter(!term %in% c("AGE","EDUC","MARST","ELDCH","black","white","lag_unemployment_rate","lag_weekly_median_wage")) 

ggplot(imp_employed_placebo_plot, aes(x = as.numeric(term), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  labs(
    x = "Event Time (Years Relative to Treatment)",
    y = "Estimated Effect on Employment Probability",
    title = "Event-Time Plot for Placebo Test (Employment)"
  ) +
  theme_minimal()

# Anticipation test
# Helper function for different anticipation windows
model_anticipation_employment <- function(k) {
  
  # Shift treatment k years earlier
  df_anticipation <- df1 %>%
    mutate(fake_treat_start_year = ifelse(treat_start_year > 0, treat_start_year - k, 0))
  
  # Run did_imputation on shifted treatment
  imp_employment_anticipation <- did_imputation(
    data        = df_anticipation,
    yname       = "birth_lastyear",
    gname       = "fake_treat_start_year",
    tname       = "YEAR",
    idname      = "STATEFIP",
    wname       = "ASECWT",
    first_stage = ~ AGE + NCHILD + EDUC + MARST + ELDCH +
      black + white + lag_unemployment_rate + lag_weekly_median_wage | STATEFIP + YEAR,
    horizon     = TRUE,
    pretrends   = TRUE,
    cluster_var = "STATEFIP"
  )
  
  # Extract horizon results (event-study)
  tibble(
    term           = imp_employment_anticipation$term,
    estimate       = imp_employment_anticipation$estimate,
    std_error      = imp_employment_anticipation$std.error,
    lower          = imp_employment_anticipation$conf.low,
    upper          = imp_employment_anticipation$conf.high,
    anticipation_k = k
  )
}

# Run for k = 1 to 5
k_values <- 1:5
anticipation_results_employment <- map_dfr(k_values, model_anticipation_employment)

anticipation_results_employment <- anticipation_results_employment %>%
  mutate(
    anticipation_k = factor(anticipation_k) # for coloring
  )

ggplot(anticipation_results_employment, aes(x = term, y = estimate, color = anticipation_k, group = anticipation_k)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = anticipation_k), alpha = 0.15, color = NA) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(
    x = "Event horizon (years relative to treatment)",
    y = "Estimated ATT (dynamic)",
    color = "Anticipation k",
    fill  = "Anticipation k",
    title = "Event-study ATT across anticipation windows"
  ) +
  theme_minimal(base_size = 12)




