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

df_state <- df1 %>%
  group_by(STATEFIP, YEAR, treat_start_year) %>%
  summarise(
    birth_lastyear = weighted.mean(birth_lastyear, ASECWT, na.rm = TRUE),
    employed = weighted.mean(employed, ASECWT, na.rm = TRUE),
    AGE   = weighted.mean(AGE,   ASECWT, na.rm = TRUE),
    NCHILD   = weighted.mean(NCHILD,   ASECWT, na.rm = TRUE),
    EDUC  = weighted.mean(EDUC,  ASECWT, na.rm = TRUE),
    MARST = weighted.mean(MARST, ASECWT, na.rm = TRUE),
    ELDCH = weighted.mean(ELDCH, ASECWT, na.rm = TRUE),
    black = weighted.mean(black, ASECWT, na.rm = TRUE), 
    white = weighted.mean(white, ASECWT, na.rm = TRUE),
    lag_unemployment_rate = weighted.mean(lag_unemployment_rate, ASECWT, na.rm = TRUE), 
    lag_weekly_median_wage = weighted.mean(lag_weekly_median_wage, ASECWT, na.rm = TRUE),
    .groups = "drop"
  )


#-------------------------------
# Birth
#-------------------------------
# Main Model 
imp_birth <- did_imputation(
  data        = df_state,
  yname       = "birth_lastyear",           # individual-level outcome
  gname       = "treat_start_year",         # treatment start year
  tname       = "YEAR",                      # calendar year
  idname      = "STATEFIP",                  # cluster/unit ID
  horizon     = TRUE,                        
  pretrends   = TRUE,                       # pre-treatment trends check
  cluster_var = "STATEFIP"                   # clustering
)

summary(imp_birth)

imp_birth_plot <- imp_birth %>% 
  filter(!term %in% c("AGE","EDUC","MARST","ELDCH","black","white","lag_unemployment_rate","lag_weekly_median_wage")) %>% 
  mutate(term = as.numeric(term))

imp_birth_plot_filled <- imp_birth_plot %>%
  complete(term = seq(-21, 19)) %>%        # ensures all terms are present
  mutate(
    estimate = replace_na(estimate, 0),    # fill missing estimates with 0
    conf.low = replace_na(conf.low, 0),    # optional: fill CI too
    conf.high = replace_na(conf.high, 0)
  )

ggplot(imp_birth_plot_filled, 
       aes(x = as.numeric(term), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, color = "blue") +
  geom_point(color = "blue", size = 2) +
  labs(
    x = "Event Time (Years Relative to Treatment)",
    y = "Estimated Effect on Birth Probability",
    title = "Event-Study Plot from Imputation DiD (Birth)"
  ) +
  theme_minimal(base_size = 12)

# Average Coefficient Effect
imp_birth_avg <- did_imputation(
  data        = df_state,
  yname       = "birth_lastyear",           # individual-level outcome
  gname       = "treat_start_year",         # treatment start year
  tname       = "YEAR",                      # calendar year
  idname      = "STATEFIP",                  # cluster/unit ID
  pretrends   = TRUE,                       # pre-treatment trends check
  cluster_var = "STATEFIP"                   # clustering
)

summary(imp_birth_avg)

# Placebo Test 
df_placebo <- df_state %>%
  mutate(fake_treat_start_year = pmax(0, treat_start_year - 7)) %>% 
  filter(YEAR < treat_start_year | treat_start_year == 0)  # keep pre-treatment/control years

birth_placebo <- did_imputation(
  data        = df_placebo,
  yname       = "birth_lastyear",
  gname       = "fake_treat_start_year",
  tname       = "YEAR",
  idname      = "STATEFIP",
  horizon     = TRUE,
  pretrends   = TRUE,
  cluster_var = "STATEFIP"
)

imp_birth_placebo_plot <- birth_placebo %>%
  filter(!term %in% c("AGE","EDUC","MARST","ELDCH","black","white","lag_unemployment_rate","lag_weekly_median_wage")) %>% 
  mutate(term = as.numeric(term))

imp_birth_placebo_plot_filled <- imp_birth_placebo_plot %>%
  complete(term = seq(-14, 6)) %>%        # ensures all terms are present
  mutate(
    estimate = replace_na(estimate, 0),    # fill missing estimates with 0
    conf.low = replace_na(conf.low, 0),    # optional: fill CI too
    conf.high = replace_na(conf.high, 0)
  )

ggplot(imp_birth_placebo_plot_filled, 
       aes(x = as.numeric(term), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, color = "blue") +
  geom_point(color = "blue", size = 2) +
  labs(
    x = "Event Time (Years Relative to Treatment)",
    y = "Estimated Effect on Birth Probability",
    title = "Event-Time Plot for Placebo Test (Birth)"
  ) +
  theme_minimal(base_size = 12)

# Anticipation test
# Helper function for different anticipation windows
model_anticipation_birth <- function(k) {
  
  # Shift treatment k years earlier
  df_anticipation <- df_state %>%
    mutate(fake_treat_start_year = ifelse(treat_start_year > 0, treat_start_year - k, 0)) 
  
  # Run did_imputation on shifted treatment
  imp_birth_anticipation <- did_imputation(
    data        = df_anticipation,
    yname       = "birth_lastyear",
    gname       = "fake_treat_start_year",
    tname       = "YEAR",
    idname      = "STATEFIP",
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
  ) %>% 
  mutate(term = as.numeric(term))

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

# Anticipation test (Average)
# Helper function for different anticipation windows
model_anticipation_birth <- function(k) {
  
  # Shift treatment k years earlier
  df_anticipation <- df_state %>%
    mutate(fake_treat_start_year = ifelse(treat_start_year > 0, treat_start_year - k, 0))
  
  # Run did_imputation on shifted treatment
  imp_birth_anticipation <- did_imputation(
    data        = df_anticipation,
    yname       = "birth_lastyear",
    gname       = "fake_treat_start_year",
    tname       = "YEAR",
    idname      = "STATEFIP",
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

# Extract only the ATT at the treatment period (term = 0)
anticipation_summary_birth <- anticipation_results_birth %>%
  filter(term == "treat") %>%   # focus on the treatment period
  mutate(anticipation_k = as.numeric(as.character(anticipation_k)))

# Plot ATT at treatment (term = 0) for each anticipation k
ggplot(anticipation_summary_birth, aes(x = anticipation_k, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = lower, ymax = upper), color = "steelblue", size = 0.8) +
  labs(
    x = "Anticipation window (k years)",
    title = "Sensitivity of ATT at treatment to anticipation window"
  ) +
  theme_minimal(base_size = 13)


#-------------------------------
# Main model - Employment
#-------------------------------
imp_employment <- did_imputation(
  data        = df_state,
  yname       = "employed",           # individual-level outcome
  gname       = "treat_start_year",         # treatment start year
  tname       = "YEAR",                      # calendar year
  idname      = "STATEFIP",                  # cluster/unit ID
  horizon     = TRUE,                        
  pretrends   = TRUE,                       # pre-treatment trends check
  cluster_var = "STATEFIP"                   # clustering
)

summary(imp_employment)

imp_employment_plot <- imp_employment %>% 
  filter(!term %in% c("AGE","EDUC","MARST","RACE","ELDCH","black","white","lag_unemployment_rate","lag_weekly_median_wage")) %>% 
  mutate(term = as.numeric(term))

imp_employed_plot_filled <- imp_employment_plot %>%
  complete(term = seq(-21, 19)) %>%        # ensures all terms are present
  mutate(
    estimate = replace_na(estimate, 0),    # fill missing estimates with 0
    conf.low = replace_na(conf.low, 0),    # optional: fill CI too
    conf.high = replace_na(conf.high, 0)
  )

ggplot(imp_employed_plot_filled, 
       aes(x = as.numeric(term), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, color = "blue") +
  geom_point(color = "blue", size = 2) +
  labs(
    x = "Event Time (Years Relative to Treatment)",
    y = "Estimated Effect on Employment Probability",
    title = "Event-Time Plot from Imputation DiD (Employment)"
  ) +
  theme_minimal(base_size = 12)

# Average Coefficient Effect
imp_employed_avg <- did_imputation(
  data        = df_state,
  yname       = "employed",           # individual-level outcome
  gname       = "treat_start_year",         # treatment start year
  tname       = "YEAR",                      # calendar year
  idname      = "STATEFIP",                  # cluster/unit ID
  pretrends   = TRUE,                       # pre-treatment trends check
  cluster_var = "STATEFIP"                   # clustering
)

summary(imp_employed_avg)


# Placebo Test 
employment_placebo <- did_imputation(
  data        = df_placebo,
  yname       = "employed",
  gname       = "fake_treat_start_year",
  tname       = "YEAR",
  idname      = "STATEFIP",
  horizon     = TRUE,
  pretrends   = TRUE,
  cluster_var = "STATEFIP"
)

imp_employed_placebo_plot <- employment_placebo %>%
  filter(!term %in% c("AGE","EDUC","MARST","ELDCH","black","white","lag_unemployment_rate","lag_weekly_median_wage")) %>% 
  mutate(term = as.numeric(term))

imp_employed_placebo_plot_filled <- imp_employed_placebo_plot %>%
  complete(term = seq(-21, 19)) %>%        # ensures all terms are present
  mutate(
    estimate = replace_na(estimate, 0),    # fill missing estimates with 0
    conf.low = replace_na(conf.low, 0),    # optional: fill CI too
    conf.high = replace_na(conf.high, 0)
  )


ggplot(imp_employed_placebo_plot_filled, 
       aes(x = as.numeric(term), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, color = "blue") +
  geom_point(color = "blue", size = 2) +
  labs(
    x = "Event Time (Years Relative to Treatment)",
    y = "Estimated Effect on Employment Probability",
    title = "Event-Time Plot for Placebo Test (Employment)"
  ) +
  theme_minimal(base_size = 12)


# Anticipation test
# Helper function for different anticipation windows
model_anticipation_employment <- function(k) {
  
  # Shift treatment k years earlier
  df_anticipation <- df_state %>%
    mutate(fake_treat_start_year = ifelse(treat_start_year > 0, treat_start_year - k, 0))
  
  # Run did_imputation on shifted treatment
  imp_employment_anticipation <- did_imputation(
    data        = df_anticipation,
    yname       = "employed",
    gname       = "fake_treat_start_year",
    tname       = "YEAR",
    idname      = "STATEFIP",
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
  ) %>% 
  mutate(term = as.numeric(term))

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

# Anticipation test (Average)
# Helper function for different anticipation windows
model_anticipation_employment <- function(k) {
  
  # Shift treatment k years earlier
  df_anticipation <- df_state %>%
    mutate(fake_treat_start_year = ifelse(treat_start_year > 0, treat_start_year - k, 0))
  
  # Run did_imputation on shifted treatment
  imp_employment_anticipation <- did_imputation(
    data        = df_anticipation,
    yname       = "employed",
    gname       = "fake_treat_start_year",
    tname       = "YEAR",
    idname      = "STATEFIP",
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

# Extract only the ATT at the treatment period (term = 0)
anticipation_summary_employment <- anticipation_results_employment %>%
  filter(term == "treat") %>%   # focus on the treatment period
  mutate(anticipation_k = as.numeric(as.character(anticipation_k)))

# Plot ATT at treatment (term = 0) for each anticipation k
ggplot(anticipation_summary_employment, aes(x = anticipation_k, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = lower, ymax = upper), color = "steelblue", size = 0.8) +
  labs(
    x = "Anticipation window (k years)",
    title = "Sensitivity of ATT at treatment to anticipation window"
  ) +
  theme_minimal(base_size = 13)




