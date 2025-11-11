# Import packages
library(fixest)
library(ggplot2)
library(dplyr)
library(tidyr)
library(car)   


# Import cleaned data
df <- readRDS("../cleaned_data/finalmerged.rds")

# Remove all observations where year > repeal year
df1 <- df %>%
  filter(is.na(repeal_year) | YEAR <= repeal_year) %>%
  filter(ASECWT != 0)

colnames(df1)

# For never-treated units, we set treat_start_year to be 0, to be consistent with how the package wants it
df1$treat_start_year[is.na(df1$treat_start_year)] <- 0

# Basic TWFE (Birth): unit and year fixed effects
twfe_model_birth <- feols(
  birth_lastyear ~ treated + AGE + NCHILD + EDUC + MARST + ELDCH + black + white + 
    lag_unemployment_rate + lag_weekly_median_wage | STATEFIP + YEAR,
  data = df1,
  cluster = "STATEFIP"
)

summary(twfe_model_birth)

# Pretrend check 
# Find the first year treatment started
first_treat_year <- min(df1$treat_start_year[df1$ever_treated == 1], na.rm = TRUE)

# Prepare data for plotting
event_avg_birth <- df1 %>%
  mutate(group = ifelse(ever_treated == 1, "Treated", "Never-treated")) %>%
  group_by(group, YEAR) %>%
  summarise(avg_births = mean(birth_lastyear, na.rm = TRUE), .groups = "drop")

# Plot with vertical dashed line
ggplot(event_avg_birth, aes(x = YEAR, y = avg_births, color = group)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = first_treat_year, linetype = "dashed", color = "red") +
  labs(
    title = "Births Over Calendar Years: Treated vs Never-Treated",
    x = "Year",
    y = "Average Births Last Year"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "orange"))

# ----------------------------------------
# FORMAL PRETREND TEST FOR BIRTH
# ----------------------------------------

# Create event time (relative to treatment start)
df1 <- df1 %>%
  mutate(event_time = YEAR - treat_start_year)

# Restrict to a balanced window, e.g. -10 to +10 years
df_event <- df1 %>%
  filter(event_time >= -10 & event_time <= 10)

# Run event-study TWFE model with year -1 as reference period
event_study_birth <- feols(
  birth_lastyear ~ i(event_time, treated, ref = -1) + AGE + EDUC + MARST + NCHILD + ELDCH +
    black + white + lag_unemployment_rate + lag_weekly_median_wage | STATEFIP + YEAR,
  data = df_event,
  cluster = "STATEFIP"
)

# View summary
summary(event_study_birth)

# Plot event-study coefficients
iplot(event_study_birth,
      main = "Event Study Plot: Births",
      ref.line = -1,
      xlab = "Years relative to treatment",
      ylab = "Coefficient")

# Get all coefficient names
coef_names <- names(coef(event_study_birth))

# Extract pre-treatment dummy names: event_time -10 to -2
pretrend_names <- coef_names[grepl("treat_start_year::", coef_names)]  # all i() dummies
pretrend_names <- pretrend_names[grepl("^-10$|-9$|-8$|-7$|-6$|-5$|-4$|-3$|-2$", pretrend_names, perl = TRUE)] 

# Perform Wald test for joint significance of pre-trends
wald_pretrend_birth <- wald(event_study_birth, keep = pretrend_names)
wald_pretrend_birth

# Anticipation Test
anticipation_birth <- df1 %>%
  filter(ever_treated == 1) %>%       # only treated units
  mutate(event_time = YEAR - treat_start_year) %>%  # create relative time
  group_by(event_time) %>%
  summarise(avg_births = mean(birth_lastyear, na.rm = TRUE), .groups = "drop")

ggplot(anticipation_birth, aes(x = event_time, y = avg_births)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 0, linetype="dashed", color="red") +
  labs(
    title = "Anticipation Test: Event-Time Plot",
    x = "Years Since Treatment",
    y = "Average Births"
  ) +
  theme_minimal()

# Placebo Test
df_placebo <- df1 %>%
  mutate(fake_treat_start_year = treat_start_year + 7)  # shift forward

twfe_placebo_birth <- feols(
  birth_lastyear ~ I(YEAR >= fake_treat_start_year) + AGE + NCHILD + EDUC + MARST + ELDCH + black + white + 
    lag_unemployment_rate + lag_weekly_median_wage | STATEFIP + YEAR,
  data = df_placebo,
  cluster = "STATEFIP"
)

summary(twfe_placebo_birth)


# Basic TWFE (Employment): unit and year fixed effects
twfe_model_employment <- feols(
  employed ~ treated + AGE + NCHILD + EDUC + MARST + ELDCH + black + white + 
    lag_unemployment_rate + lag_weekly_median_wage | STATEFIP + YEAR,
  data = df1,
  cluster = "STATEFIP"
)

summary(twfe_model_employment)

# Pretrend check 
# Find the first year treatment started
first_treat_year <- min(df1$treat_start_year[df1$ever_treated == 1], na.rm = TRUE)

# Prepare data for plotting
event_avg_employment <- df1 %>%
  mutate(group = ifelse(ever_treated == 1, "Treated", "Never-treated")) %>%
  group_by(group, YEAR) %>%
  summarise(avg_employment = mean(employed, na.rm = TRUE), .groups = "drop")

# Plot with vertical dashed line
ggplot(event_avg_employment, aes(x = YEAR, y = avg_employment, color = group)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = first_treat_year, linetype = "dashed", color = "red") +
  labs(
    title = "Births Over Calendar Years: Treated vs Never-Treated",
    x = "Year",
    y = "Average Births Last Year"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "orange"))

# Anticipation Test 
anticipation_employment <- df1 %>%
  filter(ever_treated == 1) %>%       # only treated units
  mutate(event_time = YEAR - treat_start_year) %>%  # create relative time
  group_by(event_time) %>%
  summarise(avg_employment = mean(employed, na.rm = TRUE), .groups = "drop")

ggplot(anticipation_employment, aes(x = event_time, y = avg_employment)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 0, linetype="dashed", color="red") +
  labs(
    title = "Anticipation Test: Event-Time Plot",
    x = "Years Since Treatment",
    y = "Average Employment"
  ) +
  theme_minimal()

# Placebo Test
df_placebo <- df1 %>%
  mutate(fake_treat_start_year = treat_start_year + 7)  # shift forward

twfe_placebo_employment <- feols(
  employed ~ I(YEAR >= fake_treat_start_year) + AGE + NCHILD + EDUC + MARST + ELDCH + black + white + 
    lag_unemployment_rate + lag_weekly_median_wage | STATEFIP + YEAR,
  data = df_placebo,
  cluster = "STATEFIP"
)

summary(twfe_placebo_employment)

# ----------------------------------------
# FORMAL PRETREND TEST FOR EMPLOYMENT
# ----------------------------------------


# Create event time (relative to treatment start)
df1 <- df1 %>%
  mutate(event_time = YEAR - treat_start_year)

# Restrict to a balanced window, e.g. -10 to +10 years
df_event <- df1 %>%
  filter(event_time >= -10 & event_time <= 10)

# Run event-study TWFE model with year -1 as reference period
event_study_employment <- feols(
  employed ~ i(event_time, treated, ref = -1) + AGE + EDUC + MARST + NCHILD + ELDCH +
    black + white + lag_unemployment_rate + lag_weekly_median_wage | STATEFIP + YEAR,
  data = df_event,
  cluster = "STATEFIP"
)

# View summary
summary(event_study_employment)

# Plot event-study coefficients
iplot(event_study_employment,
      main = "Event Study Plot: Employment",
      ref.line = -1,
      xlab = "Years relative to treatment",
      ylab = "Coefficient")

# Formal pre-trend test: Are all pre-treatment coefficients jointly zero?
# Get all coefficient names
coef_names <- names(coef(event_study_employment))

# Extract pre-treatment dummy names: event_time -10 to -2
pretrend_names <- coef_names[grepl("treat_start_year::", coef_names)]  # all i() dummies
pretrend_names <- pretrend_names[grepl("^-10$|-9$|-8$|-7$|-6$|-5$|-4$|-3$|-2$", pretrend_names, perl = TRUE)] 

# Perform Wald test for joint significance of pre-trends
wald_pretrend_employment <- wald(event_study_employment, keep = pretrend_names)
wald_pretrend_employment
