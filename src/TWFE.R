# Import packages
library(fixest)
library(ggplot2)
library(dplyr)
library(tidyr)


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
