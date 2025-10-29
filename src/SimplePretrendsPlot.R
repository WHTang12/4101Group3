library(tidyverse)
library(lubridate)
library(ggplot2)

# Import data
df <- readRDS("../cleaned_data/finalmerged.rds")

# Filter only for years where it's before repeal_year
df1 <- df %>%
  filter(is.na(repeal_year) | YEAR <= repeal_year)

# Compute birth rates and employment rate for each (ever_treated, year) by taking weighted means
df_stateyear <- df1 %>%
  group_by(YEAR, ever_treated) %>%
  summarise(
    birth_rate = weighted.mean(birth_lastyear, na.rm = TRUE) * 1000,
    emp_rate   = weighted.mean(employed, ASECWT, na.rm = TRUE) * 100
  ) %>%
  ungroup()

# Plot
ggplot(df_stateyear, aes(x = YEAR, y = birth_rate,
                         color = factor(ever_treated, levels = c(1, 0),
                                        labels = c("Treated states", "Never treated states")))) +
  geom_line(size = 1) +
  geom_vline(xintercept = 1992, linetype = "dashed", color = "black") +
  labs(
    title = "Average Annual Birth Rates by Treatment Status",
    x = "Year", y = "Average Births per 1,000 Women", color = "Group"
  ) +
  scale_color_manual(values = c("firebrick", "steelblue")) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggplot(df_stateyear, aes(x = YEAR, y = emp_rate,
                         color = factor(ever_treated, levels = c(1, 0),
                                        labels = c("Treated states", "Never treated states")))) +
  geom_line(size = 1) +
  geom_vline(xintercept = 1992, linetype = "dashed", color = "black") +
  labs(
    title = "Average Annual Employment Rates by Treatment Status",
    x = "Year", y = "Average Employment Rate", color = "Group"
  ) +
  scale_color_manual(values = c("firebrick", "steelblue")) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

df2 <- df_stateyear %>%
  mutate(event_time = YEAR - treat_start_year)

est <- feols(birth_rate ~ i(event_time, ref = -1) | STATEFIP + YEAR, data = df2)
summary(est)

iplot(est, ref.line = 0, xlab = "Years relative to treatment", ylab = "Effect on birth rate")