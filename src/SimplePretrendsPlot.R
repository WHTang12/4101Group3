# This file is just for plotting the pre-trends used in presentation/report. 
# There are no models/analysis done, so you can ignore.

library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)
#install.packages("patchwork")

# Import data
df <- readRDS("../cleaned_data/finalmerged.rds")

# Filter only for years where it's before repeal_year
df1 <- df %>%
  filter(is.na(repeal_year) | YEAR <= repeal_year)

# Compute birth rates and employment rate for each (ever_treated, year) by taking weighted means
df_stateyear <- df1 %>%
  group_by(YEAR, ever_treated) %>%
  summarise(
    birth_rate = weighted.mean(birth_lastyear, w = ASECWT, na.rm = TRUE) * 1000,
    emp_rate   = weighted.mean(employed,     w = ASECWT, na.rm = TRUE) * 100
    , .groups = "drop") %>%
  mutate(group = if_else(ever_treated == 1, "Treated states", "Never treated states"))


##### Plotting #####
color_scale <- scale_color_manual(
  values = c("Treated states" = "firebrick", "Never treated states" = "steelblue"),
  labels  = c("Treated", "Never treated")
)

# Birth rate plot
birth_plot <- ggplot(df_stateyear, aes(x = YEAR, y = birth_rate, color = group)) +
  geom_line(size = 1.2) +
  annotate("rect", xmin = 1992, xmax = 2011, ymin = -Inf, ymax = Inf, alpha = 0.08) +
  labs(subtitle = "Birth rates", x = "Year", y = "Births per 1,000 Women", color = "Treatment status") +
  color_scale +
  scale_x_continuous(breaks = pretty_breaks(n = 7)) +
  scale_y_continuous(breaks = pretty_breaks(n = 6)) +
  guides(color = guide_legend(
    title.position = "top", title.hjust = 0.5,
    nrow = 2, byrow = TRUE,
    override.aes = list(size = 4), keywidth = 1.8, keyheight = 1.2
  )) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = c(0.05, 0.95),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "gray70", linewidth = 0.6),
    legend.box.background = element_rect(fill = "white", color = "gray60", linewidth = 0.6),
    legend.margin = margin(8, 8, 8, 8),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 11),
    plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5)
  )

# Employment plot
employment_plot <- ggplot(df_stateyear, aes(x = YEAR, y = emp_rate, color = group)) +
  geom_line(size = 1.2) +
  annotate("rect", xmin = 1992, xmax = 2011, ymin = -Inf, ymax = Inf, alpha = 0.08) +
  labs(subtitle = "Employment rates", x = "Year", y = "Employment Rate (%)") +
  color_scale +
  scale_x_continuous(breaks = pretty_breaks(n = 7)) +
  scale_y_continuous(breaks = pretty_breaks(n = 6)) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5)
  )

# Combined
combined <- (birth_plot | employment_plot) +
  plot_annotation(
    title = "Trends in Birth and Employment Rates by Treatment Status",
    subtitle = "Dashed line marks start of first Family Cap adoption (1992)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 12, colour = "grey30", hjust = 0.5)
    )
  ) &
  geom_vline(xintercept = 1992, linetype = "dashed", color = "black")

combined

ggsave("combined_birth_employment_plots.png", combined,
       width = 14, height = 6.5, dpi = 600, units = "in", bg = "white")
#####
# This part is redundant, only for diagnostics of model later on.
# The idea is to have a plot for each of the treat_start_year, so we can see whether pretrends hold for all of the groups or not.
####
df_stateyear <- df1 %>%
  group_by(YEAR, treat_start_year) %>%
  summarise(
    birth_rate = weighted.mean(birth_lastyear, w = ASECWT, na.rm = TRUE) * 1000,
    emp_rate   = weighted.mean(employed,     w = ASECWT, na.rm = TRUE) * 100,
    .groups = "drop"
  )

cohorts <- sort(na.omit(unique(df_stateyear$treat_start_year)))

plots <- map(cohorts, function(year_i) {
  
  df_plot <- df_stateyear %>%
    filter(is.na(treat_start_year) | treat_start_year == year_i) %>%
    mutate(
      group = if_else(is.na(treat_start_year), "Never treated",
                      paste0("Treated (", year_i, ")"))
    )
  
  ggplot(df_plot, aes(x = YEAR, y = birth_rate, color = group)) +
    geom_line(size = 1.1) +
    geom_vline(xintercept = year_i, linetype = "dashed", color = "black") +
    scale_x_continuous(breaks = seq(1982, 2011, 5)) +
    labs(
      title = paste("Birth Rates – Cohort Starting", year_i, "vs Never Treated"),
      subtitle = paste("Vertical line marks first policy year:", year_i),
      x = "Year", y = "Births per 1,000 Women", color = "Group"
    ) +
    scale_color_manual(values = c("firebrick", "steelblue")) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
})

wrap_plots(plots, ncol = 2)