library(readr)

# read CSV
cps <- read_csv("../data/cps_00004.csv")

df <- read_csv("../data/search-results.csv")

length(unique(policy$State))

# Example: load your data
# df <- read.csv("your_file.csv")
library(dplyr)
library(ggplot2)
library(lubridate)

# Keep only treated states
df_treated <- df %>%
  filter(FamCapExist == "Yes" & State != "Guam") %>%
  mutate(FamCapYear = year(mdy(FamCapDate)))   # adjust if FamCapDate format differs

# First adoption year per treated state
adoption <- df_treated %>%
  group_by(State) %>%
  summarise(FirstYear = min(FamCapYear, na.rm = TRUE))

# Add custom policy end years
end_years <- tibble(
  State = c("Wyoming", "Oklahoma", "Illinois", "Maryland", "Nebraska"),
  EndYear = c(2008, 2009, 2004, 2004, 2007)
)

# Merge: default end year = 2010 unless overridden
adoption <- adoption %>%
  left_join(end_years, by = "State") %>%
  mutate(EndYear = ifelse(is.na(EndYear), 2010, EndYear)) %>%
  mutate(State = reorder(State, -FirstYear))

# Timeline plot with capped lines
ggplot(adoption, aes(y = State)) +
  # lines from FirstYear to EndYear
  geom_segment(aes(x = FirstYear, xend = EndYear, yend = State), color = "steelblue") +
  
  # green dot for first implementation
  geom_point(aes(x = FirstYear), color = "darkgreen", size = 3) +
  
  # red dot only for states with specified end year (EndYear < 2010)
  geom_point(data = subset(adoption, EndYear < 2010),
             aes(x = EndYear), color = "red", size = 3) +
  
  labs(title = "Staggered Adoption Timeline of Family Cap",
       x = "Year",
       y = "State") +
  scale_x_continuous(limits = c(1982, 2010), breaks = seq(1982, 2010, 2)) +
  theme_minimal()+
  theme(panel.grid = element_blank())

ggsave("FamCap_Timeline.png", width = 10, height = 8, dpi = 300)


