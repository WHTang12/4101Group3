# Can ignore this file, it's only for visualization of adoption timeline and was made before any analysis was done

# data
cps <- read_csv("../data/cps_00004.csv")
df <- read_csv("../data/search-results.csv")

library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

# Keep only treated states
df_treated <- df %>%
  filter(FamCapExist == "Yes" & State != "Guam") %>%
  mutate(FamCapYear = year(mdy(FamCapDate)))

# First adoption year per treated state
adoption <- df_treated %>%
  group_by(State) %>%
  summarise(FirstYear = min(FamCapYear, na.rm = TRUE))

# Add custom policy end years
end_years <- tibble(
  State = c("Wyoming", "Oklahoma", "Illinois", "Maryland", "Nebraska"),
  EndYear = c(2008, 2009, 2004, 2004, 2007)
)

adoption <- adoption %>%
  left_join(end_years, by = "State") %>%
  mutate(EndYear = ifelse(is.na(EndYear), 2010, EndYear)) %>%
  mutate(State = reorder(State, -FirstYear))

ref_years <- c(1992, 1994, 1995, 1996, 1997, 1998, 2003)
ref_df    <- data.frame(year = ref_years)

ggplot(adoption, aes(y = State)) +
  geom_vline(data = ref_df, aes(xintercept = year),
             linetype = "dashed", color = "grey80", linewidth = 0.3, alpha = 0.7) +
  
  geom_segment(aes(x = FirstYear, xend = EndYear, yend = State), color = "#3a6ea5", linewidth = 0.6) +
  
  geom_point(aes(x = FirstYear), shape = 21, size = 2.8, stroke = 0.5,
             fill = "#1b7f3a", color = "white") +
  
  geom_point(data = subset(adoption, EndYear < 2010),
             aes(x = EndYear), shape = 21, size = 2.8, stroke = 0.5,
             fill = "#c7362f", color = "white") +
  
  labs(title = "Staggered Adoption Timeline of Family Cap",
       x = "Year", y = "State",
       caption = "Green = first adoption, Red = repeal") +
  scale_x_continuous(limits = c(1982, 2010), breaks = seq(1982, 2010, 1)) +
  scale_y_discrete(expand = expansion(add = c(0.5, 0.5))) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey92"),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(color = "grey40", hjust = 0),
    axis.title.y = element_blank(),
    plot.margin = margin(10, 20, 10, 10)
  )

# ggsave(
#   filename = "FamCap_Timeline.png",
#   plot = last_plot(),   
#   width = 12,      
#   height = 8,      
#   dpi = 600,          
#   units = "in"       
# )
