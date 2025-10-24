# Importing relevant packages
library(tidyverse)
library(lubridate)

# Importing Dataset
rawPolicyData <- read_csv("../data/search-results.csv", show_col_types = FALSE)
head(rawPolicyData) 

# Handling of Policy's Start/End dates
clean <- rawPolicyData %>%
  mutate(
    # If the end date of policy is outside of our sample range, we set it to last possible date
    Policy_Date_End = if_else(Policy_Date_End == "Ongoing", "2011-12-31", Policy_Date_End),
    FamCapDate = na_if(FamCapDate, "n.a."),
    Policy_Date_Begin = ymd(Policy_Date_Begin),
    Policy_Date_End   = ymd(Policy_Date_End),
    FamCapDate        = mdy(FamCapDate) # FamCapDate is in the format of m/d/y
  )

state_policy <- clean %>%
  group_by(State) %>%
  summarise(
    ever_treated     = as.integer(any(FamCapExist == "Yes")), # ever_treated tells us if the state was ever treated at any point
    treat_start_year = min(coalesce(year(FamCapDate), year(Policy_Date_Begin))[FamCapExist == "Yes"], na.rm = TRUE),
    repeal_year = max(year(Policy_Date_End)[FamCapExist == "Yes"], na.rm = TRUE),
    # Coalesce is because there exists scenarios where FamCapDate is somehow before Policy_Date_Begin in the dataset
    .groups = "drop" # Ungroup after
  )%>%
  mutate(
    # this is so that treat_start_year/repeal_year is set to N.A. for those that weren't ever treated
    treat_start_year = if_else(is.infinite(treat_start_year), NA_real_, treat_start_year),
    repeal_year      = if_else(is.infinite(repeal_year), NA_real_, repeal_year)
  )

table(state_policy$ever_treated) # 24 treated, 30 never treated, but there's some that should be removed later on

# Next, we make it into panel data, aka making an entry for every (State, year) pair
t_min <- 1982; t_max <- 2011
state_year <- expand_grid(State = unique(state_policy$State),
                          year = t_min:t_max) %>%
  left_join(state_policy, by = "State") %>%
  mutate(
    treated = if_else(!is.na(treat_start_year) & year >= treat_start_year, 1L, 0L),
    treated = if_else(!is.na(repeal_year) & year > repeal_year, 0L, treated)
  )

state_year

# Save as RDS
#saveRDS(state_year, "../cleaned_data/policyImplementation.rds")