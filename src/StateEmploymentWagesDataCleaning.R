# Import libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(Hmisc) # this one is for computing weighted median wages

##### Unemployment rate #####

# Import data
rawEmploymentData <- read_excel("../data/ststdnsadata.xlsx",
                                range = "A9:K31596",
                                col_names = c("STATEFIP", "STATE", "YEAR", "MONTH", "CNIP", "LABORFORCE", "PCT", "EMPLOYED", "EMPLOYMENTRATE", "UNEMPLOYED", "UNEMPLOYMENTRATE")
)

head(rawEmploymentData)

n_distinct(rawEmploymentData$STATEFIP)
unique(rawEmploymentData$STATEFIP)

# Keep only states
employmentData1 <- rawEmploymentData %>%
  filter(!STATEFIP %in% c("037", "51000"))

n_distinct(employmentData1$STATEFIP)

# Aggregate monthly unemployment rates into annual
annualEmploymentData <- employmentData1 %>%
  group_by(STATE, STATEFIP, YEAR) %>%
  summarise(
    UNEMPLOYMENTRATE = mean(UNEMPLOYMENTRATE, na.rm = TRUE),
  ) %>%
  ungroup()

# Get lag unemployment rates
annualEmploymentData <- annualEmploymentData %>%
  group_by(STATE) %>%
  mutate(lag_unemployment_rate = lag(UNEMPLOYMENTRATE)) %>%
  ungroup()

head(annualEmploymentData)
colSums(is.na(annualEmploymentData))

##### Female Median Weekly Wages #####

# Import data
rawFemaleWage <- read_csv("../data/cps_00006.csv", show_col_types = FALSE)

head(rawFemaleWage)
colSums(is.na(rawFemaleWage))

# Filter only for working women, avoiding case where number of hours worked = 0, but wage not 0 etc
rawFemaleWage1 <- rawFemaleWage %>%
  filter(WKSWORK1 > 0, INCWAGE > 0)

# Compute median wage for each state-year, weighted by ASECWT
medianWage <- rawFemaleWage1 %>%
  mutate(weekly_wage = INCWAGE / WKSWORK1) %>%
  group_by(STATEFIP, YEAR) %>%
  summarise(
    median_weekly_wage = wtd.quantile(weekly_wage, weights = ASECWT, probs = 0.5, na.rm = T)
  )

# Get lag weekly median wage
medianWage <- medianWage %>%
  group_by(STATEFIP) %>%
  mutate(lag_weekly_median_wage = lag(median_weekly_wage)) %>%
  ungroup()

head(medianWage)



# Save data
#saveRDS(annualEmploymentData, "../cleaned_data/annualEmployment.rds")
#saveRDS(medianWage, "../cleaned_data/medianWage.rds")