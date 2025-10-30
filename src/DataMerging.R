# Import packages
library(tidyverse)
library(lubridate)

# Import all cleaned datasets
annualEmployment <- readRDS("../cleaned_data/annualEmployment.rds")
indivCPS <- readRDS("../cleaned_data/indivCPS.rds")
medianWage <- readRDS("../cleaned_data/medianWage.rds")
policyImplementation <- readRDS("../cleaned_data/policyImplementation.rds")

##### Merge CPS with unemployment #####

# Making sure both STATEFIP and YEAR are numeric type
annualEmployment <- annualEmployment %>%
  mutate(
    STATEFIP = as.numeric(STATEFIP),
    YEAR = as.numeric(YEAR)
  )
head(annualEmployment)

# Pre-merger checks: seeing if there are any potential missing entries
sum(is.na(indivCPS$STATEFIP) | is.na(indivCPS$YEAR))
sum(is.na(annualEmployment$STATEFIP) | is.na(annualEmployment$YEAR))

annualEmployment %>%
  count(STATEFIP, YEAR) %>%
  filter(n > 1)

anti_join(indivCPS, annualEmployment, by = c("STATEFIP", "YEAR")) %>%
  distinct(STATEFIP, YEAR)

# shows observations in annualEmployment that isn't in indivCPS
anti_join(annualEmployment, indivCPS, by = c("STATEFIP", "YEAR")) %>%
  distinct(YEAR)
# Only extra entries are the years not accounted for in our project (pre 1982 and post 2011)

# Finally, merge CPS with unemployment data (by YEAR, STATEFIP)
mergedData1 <- indivCPS %>%
  left_join(annualEmployment, by = c("STATEFIP", "YEAR"))

head(mergedData1)

##### Merge with median weekly wage #####
# Pre-merger checks
sum(is.na(medianWage$STATEFIP)  | is.na(medianWage$YEAR))
medianWage %>% count(STATEFIP, YEAR) %>% filter(n > 1)
anti_join(mergedData1, medianWage, by=c("STATEFIP","YEAR")) %>% distinct(STATEFIP,YEAR)
anti_join(medianWage, mergedData1, by=c("STATEFIP","YEAR")) %>% distinct(STATEFIP,YEAR) 

# Merge
mergedData2 <- mergedData1 %>%
  left_join(medianWage, by = c("STATEFIP", "YEAR"))

head(mergedData2)

##### Merge with policy implementation year #####
head(policyImplementation)

# Keeping column names consistent
policyImplementation <- policyImplementation %>%
  rename(
    STATE = State,
    YEAR = year
  )

head(policyImplementation)

# Pre-merger checks
sum(is.na(policyImplementation$STATE) | is.na(policyImplementation$YEAR))
sum(is.na(mergedData2$STATE) | is.na(mergedData2$YEAR))
policyImplementation %>% count(STATE, YEAR) %>% filter(n > 1)

anti_join(mergedData2, policyImplementation, by=c("STATE","YEAR")) %>% distinct(STATE,YEAR)
anti_join(policyImplementation, mergedData2, by=c("STATE","YEAR")) %>% distinct(STATE)

# Finally, merge
mergedData3 <- mergedData2 %>%
  left_join(policyImplementation, by = c("STATE", "YEAR"))

# For convenience, arrange the columns
mergedData4 <- mergedData3 %>% 
  select(YEAR, STATEFIP, STATE, ASECWT, ever_treated, treat_start_year, treated, 
         UNEMPLOYMENTRATE, lag_unemployment_rate, median_weekly_wage, lag_weekly_median_wage, 
         everything())

head(mergedData4)
colSums(is.na(mergedData4))

# Save as RDS
#saveRDS(mergedData4, "../cleaned_data/finalmerged.rds")