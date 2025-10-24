# Import libraries
library(readr)
library(dplyr)
library(lubridate)

# Import Data
rawCPS <- read_csv("../data/cps_00004.csv", show_col_types = FALSE)
head(rawCPS)

# Filtering for 
# 1. Unmarried (Not 1, 2) 
# 2. women (2) 
# 3. aged between 18 and 45 
# 4. with at most a high school degree (<= 73)
CPS1 <- rawCPS %>%
  filter(SEX == 2, AGE >= 18, AGE <= 45, EDUC <= 73, (MARST %in% c(3, 4, 5, 6, 7)))


# Checking for NA's
colSums(is.na(CPS1)) # NA's present in UHRSWORKT

# Checking which years contain the NA's, which are outside of our sample range (pre 1994), so not a big issue
print(CPS1 %>%
  group_by(YEAR) %>%
  summarise(
    n_total = n(),
    n_na = sum(is.na(UHRSWORKT)),
    share_na = mean(is.na(UHRSWORKT))
  ), n = 100)

# Checking if any weird values/outliers, which there are
summary(CPS1)

# Cleaning "Total hours worked last week" (according to codebook)
CPS2 <- CPS1 %>%
  mutate(
    UHRSWORKT = case_when(
      UHRSWORKT == 997 ~ NA_real_,    # hours vary -> NA
      UHRSWORKT == 999 ~ 0,            # not applicable means 0
      TRUE ~ UHRSWORKT
    )
  )

# Creating outcome variable: "Employed" and "Fertility" (using whether youngest child <= 1yo)
CPS3 <- CPS2 %>%
  mutate(
    employed = ifelse(EMPSTAT %in% c(10, 12), 1, 0), # creating "employed" outcome indicator
    birth_lastyear = ifelse(YNGCH <= 1, 1, 0) # creating "fertility" outcome indicator
  )

table(CPS3$birth_lastyear)
table(CPS3$employed)

# Creating Race dummy variables
CPS4 <- CPS3 %>%
  mutate(
    black = if_else(RACE == 200, 1, 0),
    white = if_else(RACE == 100, 1, 0),
    other = if_else(!RACE %in% c(100, 200), 1, 0)
  )

# Lastly, check for NA's again
colSums(is.na(CPS4))

# Save as RDS
saveRDS(CPS4, "../cleaned_data/indivCPS.rds")
