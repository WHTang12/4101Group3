# 4101Group3
# U.S. Family Cap Policy Analysis

This repository contains the code and data used to estimate the causal effects of the U.S. Family Cap policy on fertility and women’s employment.

The analysis compares three modern DiD estimators, C&S (2021), S&A (2021) and BJS (2024)

# Structure
```
├── cleaned_data/ # Processed datasets after cleaning and merging
├── data/ # Raw data inputs (CPS, BLS, Welfare Rules)
├── modelresults/ # Saved model outputs (RData objects for CS)
├── plots/ # Generated figures
├── src/ # R Scripts
│ ├── CPSIndivDataCleaning.R
│ ├── PolicyDataCleaning.R
│ ├── StateEmploymentWagesDataCleaning.R
│ ├── DataMerging.R
│ ├── GTATT.R # Callaway & Sant’Anna Group-time estimator
│ ├── interaction-weighted.R # Sun & Abraham estimator
│ ├── TWFE.R # Simple TWFE
│ ├── SimplePretrendsPlot.R # Visualization of pre-trends (for presentation only)
│ ├── FamCap_Timeline.R # Plotting policy adoption timeline
│ ├── Final Imputation Code.R # Borusyak et al's Imputation estimator
│ └── DSE4101 Project code.R
```

# Data

The analysis uses a cleaned dataset saved as ```../cleaned_data/finalmerged.rds```, which contains state-year level records with treatment timing, demographic controls, and economic variables.

For the imputation and weighting estimators, data is aggregated to the state-year level using weighted means by ASECWT. The resulting dataset is stored as df_state:

```df_state <- df1 %>%
  group_by(STATEFIP, YEAR, treat_start_year) %>%
  summarise(
    birth_lastyear = weighted.mean(birth_lastyear, ASECWT, na.rm = TRUE),
    employed = weighted.mean(employed, ASECWT, na.rm = TRUE),
    AGE   = weighted.mean(AGE,   ASECWT, na.rm = TRUE),
    NCHILD   = weighted.mean(NCHILD,   ASECWT, na.rm = TRUE),
    EDUC  = weighted.mean(EDUC,  ASECWT, na.rm = TRUE),
    MARST = weighted.mean(MARST, ASECWT, na.rm = TRUE),
    ELDCH = weighted.mean(ELDCH, ASECWT, na.rm = TRUE),
    black = weighted.mean(black, ASECWT, na.rm = TRUE), 
    white = weighted.mean(white, ASECWT, na.rm = TRUE),
    lag_unemployment_rate = weighted.mean(lag_unemployment_rate, ASECWT, na.rm = TRUE), 
    lag_weekly_median_wage = weighted.mean(lag_weekly_median_wage, ASECWT, na.rm = TRUE),
    .groups = "drop"
  )
```

# Analyses
## 1. Callaway & Sant’Anna Group-Time ATT Estimator

The R script GTATT.R contains the full analysis for the group-time ATT estimator. Due to computational intensity, model outputs are saved in the modelresults folder and loaded as needed to ensure efficient and reproducible analysis.

## 2. Sun and Abraham’s Interaction-Weighted Estimator

The analysis uses the interaction-weighted (IW) Difference-in-Differences estimator developed by Sun & Abraham (2021), implemented via the ```sunab()``` function in the ```fixest``` package. 

## 3. BJS Imputation-Based DiD Estimator

The R script Final Imputation Code.R (located in the src folder) runs the imputation-based Difference-in-Differences estimator using ```did_imputation()```. Event-study and average treatment effects (ATTs) are estimated as follows:

```imp_birth <- did_imputation(
  data        = df_state,
  yname       = "birth_lastyear",
  gname       = "treat_start_year",
  tname       = "YEAR",
  idname      = "STATEFIP",
  horizon     = TRUE,
  pretrends   = TRUE,
  cluster_var = "STATEFIP"
)
```
Analogous models are run for employed. Event-study plots display dynamic treatment effects with 95% confidence intervals. Setting horizon = FALSE yields the average ATT.
