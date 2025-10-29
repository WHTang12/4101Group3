library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(fixest)
library(didimputation)
library(did)

##Data Loading
df <- readRDS("finalmerged.rds")
df1 <- df %>%
  filter(is.na(repeal_year) | YEAR <= repeal_year)
df1$treat_start_year[is.na(df1$treat_start_year)] <- 0

##ATT
att_cs <- att_gt(
  yname   = "birth_lastyear",
  tname   = "YEAR",
  gname   = "treat_start_year",
  xformla = ~ AGE + NCHILD + EDUC + RACE + MARST,
  control_group = "nevertreated",
  weightsname = "ASECWT",
  data    = df1,
  panel = F,
  clustervars = "STATEFIP"
)

summary(att_cs)

agg_cs <- aggte(att_cs, type = "group") 
summary(agg_cs) 

agg_dyn <- aggte(att_cs, type = "dynamic") 
summary(agg_dyn) 

ggdid(agg_dyn)

pre <- agg_dyn$att.egt[agg_dyn$egt < 0]
pre_se <- agg_dyn$se.egt[agg_dyn$egt < 0]
wald_stat <- sum((pre / pre_se)^2)
df <- length(pre)
pval <- 1 - pchisq(wald_stat, df)
pval  # formal pretrend p-value

##IW Estimator
m_birth <- feols(
  birth_lastyear ~ sunab(treat_start_year, YEAR) + AGE + EDUC + RACE + MARST |
    STATEFIP + YEAR,
  data = df1,
  weights = ~ASECWT,
  cluster = ~STATEFIP
)

summary(m_birth)

etable(m_birth, se = "cluster")

post <- coef(m_birth)[grepl("event::", names(coef(m_birth))) & !grepl("event_\\-", names(coef(m_birth)))]
mean(post)   # simple average of post coefficients

iplot(m_birth, ref.line = 0,
      xlab = "Event Time (Years)",
      main = "Event-Study Estimates (Sun & Abraham, 2021)")



##Imputation Estimator
df_state <- df1 %>%
  group_by(STATEFIP, YEAR, treat_start_year) %>%
  summarise(
    birth_rate = weighted.mean(birth_lastyear, ASECWT, na.rm = TRUE),
    AGE   = weighted.mean(AGE,   ASECWT, na.rm = TRUE),
    EDUC  = weighted.mean(EDUC,  ASECWT, na.rm = TRUE),
    RACE  = weighted.mean(RACE,  ASECWT, na.rm = TRUE),
    MARST = weighted.mean(MARST, ASECWT, na.rm = TRUE),
    .groups = "drop"
  )

bjs_results <- did_imputation(
  data        = df_state,
  yname       = "birth_rate",
  tname       = "YEAR",
  gname       = "treat_start_year",
  idname      = "STATEFIP",
  cluster_var = "STATEFIP"
)

summary(bjs_results)

es_data <- bjs_results$event_study  # sometimes NULL if not computed

if(!is.null(es_data)) {
  head(es_data)
}

df_state$event_time <- df_state$YEAR - df_state$treat_start_year
df_event <- df_state %>%
  filter(!is.na(event_time) & event_time >= -5 & event_time <= 5)

event_model <- feols(
  birth_rate ~ i(event_time, ref = -1) + AGE + EDUC + RACE + MARST |
    STATEFIP + YEAR,
  data = df_event,
  cluster = ~STATEFIP
)

iplot(event_model, ref.line = 0, main = "Imputation-based Pretrend Check")

pre_coefs <- coef(event_model)[grepl("event_time::-", names(coef(event_model)))]
pre_se <- sqrt(diag(vcov(event_model)))[grepl("event_time::-", names(coef(event_model)))]
wald_stat <- sum((pre_coefs / pre_se)^2)
df <- length(pre_coefs)
pval <- 1 - pchisq(wald_stat, df)
pval
