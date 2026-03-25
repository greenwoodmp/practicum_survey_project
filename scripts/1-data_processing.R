rm(list=ls())

#### Preface ####

# This script will load a local version of the NSSAL-2 and NSSAL-3 survey data and
# prepare them for analysis. Data will be filtered for non-meaningful answers in the
# context of this study, formatting to ensure compatability between surveys, and
# then combined into an operable dataset.

# The dataset is concomitantly filtered down to columns of interest for analysis -
# het1yr (the dependent), rsex (gender; predictor), and reportingperiod (engineered
# time of survey predictor).

#### Loading packages ####
pkgs <- c("dplyr","foreign","here")
lapply(pkgs, require, character.only = TRUE)

#### Loading data ####
n2 <- read.dta(here("./data/raw/UKDA-5223-stata/NSSAL_2000.dta"))
n3 <- read.dta(here("./data/raw/UKDA-7799-stata/NSSAL_2010.dta"))

#### Processing data ####

## Add a column to each data set signifying the survey decade/time

# 2000 = 0
n2 <- n2 %>% mutate(reportingperiod=0,.before = 1)

# 2010 = 1
n3 <- n3 %>% mutate(reportingperiod=1,.before = 1)

## Recode gender/sex in each dataset

# n2: female=0, male=1
n2 <- n2 %>% mutate(rsex = case_when(rsex == "male" ~ 1, rsex == "female" ~ 0))

# n3: Female=0, Male=1
n3 <- n3 %>% mutate(rsex = case_when(rsex == "Male" ~ 1, rsex == "Female" ~ 0))

## Modify PSU columns to match between survey periods

# In n2 - get the unique serial IDs of respondents
n2$serial <- n2$sserial

# In n3 - get the unique serial IDs of respondents
n3$serial <- n3$sin2

# In n3 convert the PSU column name to match that in n2
n3$totalpsu <- n3$psu_scrm

## Trim n3 down so that it considers a similar age range to n2 (<=44)
n3 <- n3[n3$dage<=44,]

## Simultaneously reduce the data sets to desired columns and merge them
desired_cols <- c("serial", # unique respondent IDs
                  "total_wt", "strata", "totalpsu", # survey stratification and weighting variables
                  "reportingperiod","rsex", # independent variables
                  "dage", # potential covariate to account for
                  "het1yr" # dependent variable
                  )

n_final <- rbind(n2[,desired_cols],n3[,desired_cols])

## Filter out cases where answers are ambiguous or missing
out_codes_str <- c("At least one","at least one","at least two","at least three",
               "Not answered","not answered","Not applicable","not applicable")
out_codes_num <- c(995, 997, 996, 999, -1)

n_final <- n_final %>% filter(if_all(everything(), ~ !.x %in% out_codes_num)) %>%
  filter(if_all(everything(), ~ !.x %in% out_codes_str))

# Keep only complete cases
n_final <- n_final[complete.cases(n_final),]

# Ensure variables are correctly registered as factors, numerics, etc
n_final <- n_final %>%
  mutate(across(c("strata","totalpsu","reportingperiod","rsex","serial"), factor)) %>%
  mutate(across(c("total_wt","het1yr","dage"), as.numeric))


#### Export dataset ####

# Save as a csv
write.csv(n_final,here("./data/processed/natsal_filtered.csv"), row.names = FALSE)

# Also save as an R object so the data categories can be maintained
df <- n_final
save(df, file = here("./data/processed/natsal_filtered.RData"))
