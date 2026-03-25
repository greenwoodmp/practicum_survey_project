rm(list=ls())

#### Preface ####

# This script will load the filtered, combined NSSAL-2 and NSSAL-3 survey data sets.
# The data will be described and explored in a factorial manner - each combination
# or cell formed by the independent variables (gender, reportingperiod) will be
# assessed separately.

# Summary statistics, formal tests of normality, and formal tests for homogeneity
# of variance across groups will be produced.

#### Loading packages ####
pkgs <- c("here","ggplot2", "survey", "moments","car")
lapply(pkgs, require, character.only = TRUE)

#### Loading data ####

# Retrieve dataframe
load(here("./data/processed/natsal_filtered.RData"))

#### Processing data ####

# Add a column that can be used to identify the cells formed by gender and reportingperiod
df$cell <- paste(df$reportingperiod,df$rsex, sep="-")

# Recode and factorize the column
df$cell <- case_when(df$cell == "0-0" ~ "F-2000",
                     df$cell == "1-0" ~ "M-2000",
                     df$cell == "0-1" ~ "F-2010",
                     df$cell == "1-1" ~ "M-2010")
df$cell <- factor(df$cell, levels=c("F-2000","M-2000","F-2010","M-2010"))

#### Accounting for survey structure ####

# Make a survey design object
natsal_design <- svydesign(ids = ~totalpsu,
                           strata = ~strata,
                           weights = ~total_wt,
                           data = df,
                           nest = TRUE
                           )

#### Descriptives per cell - weighted ####

# Means
weighted_cell_means <- svyby(~het1yr,
                             ~rsex+reportingperiod,
                             svymean,
                             design=natsal_design)
weighted_cell_means

# Variance
weighted_cell_vars <- svyby(~het1yr,
                             ~rsex+reportingperiod,
                             svyvar,
                             design=natsal_design)
weighted_cell_vars

#### Boxplot of weighted data ####

svyboxplot(het1yr~cell, natsal_design,
           main="Weighted het1yr per cell",
           )

#### Descriptives per cell - raw ####

# Means, variance, and skewness
cell_means_vars_skew <- df %>%
  group_by(reportingperiod, rsex)%>%
  summarise(het1yr_avg=mean(het1yr), var=sd(het1yr)^2, skewness=skewness(het1yr))

cell_means_vars_skew

#### Tests of normality ####

# Some of the cells are too large (>5000) for a formal shapiro-wilk test
# Accordingly, qq plots were examined
par(mfrow=c(2,2))
qqnorm(df$het1yr[df$cell=="F-2000"], main="QQ-plot for F-2000"); qqline(df$het1yr[df$cell=="F-2000"], col = 2)
qqnorm(df$het1yr[df$cell=="F-2010"], main="QQ-plot for F-2010"); qqline(df$het1yr[df$cell=="F-2010"], col = 2)
qqnorm(df$het1yr[df$cell=="M-2000"], main="QQ-plot for M-2000"); qqline(df$het1yr[df$cell=="M-2000"], col = 2)
qqnorm(df$het1yr[df$cell=="M-2010"], main="QQ-plot for M-2010"); qqline(df$het1yr[df$cell=="M-2010"], col = 2)

#### Test of homogeneity of variance ####
leveneTest(het1yr~cell,df)

