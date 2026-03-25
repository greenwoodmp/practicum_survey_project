rm(list=ls())

#### Preface ####

# This script will implement a generalized linear model (GLM) to guage the factors that
# shape sexual activity. An important feature of this model is the inclusion
# of the interaction term between gender and reportingperiod, the significance of
# which will directly address the research question at hand.

# In conducting this GLM, we use the survey package to account for stratification
# in the data set and, so, the results will describe patterns that can be generalized
# to the entire British population

#### Loading packages ####
pkgs <- c("here","ggplot2", "survey")
lapply(pkgs, require, character.only = TRUE)

options(survey.lonely.psu = "adjust")

#### Loading data ####

# Retrieve dataframe
load(here("./data/processed/natsal_filtered.RData"))

#### Accounting for the survey design ####

# Generate a survey design object from the dataframe
natsal_design <- svydesign(ids = ~totalpsu,
                           strata = ~strata,
                           weights = ~total_wt,
                           data = df,
                           nest = TRUE
                           )

# Allow single/lonely PSUs in the survey design
options(survey.lonely.psu="adjust")

#### Statistical modelling ####

# We will model het1yr using the two factors of interest, rsex and reportingperiod

### Preliminary Poisson GLM

prelim_model <- svyglm(
  het1yr ~ rsex * reportingperiod,
  design = natsal_design,
  family = poisson()
)

# Check overdispersion parameter
deviance(prelim_model)/prelim_model$df.residual

### Naive quasiPoisson GLM

## Build and fit the model

# We stipulate an error term with a quasipoisson distribution to account
# for overdispersion in the data
model_weighted <- svyglm(
  het1yr ~ rsex * reportingperiod,
  design = natsal_design,
  family = quasipoisson()
)

### Naive GLM

# Model het1yr using the two factors of interest, gender and time

## Build and fit the model

# We stipulate an error term with a quasipoisson distribution to account
# for overdispersion in the data
model_weighted <- svyglm(
  het1yr ~ rsex * reportingperiod,
  design = natsal_design,
  family = quasipoisson()
)

## Assess the model

# Output summary
summary(model_weighted)

# Plots of model fit
par(mfrow=c(2,2), mar=margin(4,4,4,4))
plot(model_weighted)

## Graph coefficients 
model_weighted_coef <- data.frame(est=coef(model_weighted),
                                  est_025 = confint(model_weighted)[,1],
                                  est_975=confint(model_weighted)[,2],
                                  exp_est=exp(coef(model_weighted)),
                                  exp_est_025=exp(confint(model_weighted)[,1]),
                                  exp_est_975=exp(confint(model_weighted)[,2])
                                  )

model_weighted_coef$coef <- factor(row.names(model_weighted_coef),
                                   levels=c("rsex1:reportingperiod1","reportingperiod1","rsex1","(Intercept)")
                                   )

coef_plot <- ggplot(model_weighted_coef, aes(y=coef, x=est))+
  geom_point(size=3)+
  geom_errorbar(aes(xmin=est_025,xmax=est_975), width=0.2)+
  geom_vline(xintercept = 0, linetype="dashed", lwd=1)+
  theme_bw()+
  labs(y="Model coefficient", x="Estimate")

# Export as 8x3 landscape
## Explore incidence risk ratios (IRRs)

# Convert coefficients to IRRs
exp(coef(model_weighted))

# Get Confidence Intervals for those IRRs
exp(confint(model_weighted))

## Visualization of effects using weighted data

# Main effect of gender
gen_weighted_means <- svyby(~het1yr,
                            ~rsex,
                            design = natsal_design,
                            svymean,
                            na.rm = TRUE)

gen_eff <- ggplot(gen_weighted_means, aes(x = rsex, y = het1yr, group = rsex)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = het1yr - se, ymax = het1yr + se), width = 0.1) +
  labs(
    title = "Mean Number of Heterosexual Partners (Past Year)",
    subtitle = "Adjusted for NATSAL cluster, strata, and weights",
    x = "Gender",
    y = "Mean Partners"
  ) +
  theme_minimal()+
  scale_y_continuous(limits=c(0,max(gen_weighted_means$het1yr+gen_weighted_means$se)+0.1))

# Main effect of time
rep_weighted_means <- svyby(~het1yr,
                            ~reportingperiod,
                            design = natsal_design,
                            svymean,
                            na.rm = TRUE)

rep_eff <- ggplot(rep_weighted_means, aes(x = reportingperiod, y = het1yr, group = reportingperiod)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = het1yr - se, ymax = het1yr + se), width = 0.1) +
  labs(
    title = "Mean Number of Heterosexual Partners (Past Year)",
    subtitle = "Adjusted for NATSAL cluster, strata, and weights",
    x = "Decade",
    y = "Mean Partners"
  ) +
  theme_minimal()+
  scale_y_continuous(limits=c(0,max(rep_weighted_means$het1yr+rep_weighted_means$se)+0.1))

### Age-aware GLM

# Model het1yr using the two factors of interest, gender and time, and accounting
# for the potential relationship between age and het1yr

## Build and fit the model

# We stipulate an error term with a quasipoisson distribution to account
# for overdispersion in the data
model_weighted_cov <- svyglm(
  het1yr ~ rsex * reportingperiod+dage,
  design = natsal_design,
  family = quasipoisson()
)

## Assess the model

# Output summary
summary(model_weighted_cov)

# Plots of model fit
par(mfrow=c(2,2), mar=margin(4,4,4,4))
plot(model_weighted_cov)

# Get the leverage plot; manually exported as a 8x4 landscape pdf
plot(model_weighted_cov)

## Graph coefficients

# Make a dataframe of values
model_weighted_cov_coef <- data.frame(est=coef(model_weighted_cov),
                                  est_025 = confint(model_weighted_cov)[,1],
                                  est_975=confint(model_weighted_cov)[,2],
                                  exp_est=exp(coef(model_weighted_cov)),
                                  exp_est_025=exp(confint(model_weighted_cov)[,1]),
                                  exp_est_975=exp(confint(model_weighted_cov)[,2])
)

model_weighted_cov_coef$coef <- factor(row.names(model_weighted_cov_coef),
                                   levels=c("dage","rsex1:reportingperiod1","reportingperiod1","rsex1","(Intercept)")
)

# Plot
coef_cov_plot <- ggplot(model_weighted_cov_coef, aes(y=coef, x=est))+
  geom_point(size=3)+
  geom_errorbar(aes(xmin=est_025,xmax=est_975), width=0.2)+
  geom_vline(xintercept = 0, linetype="dashed", lwd=1)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y="Model coefficient", x="Estimate", title="het1yr ~ rsex*reportingperiod +dage")

# Export as 8x3
png(filename=here("./report/images/sophis_glm_coef_plot.png"),
    width=8, height=3,units="in", res=300)
coef_cov_plot
dev.off()

## Explore coefficients and incidence risk ratios (IRRs)

# Convert coefficients to IRRs
exp(coef(model_weighted_cov))

# Get Confidence Intervals for those IRRs
exp(confint(model_weighted_cov))

# Main effect of gender
gen_weighted_means <- svyby(~het1yr,
                            ~rsex,
                            design = natsal_design,
                            svymean,
                            na.rm = TRUE)

gen_eff <- ggplot(gen_weighted_means, aes(x = rsex, y = het1yr, group = rsex)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = het1yr - se, ymax = het1yr + se), width = 0.1) +
  labs(
    title = "Mean Number of Heterosexual Partners (Past Year)",
    subtitle = "Adjusted for NATSAL cluster, strata, and weights",
    x = "Gender",
    y = "Mean Partners"
  ) +
  theme_minimal()+
  scale_y_continuous(limits=c(0,max(gen_weighted_means$het1yr+gen_weighted_means$se)+0.1))


### Model comparison

anova(model_weighted,model_weighted_cov, test="Chisq")
