rm(list=ls())

library(survey)
options(survey.lonely.psu = "adjust")

load(here("./data/processed/natsal_filtered.RData"))

df_final <- df

natsal_design <- svydesign(ids=~totalpsu, strata=~strata, weights=~total_wt,
                           data=df, nest=TRUE)

# 1) WEIGHTED MEAN PLOT 

m1 <- svyby(~het1yr, ~rsex + reportingperiod,
            design = natsal_design, FUN = svymean, na.rm = TRUE)

m1$mean <- m1$het1yr

f <- subset(m1, rsex == "0"); f <- f[order(f$reportingperiod), ]
m <- subset(m1, rsex == "1");   m <- m[order(m$reportingperiod), ]

x <- c(1, 2)
ylim_all <- range(c(f$mean - f$se, f$mean + f$se,
                    m$mean - m$se, m$mean + m$se), na.rm = TRUE)

# Start plotting and export as 8x6 landscape
png(filename=here("./report/images/sophis_glm_interaction_plot.png"),
    width=8, height=6,units="in", res=300)

plot(x, f$mean, type="b", pch=16, lty=1,
     xaxt="n", xlab="Wave",
     ylab="Weighted mean # heterosexual partners (last year)",
     ylim=ylim_all,
     main="Weighted mean partners by gender")
axis(1, at=x, labels=c("2000","2010"))

lines(x, m$mean, type="b", pch=17, lty=2)

arrows(x, f$mean - f$se, x, f$mean + f$se, angle=90, code=3, length=0.05)
arrows(x, m$mean - m$se, x, m$mean + m$se, angle=90, code=3, length=0.05)

legend("bottomright", legend=c("female","male"),
       pch=c(16,17), lty=c(1,2), bty="n")

dev.off()

# 2) RAW DISTRIBUTION PLOT 
df_final$grp <- factor(
  paste(ifelse(df_final$reportingperiod==0,"2000","2010"), ifelse(df_final$rsex==0,"F","M")),
  levels = c("2000 F","2000 M","2010 F","2010 M")
)

# Start plotting and export as 8x3 landscape
png(filename=here("./report/images/raw_data_boxplot.png"),
    width=8, height=4,units="in", res=300)
boxplot(
  het1yr ~ grp,
  data = df_final,
  ylab = "# heterosexual partners 1yr",
  xlab = "",
  main = "Raw (unweighted) distribution: 1-year partners",
  outline = TRUE
)
dev.off()

# 3) RESIDUALS vs LEVERAGE
m_diag <- glm(
  het1yr_clean ~ rsex * reportingperiod + dage,
  family = quasipoisson(),
  data = df_final,
  weights = total_wt
)
plot(m_diag, which = 5)


# 4) WEIGHTED BOXPLOT BY CELL (weighted quantiles)
natsal_design <- update(natsal_design,
                        cell = interaction(reportingperiod, rsex, sep = " "))

cells <- c("2000 female","2000 male","2010 female","2010 male")
probs <- c(0.05, 0.25, 0.50, 0.75, 0.95)

q_list <- lapply(cells, function(cl) {
  des_cl <- subset(natsal_design, cell == cl)
  as.numeric(coef(svyquantile(~het1yr_clean, des_cl,
                              quantiles = probs, ci = FALSE, na.rm = TRUE)))
})

stats_mat <- do.call(cbind, q_list)
colnames(stats_mat) <- c("2000 F","2000 M","2010 F","2010 M")

bxp(list(
  stats = stats_mat,
  n = rep(NA, length(cells)),
  names = colnames(stats_mat)
),
main = "Weighted boxplot by cell",
ylab = "# heterosexual partners last year\n(weighted quantiles; whiskers=5th/95th)",
xlab = ""
)
