library(brms)
try(setwd("~/Documents/Bristol/FTRAccounting/FTRAccountingStudy/analysis/"))

# All var, 5000 datapoints, 10k runs
load("../results/modelResults/AAM_vs_pcftr_MMME_613644439301.Rdat")
# With random slopes
load("../results/modelResults/AAM_vs_pcftr_MMME_10k_277795389894.Rdat")

summary(fit_mm)

plot(fit_mm)
plot(marginal_effects(fit_mm), ask = FALSE)

stanplot(fit_mm,type="trace",pars='G1')

stanplot(fit_mm)
stanplot(fit_mm, pars = c("pcftr","invpro","pd",'ggr','SIZE',"BTM","LEV","ROA","ISSUE","MEET","LOSS")) + geom_vline(aes(xintercept=0))
