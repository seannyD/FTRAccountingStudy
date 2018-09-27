library(brms)
library(parallel)

numChains = 1

load("../data/clean/data_min.Rdat")

d$AAM = d$AAM + 0.00001
d = d[sample(1:nrow(d),10000),]

fit_mm <- 
  brm(AAM ~ 1 + pcftr + 
        invpro+pd+indiv+mas+ua+lto+
        indul+ggr+SIZE+BTM+LEV+ROA+
        ISSUE+MEET+LOSS+
        (1 | gvkey) +
        (1 | fyear) +
        (1 | indus) +
        (1 + pcftr | mm(G1, G2, G3, # genuses
                weights = cbind(G1.p, G2.p, G3.p))), 
      data   = d,
      family = exponential,
      warmup =  1000,
      iter   = 11000,
      save_dso = TRUE,
      thin =   1,
      chains = 1)

filename = paste0("../results/modelResults/AAM_vs_pcftr_MMME_10k_",
                  paste0(sample(0:9,12,replace=T),collapse = ""),
                  ".Rdat")
save(fit_mm, file=filename)
