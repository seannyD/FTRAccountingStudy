library(brms)
library(parallel)

numChains = 8

load("../data/clean/data_min.Rdat")

fit_mm <- 
  brm(AAM ~ 1 + pcftr + 
        (1 | mm(G1, G2, G3, # genuses
                weights = cbind(G1.p, G2.p, G3.p))), 
      data   = d,
      warmup =  1000,
      iter   = 11000,
      chains = numChains,
      cores = numChains)

save(fit_mm, file="../results/modelResults/AAM_vs_pcftr_MMME.Rdat")