library(brms)
library(parallel)


numChains = 8


d = read.csv("../data/clean/data.csv",
             fileEncoding = "utf-8",
             encoding = 'utf-8')

fit_mm <- 
  brm(AAM ~ 1 + pcftr + 
        (1 | mm(G1, G2, G3, # genuses
                weights = cbind(G1.p, G2.p, G3.p))), 
      data   = d,
      warmup = 50,
      iter   = 100,
      chains = numChains)

save(fit_mm, file="../results/modelResults/AAM_vs_pcftr_MMME.Rdat")