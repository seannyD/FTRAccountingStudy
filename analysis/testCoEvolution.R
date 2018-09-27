
d = read.csv("../data/clean/data.csv",
             fileEncoding = "utf-8",
             encoding = 'utf-8')

countryMainLanguageFamily =
  read.csv("../data/raw/CountryMainLanguageToLanguageFamily.csv",
           stringsAsFactors = F)

d$mainLanguageFamily =
  countryMainLanguageFamily[
    match(as.character(d$loc),
          countryMainLanguageFamily$Country.Code),
    ]$Family

d = d[d$mainLanguageFamily=="Indo-European",]

d$DPlaceLang =
  countryMainLanguageFamily[
    match(as.character(d$loc),
          countryMainLanguageFamily$Country.Code),
    ]$DPlaceLang

dplaceLangs = countryMainLanguageFamily$DPlaceLang[countryMainLanguageFamily$DPlaceLang!=""]

library(ape)
tree = read.nexus(file = "../data/raw/trees/bouckaert_et_al2012-d-place_2.NEXUS")

tree = drop.tip(tree,tree$tip.label[!tree$tip.label %in% dplaceLangs])

library(caper)
library(phytools)

DP.FTR = tapply(d$strongftr,d$DPlaceLang,head,n=1)

cdata = data.frame(
  FTR = DP.FTR,
  lang = names(DP.LTO)
)
cdata = cdata[cdata$lang!="",]

cultVars = c("lto","trustr",'religc','indiv','mas','pd',"AAM")

for(v in cultVars){
  cdata[,v] = tapply(d[d$DPlaceLang!="",v],
                     d[d$DPlaceLang!="",]$DPlaceLang,
                     mean,na.rm=T)
}

cdata = cdata[cdata$lang %in% tree$tip.label,]
cdata$FTR = as.factor(cdata$FTR)
for(v in cultVars){
  cdata[,v] = scale(cdata[,v])
}

library(MCMCglmm)

prior.PN<-list(
  G=list(
    G1=list(V=1,nu=0.002)),
  R=list(V=1,nu=0.002))

burnin = 10000
postBurnin =100000
thin = 10#postBurnin/10000

for(v in cultVars){
  fm = formula(paste(v,"~","FTR"))
  model0<-MCMCglmm(
    fm,
    random=~lang, 
    ginverse=list(
      lang=inverseA(tree)$Ainv), 
    prior = prior.PN, 
    verbose=FALSE, 
    family="gaussian",
    data = cdata,
    nitt=burnin+postBurnin, 
    thin=thin, 
    burnin=burnin) 
  print(summary(model0))
}