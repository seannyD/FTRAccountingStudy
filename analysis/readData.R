library(haven)
library(brms)

setwd("~/Documents/Bristol/FTRAccounting/FTRAccountingStudy/analysis/")


# Curcd: ISO currency code
# Fic: Current ISO country code – country of incorporation
# Loc: Current ISO country code – headquarters
# Country: Country ID from IBES (Thompson Reuters). This database includes financial analysts forecasts information. I attach the table from IBES manual that details the country codes used.

d = read_dta("../data/raw/ftrdataset July 5 2017.dta", encoding = 'utf-8')

l = read.csv("../data/raw/langftr.csv",
             stringsAsFactors = F,
             encoding = "UTF-8",
             fileEncoding = 'UTF-8')

l$genus = gsub(" +$","",l$genus)
l$genus2 = gsub(" +$","",l$genus2)
l$genus3 = gsub(" +$","",l$genus3)

l$genus[l$genus==""] = " None"
l$genus2[l$genus2==""] = " None"
l$genus3[l$genus3==""] = " None"

allGenus = unique(c(l$genus,l$genus2,l$genus3))
allGenus = sort(allGenus)
allGenus.code = 0:length(allGenus)
names(allGenus.code)= allGenus


l$G1 = allGenus.code[l$genus]
l$G2 = allGenus.code[l$genus2]
l$G3 = allGenus.code[l$genus3]
l$G1[is.na(l$G1)] = 0
l$G2[is.na(l$G1)] = 0
l$G3[is.na(l$G1)] = 0

# weights (balanced, but zero if genus is ' None')
l$G1.p = 1/apply(l[,c("G1","G2","G3")],1,function(X){sum(X>0)})
l$G2.p = 1/apply(l[,c("G1","G2","G3")],1,function(X){sum(X>0)})
l$G3.p = 1/apply(l[,c("G1","G2","G3")],1,function(X){sum(X>0)})

d$G1 = l[match(d$loc,l$loc),]$G1
d$G2 = l[match(d$loc,l$loc),]$G2
d$G3 = l[match(d$loc,l$loc),]$G3
d$G1.p = l[match(d$loc,l$loc),]$G1.p
d$G2.p = l[match(d$loc,l$loc),]$G2.p
d$G3.p = l[match(d$loc,l$loc),]$G3.p


write.csv(d,"../data/clean/data.csv", 
          row.names = F,
          fileEncoding = "utf-8")

# Make minimal file for cluster run
d = d[,c("AAM","pcftr","G1","G2","G3","G1.p","G2.p","G3.p")]

save(d,file = "../data/clean/data_min.Rdat")

#####

#pcftr = read.csv("../data/raw/Chen2013/OnlineFTRRatios_Chen_2013.csv",stringsAsFactors = F)


# pcftr2 = 
#   data.frame(pcftr = tapply(d$pcftr,d$countryname,head,n=1),
#              country = tapply(d$countryname,d$countryname,head,n=1))
# 
# pcftr2$chen13 = c(Brazil=85.0, 
#                   China=0,
#                   Denmark = 10.0, 
#                   "United Kingdom"=88.1,
#                   "United States" =76.9,
#                   Russia = 72.2,
#                   Spain = 71.6,
#                   Poland = 28.2)[pcftr2$country]
# 
# pcftr2$chen13 = c(Brazil=0, 
#                   China=0,
#                   Denmark = 12.5, 
#                   "United Kingdom"=92.9,
#                   "United States" =87.5,
#                   Russia = 80.8,
#                   Spain = 74.1,
#                   Poland = 34.4)[pcftr2$country]
# 
# 
# plot(pcftr2$pcftr, pcftr2$chen13)