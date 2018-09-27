library(lme4)
library(sjPlot)
try(setwd("~/Documents/Bristol/FTRAccounting/FTRAccountingStudy/analysis/"))
d = read.csv("../data/clean/data.csv",
             fileEncoding = "utf-8",
             encoding = 'utf-8')

countryMainLanguageFamily = read.csv("../data/raw/CountryMainLanguageToLanguageFamily.csv",stringsAsFactors = F)

d$mainLanguageFamily = countryMainLanguageFamily[match(as.character(d$loc),countryMainLanguageFamily$Country.Code),]$Family
d$CountryHasManyMainLanguages = countryMainLanguageFamily[match(as.character(d$loc),countryMainLanguageFamily$Country.Code),]$ManyLanguages=="Y"

d2 = d[!d$CountryHasManyMainLanguages,]


# m0 = lmer(AAM ~ strongftr +
#             invpro + 
#             CommonLaw +
#             pd + indiv + mas + ua + lto + indul + 
#             ggr (GDP growth) + 
#             SIZE + BTM + LEV + ROA +
#             MEET + LOSS +
#             (1 | fyear) +
#             (1 | indus),
#           data = d2)


d2$AAM2 = log(1+d2$AAM)

for(v in c("AAM2","pd",'indiv','mas','ua','lto','indul','ggr','SIZE',"BTM","LEV","ROA")){
  d2[,v] = scale(d2[,v])
}

mA0 = lmer(AAM2 ~ 1 +
             invpro + 
             pd + indiv + mas + ua + lto + indul + 
             ggr + 
             SIZE + BTM + LEV + ROA +
             MEET + LOSS +
             (1 | fyear) +
             (1 | indus),
           data = d2)

mA1 = update(mA0,~.+strongftr)
anova(mA0,mA1)

sjp.lmer(mA1,type="fe",p.kr = F)

mB0= lmer(AAM ~ 1 +
            invpro + 
            pd + indiv + mas + ua + lto + indul + 
            ggr + 
            SIZE + BTM + LEV + ROA +
            MEET + LOSS +
            (1 | fyear) +
            (1 | indus) +
            (1 | mainLanguageFamily),
          data = d2)

mB1= update(mB0, ~.+strongftr)
anova(mB0,mB1)

summary(mB1)


sjp.lmer(mB1,type="fe",p.kr = F)

plotA = sjp.lmer(mA1,type="fe",p.kr = F,geom.colors = c("black","black"))
plotB = sjp.lmer(mB1,type="fe",p.kr = F,geom.colors = c("red","red"))

x_APoints = 1:length(plotA$data$term)
x_BPoints = x_APoints + 0.5

pdf("../results/singleMembershipModel_AOnly.pdf",
    width=6,height=5)
par(mar=c(3,5,4,1))
plot(c(-0.5,0.5),c(1,15.5),type='n',yaxt='n',xlab="",ylab="")
abline(h=x_APoints-0.25,col="gray")
points(plotA$data$estimate,x_APoints,pch=16)
arrows(plotA$data$conf.low,x_APoints,plotA$data$conf.high,x_APoints, code=3,angle = 90,length = 0.1)
axis(2,at=x_APoints+0.25,labels = plotA$data$term,las=2)
abline(v=0)
legend(-0.4,19,xpd=T,legend=c("With language family controls","Without language family controls"),ncol=1,
       lty=1,col=2:1,text.col=2:1)
dev.off()


pdf("../results/singleMembershipModel.pdf",
    width=6,height=5)
par(mar=c(3,5,4,1))
plot(c(-0.5,0.5),c(1,15.5),type='n',yaxt='n',xlab="",ylab="")
abline(h=x_APoints-0.25,col="gray")
points(plotA$data$estimate,x_APoints,pch=16)
arrows(plotA$data$conf.low,x_APoints,plotA$data$conf.high,x_APoints, code=3,angle = 90,length = 0.1)
points(plotB$data$estimate,x_BPoints,col="red",pch=16)
arrows(plotB$data$conf.low,x_BPoints,plotB$data$conf.high,x_BPoints, code=3,angle = 90,length = 0.1,col="red")
axis(2,at=x_APoints+0.25,labels = plotA$data$term,las=2)
abline(v=0)
legend(-0.4,19,xpd=T,legend=c("With language family controls","Without language family controls"),ncol=1,
       lty=1,col=2:1,text.col=2:1)
dev.off()

mB2= update(mB1, ~.+(0+strongftr|mainLanguageFamily),
            data = d2[d2$mainLanguageFamily %in% c("Austronesian","Indo-European","Sino-Tibetan","Uralic"),])
sjp.lmer(mB2,type="rs.ri",vars="strongftr",show.legend = T)
