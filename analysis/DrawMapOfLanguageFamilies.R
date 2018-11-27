try(setwd("~/Documents/Bristol/FTRAccounting/FTRAccountingStudy/analysis/"))

library(maps)
library(ggplot2)
library(sp)
library(rworldmap)
library(RColorBrewer)
#library(concaveman)

g = read.csv("../data/raw/glottolog_languoid.csv/languoid.csv", stringsAsFactors = F,fileEncoding = "UTF-8",encoding = "UTF-8")

g = g[!is.na(g$latitude),]
g = g[g$level=="language",]
g = g[g$status!="extinct",]
g = g[g$iso639P3code!="",]

families = c("aust1307","afro1255","atla1278","indo1319","sino1245","pama1250","turk1311","utoa1244",'tupi1275')
names(families) = c("Austronesian","Afro-Asiatic","Atlantic-Congo","Indo-European","Sino-Tibetan","Pama-Nyungan","Turkic","Uto-Aztecan",'Tupian')

g = g[g$family_id %in% families,]

g$Family = names(families)[match(g$family_id,families)]
g$longitude <- ifelse(g$longitude < -25, g$longitude + 360, g$longitude) 


mapWorld <- map_data('world', wrap=c(-25,335), ylim=c(-55,75))

pcols = c(brewer.pal(n = 8,name = "Set2"),"#FFFFFF")

lmap = ggplot() +
  geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group)) +
  geom_point(data = g, aes(x = longitude, y = latitude,colour=Family)) + 
  scale_color_manual(values=pcols)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

setEPS()
postscript("../results/map.eps", width=8, height=4.2)
lmap
dev.off()


ftrLangs = read.csv("../data/raw/CountryMainLanguageToLanguageFamily.csv",stringsAsFactors = F)
ftrLangs$Country.Name = gsub(" +$","",ftrLangs$Country.Name)
ftrLangs = ftrLangs[!duplicated(ftrLangs$Country.Name),]

lx = read.csv("../data/raw/langftr.csv",stringsAsFactors = F)
ftrLangs[ftrLangs$FTR=="",]$FTR = 
  c("Weak","Strong")[lx[match(ftrLangs[ftrLangs$FTR=="",]$Country.Code,
           lx$loc),]$strongftr+1]

cloc = read.csv("../data/raw/Country_List_ISO_3166_Codes_Latitude_Longitude.csv",stringsAsFactors = F)

ftrLangs$latitude = cloc[match(ftrLangs$Country.Code,cloc$Alpha.3.code),]$Latitude..average.
ftrLangs$longitude = cloc[match(ftrLangs$Country.Code,cloc$Alpha.3.code),]$Longitude..average.

ftrLangs[ftrLangs$Country.Name=="Canada",]$latitude = 60
ftrLangs[ftrLangs$Country.Name=="Canada",]$longitude = -110

ftrLangs$longitude <- ifelse(ftrLangs$longitude < -25, ftrLangs$longitude + 360, ftrLangs$longitude) 

ftrLangs$FTR2 = substr(ftrLangs$FTR,1,1)

lmap2 = ggplot() +
  geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group)) +
  geom_point(data = g, aes(x = longitude, y = latitude,colour=Family)) + 
  geom_text(data = ftrLangs, aes(x = longitude, y = latitude,label=FTR2),color="white") + 
  scale_color_manual(values=pcols) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
lmap2

setEPS()
postscript("../results/map2.eps", width=8, height=4.2)
lmap2
dev.off()


swcols = brewer.pal(n = 3,name = "Set2")[2:1]

lmap3 = ggplot() +
  geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group)) +
  #geom_point(data = g, aes(x = longitude, y = latitude,colour=Family)) + 
  geom_point(data = ftrLangs, aes(x = longitude, y = latitude,colour=FTR,shape=FTR),size=3) + 
  scale_color_manual(values=swcols) +
  scale_shape_manual(values=c(17,16))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
lmap3

setEPS()
postscript("../results/map3.eps", width=8, height=4.2)
lmap3
dev.off()