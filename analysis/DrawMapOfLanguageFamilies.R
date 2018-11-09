setwd("~/Documents/Bristol/FTRAccounting/FTRAccountingStudy/analysis/")

library(maps)
library(ggplot2)
library(sp)
library(rworldmap)
library(RColorBrewer)

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
  scale_color_manual(values=pcols)

setEPS()
postscript("../results/map.eps", width=8, height=4.2)
lmap
dev.off()

