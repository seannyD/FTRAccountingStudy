

set.seed(1212)
a0 = 0.1
b0 = 0.2
c0 = 0.4


a1 = a0 + rnorm(1)
a2 = a1 + rnorm(1)

b1a = a0 + rnorm(1)
b1b = a0 + rnorm(1)

b1a2a = b1a + rnorm(1)
b1a2b = b1a + rnorm(1)

b1b2a = b1b + rnorm(1)
b1b2b = b1b + rnorm(1)

c1a = c0 + rnorm(1)
c1b = c0 + rnorm(1)
c2a = c1a + rnorm(1)
c2b = c1b + rnorm(1)



library(ape)
library(phylolm)

set.seed(1212)
tA = rtree(4,br = rep(1,6))
eA = rTraitCont(tA, root.value = 0.5, , sigma = 0.1, model=c("BM"),ancestor = T)
set.seed(3223)
numBTips = 7
tB = rtree(numBTips,br = 1)
eB = rTraitCont(tB, root.value = 0, , sigma = 0.1, model=c("BM"),ancestor = T)
set.seed(12)
tC = rtree(3,br=1)
eC = rTraitCont(tC, root.value = -0.5, sigma = 0.1, model=c("BM"),ancestor = T)

dx = data.frame(
  E = c(eA["Node1"],eA[c("Node2","Node3")],eA[1:4],
        eB["Node1"],eB[c("Node2","Node3","Node4")],eB[1:numBTips],
        eC["Node1"],eC["Node2"],eC[c("t1")]),
  gen = c(0,1,1,2,2,2,2,
          0,1,1,1,rep(2,numBTips),
          0,1,2),
  L = c(rep("Type 1",7),
        rep("Type 2",numBTips+4),
        rep("Type 1",3)),
  Tree = c(rep("A",7),
           rep("B",numBTips+4),
           rep("C",3))
)

plot(compute.brlen(tA),direction = 'd',root.edge = 0)
plot(compute.brlen(tB))
plot(compute.brlen(tC))

lims = c(-1,0.5)

pdf("../results/misc/Brownian1_TreeA.pdf")
contMap(compute.brlen(tA,power = 0.8),eA[paste0("t",1:4)],outline = F,direction="downwards",lims=lims)
nodelabels(text=NA,pch=16,cex=4)
tiplabels(text=NA,pch=16,cex=4)
dev.off()

pdf("../results/misc/Brownian1_TreeB.pdf")
contMap(compute.brlen(tB,power = 0.8),eB[paste0("t",1:7)],outline = F,direction="downwards",lims=lims)
nodelabels(text=NA,pch=15,cex=4)
tiplabels(text=NA,pch=15,cex=4)
dev.off()

pdf("../results/misc/Brownian1_TreeC.pdf")
contMap(compute.brlen(tC,power = 0.8),eC[paste0("t",1:3)],outline = F,direction="downwards",lims=lims)
nodelabels(text=NA,pch=17,cex=4)
tiplabels(text=NA,pch=17,cex=4)
dev.off()

pdf("../results/misc/Brownian1_values.pdf",width=2.5,height=4)
ggplot(dx, aes(x=L,y=E)) +
  geom_boxplot(width=0.2,position = position_nudge(x=0.2),outlier.shape = NA) +
  geom_jitter(aes(shape=Tree),width=0.01,height=0) +
  facet_grid(rows = vars(gen)) +
  theme_bw() + theme(strip.background = element_blank(),
                     strip.text = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     legend.position = "none") +
  xlab("Language type")
dev.off()

