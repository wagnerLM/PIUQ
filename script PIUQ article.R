##### packages #####
library(lavaan)
library(qgraph)
library(psych)
library(semTools)
library(semPlot)

##### CFA, reliability, MIMIC, and figure #####
### data prep ###
piuq_sem<-read.csv("https://raw.githubusercontent.com/wagnerLM/PIUQ/main/PIUQ_data",sep = ";",dec = ",")
colnames(piuq_sem)<-c("Item1","Item2","Item3","Item4","Item5","Item6","Item7","Item8","Item9","Age","TSO","SPU","CESD")
View(piuq_sem)
### associations ###
boxplot(piuq_sem$TSO~piuq_sem$SPU)
kw_1<-kruskal.test(piuq_sem$TSO~as.factor(piuq_sem$SPU))
pairwise.wilcox.test(piuq_sem$TSO,as.factor(piuq_sem$SPU), p.adjust.method = "bonf",paired = FALSE)
### CFA ###
piuq_modela2<-'
general =~ Item1 + Item3 + Item9 + Item5 + Item7 + Item8 + Item2 + Item4 + Item6
neglig =~ Item1 + Item3 + Item9
obss =~ Item5 + Item7 + Item8
discon =~ Item2 + Item4 + Item6
general ~~ 0*neglig
general ~~ 0*obss
general ~~ 0*discon
'
piuq_fit1a2<-cfa(piuq_modela2,na.omit(piuq_sem),ordered = colnames(piuq_sem))
summary(piuq_fit1a2,fit.measures=T,standardized=T)
### reliability ###
reliability(piuq_fit1a2)
### MIMIC ###
piuq_modela2<-'
GF =~ Item1 + Item3 + Item9 + Item5 + Item7 + Item8 + Item2 + Item4 + Item6
Neg =~ Item1 + Item3 + Item9
Obs =~ Item5 + Item7 + Item8
CD =~ Item2 + Item4 + Item6
GF ~~ 0*Neg
GF ~~ 0*Obs
GF ~~ 0*CD
GF ~ Age + TSO + SPU + CESD
Neg ~ Age + TSO + SPU + CESD
Obs ~ Age + TSO + SPU + CESD
CD ~ Age + TSO + SPU + CESD
'
piuq_fit1a2<-cfa(piuq_modela2,piuq_sem,ordered = colnames(piuq_sem[,c(1:9,12)]),parameterization="theta")
summary(piuq_fit1a2,fit.measures=T,standardized=T,rsq=T)
### Figure ###
pdf("sempath2.pdf",9,7,pointsize = 12)
semPaths(piuq_fit1a2,
         what = "std",
         whatLabels="std",
         # style = "lisrel",
         #layout = "tree3",
         intercepts = F,
         residuals = F,
         thresholds = F,
         rotation = 2,
         sizeMan = 4,
         sizeLat = 5,
         title = F,
         reorder = T,
         color = "white",
         edge.color="black",
         #bifactor="general",
         equalizeManifests=F,
         fade=F,
         fixedStyle=c("black",1),
         freeStyle=c("black",1),
         #layoutSplit = TRUE,
         subScale=0.4,
         subScale2=0.4,
         subRes=6,
         edge.width=.1,
         curve=T,
         curvature =4,
         curveAdjacent="<->",
         cardinal=T,
         #equalizeManifests=T
         edge.label.cex=.5,
         nCharNodes=0)
dev.off()
##### Other models evaluated #####

piuq_model1<-'
neglig =~ Item1 + Item3 + Item9 
obss =~ Item5 + Item7 + Item8
discon =~ Item2 + Item4 + Item6
'
piuq_fit1<-cfa(piuq_model1,na.omit(piuq_sem),ordered = colnames(piuq_sem))
summary(piuq_fit1,fit.measures=T,standardized=T)

piuq_modela1<-'
negligdiscon =~ Item1 + Item3 + Item9 + Item2 + Item4 + Item6
obss =~ Item5 + Item7 + Item8
'
piuq_fit1a1<-cfa(piuq_modela1,na.omit(piuq_sem),ordered = colnames(piuq_sem))
summary(piuq_fit1a1,fit.measures=T,standardized=T)

piuq_modela2<-'
general =~ Item1 + Item3 + Item9 + Item5 + Item7 + Item8 + Item2 + Item4 + Item6
 
obss =~ Item5 + Item7 + Item8
discon =~ Item2 + Item4 + Item6
'
piuq_fit1a2<-cfa(piuq_modela2,na.omit(piuq_sem),ordered = colnames(piuq_sem),orthogonal=T)
summary(piuq_fit1a2,fit.measures=T,standardized=T)

piuq_modela3<-'
general =~ Item1 + Item3 + Item9 + Item5 + Item7 + Item8 + Item2 + Item4 + Item6
negligdiscon =~ Item1 + Item3 + Item9 + Item2 + Item4 + Item6
obss =~ Item5 + Item7 + Item8
general ~~ 0*negligdiscon
general ~~ 0*obss
'
piuq_fit1a3<-cfa(piuq_modela3,piuq_sem,ordered = colnames(piuq_sem))
summary(piuq_fit1a3,fit.measures=T,standardized=T)
