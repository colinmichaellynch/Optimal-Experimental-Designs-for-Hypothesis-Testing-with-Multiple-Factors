rm(list = ls())
setwd("~/Doug Simulation/Power and Optimality") 

dataFrame = read.csv("PowerSheet1.csv") 
dataFrame = subset(dataFrame, Factor.type == "Continuous")
dataFrame$A.Efficiency = as.numeric(dataFrame$A.Efficiency)
#dataFrame = subset(dataFrame, Power > .05)
#dataFrame = subset(dataFrame, Power < 1)
dataFrameLong = data.frame(Design = dataFrame$Design, Runs = dataFrame$Runs, Efficiency = c(dataFrame$D.Efficiency, dataFrame$G.Efficiency,dataFrame$A.Efficiency), EfficiencyType = c(rep("D", nrow(dataFrame)), rep("G", nrow(dataFrame)), rep("A", nrow(dataFrame))), Power = dataFrame$Power, CoefficientType = dataFrame$Coefficient, CoefficientLevel = dataFrame$Coefficient.Level, Setting = dataFrame$SampleSizeSetting)

dataFrameLong = subset(dataFrameLong, Power > 0.05)
dataFrameLong = subset(dataFrameLong, Power < 1)
dataFrameLong = subset(dataFrameLong,Setting == "Recommended" )

cor(dataFrame$D.Efficiency, dataFrame$G.Efficiency)
cor(dataFrame$A.Efficiency, dataFrame$G.Efficiency)
cor(dataFrame$A.Efficiency, dataFrame$D.Efficiency)

library(ggplot2)
library(FSA)
library(ggpubr)
library(Rmisc)
library(car)
library(plyr)

ggplot(dataFrameLong, aes(x = Efficiency, y = Power, color = EfficiencyType)) + geom_point(size = 2) +
  geom_smooth(method='loess', formula= y~x)

cor.test(dataFrame$Power, dataFrame$D.Efficiency, method = 'spearman')
cor.test(dataFrame$Power, dataFrame$G.Efficiency, method = 'spearman')
cor.test(dataFrame$Power, dataFrame$A.Efficiency, method = 'spearman')

mdlVIF = lm(Power~D.Efficiency+G.Efficiency+A.Efficiency, data = dataFrame)
vif(mdlVIF)

###

CoefficientTypeVec = unique(dataFrame$Coefficient)
RunNumberVec = unique(dataFrame$Runs)
factorNumberVec = unique(dataFrame$Factor.number)
meanD = c()
meanG = c()
meanA = c()
meanPower = c()
counter = 0

for(i in 1:length(CoefficientTypeVec)){
  CoefficientType = CoefficientTypeVec[i]
  for(j in 1:length(RunNumberVec)){
    RunNumber = RunNumberVec[j]
    for(k in 1:length(factorNumberVec)){
      factorNumber = factorNumberVec[k]
      counter = counter+1
      dataTemp = subset(dataFrame, Coefficient == CoefficientType & Runs == RunNumber & Factor.number==factorNumber)
      meanD[counter] = mean(dataTemp$D.Efficiency)
      meanG[counter] = mean(dataTemp$G.Efficiency)
      meanA[counter] = mean(dataTemp$A.Efficiency)
      meanPower[counter] = mean(dataTemp$Power)
    }
  }
}

plot(meanD, meanPower)
plot(meanG, meanPower)
plot(meanA, meanPower)

cor.test(meanPower, meanD, method = 'spearman')
cor.test(meanPower, meanG, method = 'spearman')
cor.test(meanPower, meanA, method = 'spearman')

###

dataFrameLongMax = aggregate(dataFrameLong$Power, by = list(dataFrameLong$Design), max)

letter = c()
for(i in 1:nrow(dataFrameLongMax)){
  if(dataFrameLongMax$Group.1[i]=="A Optimal"){
    letter[i] = "AB"
  } else if(dataFrameLongMax$Group.1[i]=="Alias Optimal"){
    letter[i] = "A"
  } else if(dataFrameLongMax$Group.1[i]=="BBD"){
    letter[i] = "AB"
  } else if(dataFrameLongMax$Group.1[i]=="CCD-O"){
    letter[i] = "B"
  } else if(dataFrameLongMax$Group.1[i]=="CCD-R"){
    letter[i] = "B"
  } else if(dataFrameLongMax$Group.1[i]=="D Optimal"){
    letter[i] = "A"
  } else {
    letter[i] = "AB"
  }
}

dataFrameLongMax$Letter = letter

ggplot(dataFrameLong, aes(x = Design, y = Power)) + geom_boxplot() +geom_text(data=dataFrameLongMax,aes(x=Group.1,y=0.02+x,label=Letter),vjust=0, size = 5) + ylim(0, 1.1) + theme_bw() + theme(text = element_text(size = 16))

kruskal.test(Power~Design, data = dataFrameLong)
pairwise.wilcox.test(dataFrameLong$Power, dataFrameLong$Design, p.adj = "none")

mdl = aov(Power~Design, data = dataFrameLong)
TukeyHSD(mdl, conf.level = 0.95)

summary(aov(Power~Design, data = dataFrameLong))
pairwise.wilcox.test(dataFrameLong$Power, dataFrameLong$Design, p.adj = "none")

dataPowerSum <- summarySE(dataFrameLong, measurevar="Power", groupvars=c("Design"))

ggplot(dataFrameLong, aes(x = Design, y = Efficiency, color = EfficiencyType)) + geom_boxplot()
summary(aov(Power~D.Efficiency, data = dataFrame))
summary(aov(Power~G.Efficiency, data = dataFrame))
summary(aov(Power~A.Efficiency, data = dataFrame))

###
library(qpcR)
dataFrame = subset(dataFrame, Power < 1.1)
mdl1 = lm(Power~Coefficient+Coefficient.Level+Runs + D.Efficiency, data=dataFrame) #ignore factor number, not significant
summary(mdl1)
PRESS(mdl1, verbose = TRUE) 

mdl2 = lm(dataFrame$Power~dataFrame$Coefficient+dataFrame$Coefficient.Level+dataFrame$Runs + dataFrame$G.Efficiency)
summary(mdl2)

mdl3 = lm(dataFrame$Power~dataFrame$Coefficient+dataFrame$Coefficient.Level+dataFrame$Runs + dataFrame$A.Efficiency)
summary(mdl3)

AIC(mdl1, mdl2, mdl3)
BIC(mdl1, mdl2, mdl3)
summary(anova(mdl1, mdl2, mdl3))
print(c("D", "G", "A"))
print(c(summary(mdl1)$adj.r.squared,summary(mdl2)$adj.r.squared ,summary(mdl3)$adj.r.squared))

plot(mdl3)

###

library(RColorBrewer)

dataSum = aggregate(list(dataFrame$Power, dataFrame$D.Efficiency, dataFrame$G.Efficiency, dataFrame$A.Efficiency), by = list(dataFrame$Design), mean)

names(dataSum) = c("Design", "Power", "DEff", "GEff", "AEff")

dataSum$PowerRank = rank(-dataSum$Power)
dataSum$DRank = rank(-dataSum$DEff)
dataSum$GRank = rank(-dataSum$GEff)
dataSum$ARank = rank(-dataSum$AEff)

cor.test(dataSum$DRank, dataSum$PowerRank, method = "spearman")
cor.test(dataSum$GRank, dataSum$PowerRank, method = "spearman")
cor.test(dataSum$ARank, dataSum$PowerRank, method = "spearman")

dataGraph = data.frame(Design = dataSum$Design, PowerRank = dataSum$PowerRank, EffRank = c(dataSum$DRank, dataSum$GRank, dataSum$ARank), RankType = c(rep("D", nrow(dataSum)), rep("G", nrow(dataSum)), rep("A", nrow(dataSum))))

p1 = ggplot(dataSum, aes(x = DRank, y = PowerRank, color = Design)) + geom_point(size = 4) + theme_bw() + theme(text = element_text(size = 16)) + scale_y_reverse(breaks = c(7,6,5,4,3,2,1)) + scale_x_reverse(breaks = c(7,6,5,4,3,2,1)) + xlab("D_eff Rank") + ylab("Power Rank") + xlab(bquote(D[Eff] ~ " Rank")) + coord_fixed() + scale_color_brewer(palette="Accent") + ggtitle("A)")

p2 = ggplot(dataSum, aes(x = GRank, y = PowerRank, color = Design)) + geom_point(size = 4) + theme_bw() + theme(text = element_text(size = 16)) + scale_y_reverse(breaks = c(7,6,5,4,3,2,1)) + scale_x_reverse(breaks = c(7,6,5,4,3,2,1)) + xlab("G_eff Rank") + ylab("Power Rank") + xlab(bquote(G[Eff] ~ " Rank")) + coord_fixed() + scale_color_brewer(palette="Accent") +ggtitle("B)")

p3 = ggplot(dataSum, aes(x = ARank, y = PowerRank, color = Design)) + geom_point(size = 4) + theme_bw() + theme(text = element_text(size = 16)) + scale_y_reverse(breaks = c(7,6,5,4,3,2,1)) + scale_x_reverse(breaks = c(7,6,5,4,3,2,1)) + xlab("A_eff Rank") + ylab("Power Rank") + xlab(bquote(A[Eff] ~ " Rank"))  + coord_fixed() + scale_color_brewer(palette="Accent") +ggtitle("C)")

ggarrange(p1, p2, p3, nrow = 1, ncol = 3, common.legend = TRUE, legend = "bottom")

p1 = ggplot(dataSum, aes(x = DRank, y = PowerRank, shape = Design)) + geom_point(size = 4) + theme_bw() + theme(text = element_text(size = 16)) + scale_y_reverse(breaks = c(7,6,5,4,3,2,1)) + scale_x_reverse(breaks = c(7,6,5,4,3,2,1)) + xlab("D_eff Rank") + ylab("Power Rank") + xlab(bquote(D[Eff] ~ " Rank")) + coord_fixed() + scale_color_brewer(palette="Accent") + ggtitle("A)") +
  scale_shape_manual(values=1:7)

p2 = ggplot(dataSum, aes(x = GRank, y = PowerRank, shape = Design)) + geom_point(size = 4) + theme_bw() + theme(text = element_text(size = 16)) + scale_y_reverse(breaks = c(7,6,5,4,3,2,1)) + scale_x_reverse(breaks = c(7,6,5,4,3,2,1)) + xlab("G_eff Rank") + ylab("Power Rank") + xlab(bquote(G[Eff] ~ " Rank")) + coord_fixed() + scale_color_brewer(palette="Accent") +ggtitle("B)") +
  scale_shape_manual(values=1:7)

p3 = ggplot(dataSum, aes(x = ARank, y = PowerRank, shape = Design)) + geom_point(size = 4) + theme_bw() + theme(text = element_text(size = 16)) + scale_y_reverse(breaks = c(7,6,5,4,3,2,1)) + scale_x_reverse(breaks = c(7,6,5,4,3,2,1)) + xlab("A_eff Rank") + ylab("Power Rank") + xlab(bquote(A[Eff] ~ " Rank"))  + coord_fixed() + scale_color_brewer(palette="Accent") +ggtitle("C)") +
  scale_shape_manual(values=1:7)

ggarrange(p1, p2, p3, nrow = 1, ncol = 3, common.legend = TRUE, legend = "bottom")

###

dataSum = aggregate(list(dataFrame$Power, dataFrame$D.Efficiency, dataFrame$G.Efficiency, dataFrame$A.Efficiency), by = list(dataFrame$Design, dataFrame$Coefficient), mean)

names(dataSum) = c("Design", "Coefficient", "Power", "DEff", "GEff", "AEff")

coefficientVec = unique(dataSum$Coefficient)
spearmanCorr = c()
spearmanP = c()
designLabel = c()
coefType = c()

for(i in 1:4){
  coefficientTemp = coefficientVec[i]
  dataTemp = subset(dataSum, Coefficient == coefficientTemp)
  
  test1 = cor.test(rank(-dataTemp$Power), rank(-dataTemp$DEff))
  test2 = cor.test(rank(-dataTemp$Power), rank(-dataTemp$GEff))
  test3 = cor.test(rank(-dataTemp$Power), rank(-dataTemp$AEff))
  designLabel = c(designLabel, "D", "G", "A")
  spearmanCorr =  c(spearmanCorr, test1$estimate, test2$estimate, test3$estimate)
  spearmanP =  c(spearmanP, test1$p.value, test2$p.value, test3$p.value)
  coefType = c(coefType, rep(coefficientTemp, 3))
  
}

dataGraph = data.frame(Correlation = spearmanCorr, PValue = spearmanP, EfficientyType = designLabel, Coefficient = coefType)

ggplot(dataGraph, aes(x = Coefficient, y = Correlation, color = EfficientyType)) + geom_point()

### PRESS
model_fit_stats <- function(linear.model) {
  r.sqr <- summary(linear.model)$r.squared
  adj.r.sqr <- summary(linear.model)$adj.r.squared
  pre.r.sqr <- pred_r_squared(linear.model)
  PRESS <- PRESS(linear.model)
  return.df <- data.frame(r.squared = r.sqr, adj.r.squared = adj.r.sqr, pred.r.squared = pre.r.sqr, press = PRESS)
  return(return.df)
}

pred_r_squared <- function(linear.model) {
  #' Use anova() to get the sum of squares for the linear model
  lm.anova <- anova(linear.model)
  #' Calculate the total sum of squares
  tss <- sum(lm.anova$'Sum Sq')
  # Calculate the predictive R^2
  pred.r.squared <- 1-PRESS(linear.model)/(tss)
  
  return(pred.r.squared)
}

PRESS <- function(linear.model) {
  #' calculate the predictive residuals
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  #' calculate the PRESS
  PRESS <- sum(pr^2)
  
  return(PRESS)
}

ldply(list(mdl1, mdl2, mdl3), model_fit_stats)


### First order

rm(list = ls())
library(ggplot2)
library(latex2exp)
setwd("~/Doug Simulation/Power and Optimality") 
dataFrame = read.csv("PowerSheet2.csv")

dataFrame$Coefficient.Level.Category = factor(dataFrame$Coefficient.Level)

levels(dataFrame$Coefficient.Level.Category) <- c( parse(text=TeX('$\\beta_i$ = 0')), TeX("$\\beta_i$ = 0.25"), TeX("$\\beta_i$ = 0.5"), TeX("$\\beta_i$ = 0.75"), TeX("$\\beta_i$ = 1"), TeX("$\\beta_i$ = 1.25"), TeX("$\\beta_i$ = 1.5"),  TeX("$\\beta_i$ = 1.75"), TeX("$\\beta_i$ = 2") )

ggplot(dataFrame, aes(x = Runs, y = Power, color = Design)) + geom_point() +
  geom_smooth(method='loess', formula= y~x) + 
  facet_wrap(~ Coefficient.Level.Category, labeller=label_parsed) + theme_bw() + theme(text = element_text(size = 16)) + scale_color_brewer(palette="Accent")

p1 = ggplot(dataFrame, aes(x = Runs, y = Factor.Number, color = Design)) + geom_point(size = 3) +
  geom_smooth(method='loess', formula= y~x, se = FALSE)+ theme_bw() + theme(text = element_text(size = 16)) + scale_color_brewer(palette="Accent") + ylab("Factor Number") + ggtitle("B)")

p2 = ggplot(dataFrame, aes(x = Runs, y = Power, color = Design)) + geom_point() +
  geom_smooth(method='loess', formula= y~x) + theme_bw() + theme(text = element_text(size = 16)) + scale_color_brewer(palette="Accent") + ggtitle("A)")

ggarrange(p2, p1, nrow = 1, ncol = 2, common.legend = TRUE, legend = "bottom")

p1 = ggplot(dataFrame, aes(x = Runs, y = Factor.Number, color = Design, shape = Design)) + geom_point(size = 3) +
  geom_smooth(method='loess', formula= y~x, se = FALSE, size=1.5)+ theme_bw() + theme(text = element_text(size = 16)) + scale_color_brewer(palette="Accent") + ylab("Factor Number") + ggtitle("B)")

p2 = ggplot(dataFrame, aes(x = Runs, y = Power, color = Design, shape = Design)) + geom_point(size = 3) +
  geom_smooth(method='loess', formula= y~x, size=1.5) + theme_bw() + theme(text = element_text(size = 16)) + scale_color_brewer(palette="Accent") + ggtitle("A)")

ggarrange(p2, p1, nrow = 1, ncol = 2, common.legend = TRUE, legend = "bottom")


