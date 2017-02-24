onsetofEH.R -- extra code

# Analysis for "onset of escape-hatching"
# title: Developmental onset of the escape-hatching response in red-eyed treefrogs depends on cue type
# May 2016
# Julie Jung

ls()
rm(list=ls())
ls()
setwd('/Users/juliejung/Desktop/2cues m.s.') 
getwd()         

# read.csv(file="my.csv.filename")
onset.df<-read.csv(file="ontogeny.csv")

###################### Q1 ##########################
## Does latency to hatch after stimulus begins differ between hypoxia and mechanically cued embryos?
###################### Q1 ##########################

# tactile <- subset(onset.df, Stimulus == "T", na.rm=T, select=c(Clutch, AgeBlock, Individual, Response, AverageR2, Average.Amp, DiffRandL, HatchTime, HatchAge, HsinceH, TtoH))
hypoxic <- subset(onset.df, Stimulus == "H", na.rm=T)
tactile <- subset(onset.df, Stimulus == "T", na.rm=T)

hist(hypoxic$TtoH) #poisson or negative binomial
hist(tactile$TtoH) #geometric or negative binomial
hist(onset.df$TtoH) #geometric?

############# visual evidence that yes, very different ##############
mean(hypoxic$TtoH, na.rm=T)
mean(tactile$TtoH, na.rm=T)
sd(hypoxic$TtoH, na.rm=T)
sd(tactile$TtoH, na.rm=T)
min(tactile$TtoH, na.rm=T)
max(tactile$TtoH, na.rm=T)
sd(tactile$TtoH, na.rm=T)
summary(hypoxic$TtoH, na.rm=T)
summary(tactile$TtoH, na.rm=T)

boxplot(hypoxic$TtoH, tactile$TtoH, xlab="Stimulus", ylab="Latency to hatch (min)")
axis(1, at=1:2, labels=c("hypoxic", "tactile"))
#how different are these? significantly?
############# visual evidence that yes, very different ##############


############# start stri strategy (not as good) ###########################
glm1<-glm(TtoH~Stimulus, family="poisson", data=onset.df)
plot(glm1)
summary(glm1) #P=0.002

glm2<-glm(TtoH~Stimulus, family="binomial", data=onset.df)
plot(glm2)
summary(glm2) #P=0.0005
############# end stri strategy (not as good) ###########################


###################### ANS 1 ##########################
# Ho: Midean change in TtoH is 0
# two.sided test
wilcox.test(hypoxic$TtoH, tactile$TtoH, mu = 0, alt="two.sided", paired = FALSE, conf.int=T, conf.level=0.99)
##nonparametric -->
##tests median <http://www.r-tutor.com/elementary-statistics/non-parametric-methods/wilcoxon-signed-rank-test>
##P<2.2e-16 
##yes, very significantly different
###################### ANS 1 ##########################


c254t <- subset(tactile, Clutch == "254", select=c(Clutch, Stimulus, AgeBlock, HatchAge, Response, NumH, TtoHhours, TadLength, SUM.of.trait.values))           
c254tF <- c254t[1:2,]
c254tL <- c254t[7:8,]
mean(c254tF$TtoHhours)
mean(c254tL$TtoHhours)
mean(c254tF$AgeBlock)
mean(c254tL$AgeBlock)
mean(c254tF$HatchAge)
mean(c254tL$HatchAge)
mean(c254tF$TadLength)
mean(c254tL$TadLength)
mean(c254tF$SUM.of.trait.values)
mean(c254tL$SUM.of.trait.values)
c255t <- subset(tactile, Clutch == "255", select=c(Clutch, AgeBlock, HatchAge, Response, NumH, TtoHhours, TadLength, SUM.of.trait.values))
c255tF <- c255t[3:4,]
c255tL <- c255t[7:8,]
mean(c255tF$TtoHhours)
mean(c255tL$TtoHhours)
mean(c255tF$AgeBlock)
mean(c255tL$AgeBlock)
mean(c255tF$HatchAge)
mean(c255tL$HatchAge)
mean(c255tF$TadLength)
mean(c255tL$TadLength)
mean(c255tF$SUM.of.trait.values)
mean(c255tL$SUM.of.trait.values)
c256t <- subset(tactile, Clutch == "256", select=c(Clutch, AgeBlock, HatchAge, Response, NumH, TtoHhours, TadLength, SUM.of.trait.values))
c256tF <- c256t[5:6,]
c256tL <- c256t[7:8,]
mean(c256tF$TtoHhours)
mean(c256tL$TtoHhours)
mean(c256tF$AgeBlock)
mean(c256tL$AgeBlock)
mean(c256tF$HatchAge)
mean(c256tL$HatchAge)
mean(c256tF$TadLength)
mean(c256tL$TadLength)
mean(c256tF$SUM.of.trait.values)
mean(c256tL$SUM.of.trait.values)
c257t <- subset(tactile, Clutch == "257", select=c(Clutch, AgeBlock, HatchAge, Response, NumH, TtoHhours, TadLength, SUM.of.trait.values))
c257tF <- c257t[1:2,]
c257tL <- c257t[5:6,]
mean(c257tF$TtoHhours)
mean(c257tL$TtoHhours)
mean(c257tF$AgeBlock)
mean(c257tL$AgeBlock)
mean(c257tF$HatchAge)
mean(c257tL$HatchAge)
mean(c257tF$TadLength)
mean(c257tL$TadLength)
mean(c257tF$SUM.of.trait.values)
mean(c257tL$SUM.of.trait.values)
c258t <- subset(tactile, Clutch == "258", select=c(Clutch, AgeBlock, HatchAge, Response, NumH, TtoHhours, TadLength, SUM.of.trait.values))
c258tF <- c258t[5:6,]
c258tL <- c258t[9:10,]
mean(c258tF$TtoHhours)
mean(c258tL$TtoHhours)
mean(c258tF$AgeBlock)
mean(c258tL$AgeBlock)
mean(c258tF$HatchAge)
mean(c258tL$HatchAge)
mean(c258tF$TadLength)
mean(c258tL$TadLength)
mean(c258tF$SUM.of.trait.values)
mean(c258tL$SUM.of.trait.values)
c259t <- subset(tactile, Clutch == "259", select=c(Clutch, AgeBlock, HatchAge, Response, NumH, TtoHhours, TadLength, SUM.of.trait.values))
c259tF <- c259t[1:2,]
c259tL <- c259t[7:8,]
mean(c259tF$TtoHhours)
mean(c259tL$TtoHhours)
mean(c259tF$AgeBlock)
mean(c259tL$AgeBlock)
mean(c259tF$HatchAge)
mean(c259tL$HatchAge)
mean(c259tF$TadLength)
mean(c259tL$TadLength)
mean(c259tF$SUM.of.trait.values)
mean(c259tL$SUM.of.trait.values)
c262t <- subset(tactile, Clutch == "262", select=c(Clutch, AgeBlock, HatchAge, Response, NumH, TtoHhours, TadLength, SUM.of.trait.values))
c262tF <- c262t[7:8,]
c262tL <- c262t[17:18,]
mean(c262tF$TtoHhours)
mean(c262tL$TtoHhours)
mean(c262tF$AgeBlock)
mean(c262tL$AgeBlock)
mean(c262tF$HatchAge)
mean(c262tL$HatchAge)
mean(c262tF$TadLength)
mean(c262tL$TadLength)
mean(c262tF$SUM.of.trait.values)
mean(c262tL$SUM.of.trait.values)
c263t <- subset(tactile, Clutch == "263", select=c(Clutch, AgeBlock, HatchAge, Response, NumH, TtoHhours, TadLength, SUM.of.trait.values))
c263tF <- c263t[5:6,]
c263tL <- c263t[11:12,]
mean(c263tF$TtoHhours)
mean(c263tL$TtoHhours)
mean(c263tF$AgeBlock)
mean(c263tL$AgeBlock)
mean(c263tF$HatchAge)
mean(c263tL$HatchAge)
mean(c263tF$TadLength)
mean(c263tL$TadLength)
mean(c263tF$SUM.of.trait.values)
mean(c263tL$SUM.of.trait.values)
c264t <- subset(tactile, Clutch == "264", select=c(Clutch, AgeBlock, HatchAge, Response, NumH, TtoHhours, TadLength, SUM.of.trait.values))
c264tF <- c264t[3:4,]
c264tL <- c264t[13:14,]
mean(c264tF$TtoHhours)
mean(c264tL$TtoHhours)
mean(c264tF$AgeBlock)
mean(c264tL$AgeBlock)
mean(c264tF$HatchAge)
mean(c264tL$HatchAge)
mean(c264tF$TadLength)
mean(c264tL$TadLength)
mean(c264tF$SUM.of.trait.values)
mean(c264tL$SUM.of.trait.values)
c265t <- subset(tactile, Clutch == "265", select=c(Clutch, AgeBlock, HatchAge, Response, NumH, TtoHhours, TadLength, SUM.of.trait.values))
c265tF <- c265t[3:4,]
c265tL <- c265t[7:8,]
mean(c265tF$TtoHhours)
mean(c265tL$TtoHhours)
mean(c265tF$AgeBlock)
mean(c265tL$AgeBlock)
mean(c265tF$HatchAge)
mean(c265tL$HatchAge)
mean(c265tF$TadLength)
mean(c265tL$TadLength)
mean(c265tF$SUM.of.trait.values)
mean(c265tL$SUM.of.trait.values)
c266t <- subset(tactile, Clutch == "266", select=c(Clutch, AgeBlock, HatchAge, Response, NumH, TtoHhours, TadLength, SUM.of.trait.values))
c266tF <- c266t[5:6,]
c266tL <- c266t[11:12,]
mean(c266tF$TtoHhours)
mean(c266tL$TtoHhours)
mean(c266tF$AgeBlock)
mean(c266tL$AgeBlock)
mean(c266tF$HatchAge)
mean(c266tL$HatchAge)
mean(c266tF$TadLength)
mean(c266tL$TadLength)
mean(c266tF$SUM.of.trait.values)
mean(c266tL$SUM.of.trait.values)

c254h <- subset(hypoxic, Clutch == "254", select=c(Clutch, Stimulus, AgeBlock, HatchAge, Response, NumH, TtoHhours, TadLength, SUM.of.trait.values))           
c254hF <- c254h[1:2,]
c254hL <- c254h[5:6,]
mean(c254hF$TtoHhours)
mean(c254hL$TtoHhours)
mean(c254hF$AgeBlock)
mean(c254hL$AgeBlock)
mean(c254hF$HatchAge)
mean(c254hL$HatchAge)
mean(c254hF$TadLength)
mean(c254hL$TadLength)
mean(c254hF$SUM.of.trait.values)
mean(c254hL$SUM.of.trait.values)

c255h <- subset(hypoxic, Clutch == "255", select=c(Clutch, AgeBlock, HatchAge, Response, NumH, TtoHhours, TadLength, SUM.of.trait.values))
c255hF <- c255h[3:4,]
c255hL <- c255h[9:10,]
mean(c255hF$TtoHhours)
mean(c255hL$TtoHhours)
mean(c255hF$AgeBlock)
mean(c255hL$AgeBlock)
mean(c255hF$HatchAge)
mean(c255hL$HatchAge)
mean(c255hF$TadLength)
mean(c255hL$TadLength)
mean(c255hF$SUM.of.trait.values)
mean(c255hL$SUM.of.trait.values)

c256h <- subset(hypoxic, Clutch == "256", select=c(Clutch, AgeBlock, HatchAge, Response, NumH, TtoHhours, TadLength, SUM.of.trait.values))
c256hF <- c256h[1:2,]
c256hL <- c256h[7:8,]
mean(c256hF$TtoHhours)
mean(c256hL$TtoHhours)
mean(c256hF$AgeBlock)
mean(c256hL$AgeBlock)
mean(c256hF$HatchAge)
mean(c256hL$HatchAge)
mean(c256hF$TadLength)
mean(c256hL$TadLength)
mean(c256hF$SUM.of.trait.values)
mean(c256hL$SUM.of.trait.values)

c257h <- subset(hypoxic, Clutch == "257", select=c(Clutch, AgeBlock, HatchAge, Response, NumH, TtoHhours, TadLength, SUM.of.trait.values))
c257hF <- c257h[7:8,]
c257hL <- c257h[11:12,]
mean(c257hF$TtoHhours)
mean(c257hL$TtoHhours)
mean(c257hF$AgeBlock)
mean(c257hL$AgeBlock)
mean(c257hF$HatchAge)
mean(c257hL$HatchAge)
mean(c257hF$TadLength)
mean(c257hL$TadLength)
mean(c257hF$SUM.of.trait.values)
mean(c257hL$SUM.of.trait.values)

c258h <- subset(hypoxic, Clutch == "258", select=c(Clutch, AgeBlock, HatchAge, Response, NumH, TtoHhours, TadLength, SUM.of.trait.values))
c258hF <- c258h[5:6,]
c258hL <- c258h[9:10,]
mean(c258hF$TtoHhours)
mean(c258hL$TtoHhours)
mean(c258hF$AgeBlock)
mean(c258hL$AgeBlock)
mean(c258hF$HatchAge)
mean(c258hL$HatchAge)
mean(c258hF$TadLength)
mean(c258hL$TadLength)
mean(c258hF$SUM.of.trait.values)
mean(c258hL$SUM.of.trait.values)

c259h <- subset(hypoxic, Clutch == "259", select=c(Clutch, AgeBlock, HatchAge, Response, NumH, TtoHhours, TadLength, SUM.of.trait.values))
c259hF <- c259h[5:6,]
c259hL <- c259h[9:10,]
mean(c259hF$TtoHhours)
mean(c259hL$TtoHhours)
mean(c259hF$AgeBlock)
mean(c259hL$AgeBlock)
mean(c259hF$HatchAge)
mean(c259hL$HatchAge)
mean(c259hF$TadLength)
mean(c259hL$TadLength)
mean(c259hF$SUM.of.trait.values)
mean(c259hL$SUM.of.trait.values)

c262h <- subset(hypoxic, Clutch == "262", select=c(Clutch, AgeBlock, HatchAge, Response, NumH, TtoHhours, TadLength, SUM.of.trait.values))
c262hF <- c262h[5:6,]
c262hL <- c262h[9:10,]
mean(c262hF$TtoHhours)
mean(c262hL$TtoHhours)
mean(c262hF$AgeBlock)
mean(c262hL$AgeBlock)
mean(c262hF$HatchAge)
mean(c262hL$HatchAge)
mean(c262hF$TadLength)
mean(c262hL$TadLength)
mean(c262hF$SUM.of.trait.values)
mean(c262hL$SUM.of.trait.values)

c263h <- subset(hypoxic, Clutch == "263", select=c(Clutch, AgeBlock, HatchAge, Response, NumH, TtoHhours, TadLength, SUM.of.trait.values))
c263hF <- c263h[3:4,]
c263hL <- c263h[11:12,]
mean(c263hF$TtoHhours)
mean(c263hL$TtoHhours)
mean(c263hF$AgeBlock)
mean(c263hL$AgeBlock)
mean(c263hF$HatchAge)
mean(c263hL$HatchAge)
mean(c263hF$TadLength)
mean(c263hL$TadLength)
mean(c263hF$SUM.of.trait.values)
mean(c263hL$SUM.of.trait.values)

c264h <- subset(hypoxic, Clutch == "264", select=c(Clutch, AgeBlock, HatchAge, Response, NumH, TtoHhours, TadLength, SUM.of.trait.values))
c264hF <- c264h[1:2,]
c264hL <- c264h[9:10,]
mean(c264hF$TtoHhours)
mean(c264hL$TtoHhours)
mean(c264hF$AgeBlock)
mean(c264hL$AgeBlock)
mean(c264hF$HatchAge)
mean(c264hL$HatchAge)
mean(c264hF$TadLength)
mean(c264hL$TadLength)
mean(c264hF$SUM.of.trait.values)
mean(c264hL$SUM.of.trait.values)

c265h <- subset(hypoxic, Clutch == "265", select=c(Clutch, AgeBlock, HatchAge, Response, NumH, TtoHhours, TadLength, SUM.of.trait.values))
c265hF <- c265h[7:8,]
c265hL <- c265h[9:10,]
mean(c265hF$TtoHhours)
mean(c265hL$TtoHhours)
mean(c265hF$AgeBlock)
mean(c265hL$AgeBlock)
mean(c265hF$HatchAge)
mean(c265hL$HatchAge)
mean(c265hF$TadLength)
mean(c265hL$TadLength)
mean(c265hF$SUM.of.trait.values)
mean(c265hL$SUM.of.trait.values)

c266h <- subset(hypoxic, Clutch == "266", select=c(Clutch, AgeBlock, HatchAge, Response, NumH, TtoHhours, TadLength, SUM.of.trait.values))
c266hF <- c266h[7:8,]
c266hL <- c266h[9:10,]
mean(c266hF$TtoHhours)
mean(c266hL$TtoHhours)
mean(c266hF$AgeBlock)
mean(c266hL$AgeBlock)
mean(c266hF$HatchAge)
mean(c266hL$HatchAge)
mean(c266hF$TadLength)
mean(c266hL$TadLength)
mean(c266hF$SUM.of.trait.values)
mean(c266hL$SUM.of.trait.values)

#########CLUTCH MEANS of latency to hatch################

means <- matrix(c(mean(c254t$TtoH, na.rm=T), 
                  mean(c255t$TtoH, na.rm=T),
                  mean(c256t$TtoH, na.rm=T),
                  mean(c257t$TtoH, na.rm=T),
                  mean(c258t$TtoH, na.rm=T),
                  mean(c259t$TtoH, na.rm=T),
                  mean(c262t$TtoH, na.rm=T),
                  mean(c263t$TtoH, na.rm=T),
                  mean(c264t$TtoH, na.rm=T),
                  mean(c265t$TtoH, na.rm=T),
                  mean(c266t$TtoH, na.rm=T),
                  
                  mean(c254h$TtoH, na.rm=T),
                  mean(c255h$TtoH, na.rm=T),
                  mean(c256h$TtoH, na.rm=T),
                  mean(c257h$TtoH, na.rm=T),
                  mean(c258h$TtoH, na.rm=T),
                  mean(c259h$TtoH, na.rm=T),
                  mean(c262h$TtoH, na.rm=T),
                  mean(c263h$TtoH, na.rm=T),
                  mean(c264h$TtoH, na.rm=T),
                  mean(c265h$TtoH, na.rm=T),
                  mean(c265h$TtoH, na.rm=T)), ncol=2, byrow=FALSE)

colnames(means) <- c("tactile", "hypoxia")
rownames(means) <- c("c254", "c255", "c256", "c257", "c258", "c259", "c262", "c263", "c264", "c265", "c266")
means <- as.table(means)
means[,1]

###### TO SHOW KAREN 6/3/16

wilcox.test(means[,1], means[,2], mu = 0, alt="two.sided", paired = TRUE, conf.int=T, conf.level=0.99)


ramp <- matrix(c(9,6,3,6,6,9,15,9,15,6,9,6,9,9,6,6,6,6,12,12,3,3), ncol=2, byrow=FALSE)
colnames(ramp) <- c("tactile", "hypoxia")
rownames(ramp) <- c("c254", "c255", "c256", "c257", "c258", "c259", "c262", "c263", "c264", "c265", "c266")
ramp <- as.dataframe(ramp)
wilcox.test(ramp[,1], ramp[,2], mu = 0, alt="two.sided", paired = TRUE, conf.int=T, conf.level=0.99)



###################### Q 3 ##########################
## When hatching starts, are the ones that hatch 1st developmentally ahead of their sibs that don't?
## i.e. is 
###################### Q 3 ##########################

# read.csv(file="my.csv.filename")
devstages.df<-read.csv(file="devstages.csv")

noH <- subset(devstages.df, NumH == 0, na.rm=T)
oneH <- subset(devstages.df, NumH == 1, na.rm=T)
twoH <- subset(devstages.df, NumH == 2, na.rm=T)

boxplot(noH$MeanLength, oneH$MeanLength, twoH$MeanLength, xlab="# hatched (out of 2)", ylab="Mean Length (mm)")
axis(1, at=1:3, labels=0:2)

meanleng <- aov(MeanLength~NumH, data=devstages.df)
library(agricolae)
results<-HSD.test(meanleng, "NumH", group=TRUE)
results
# all significantly different (using Tukey test) 

boxplot(noH$Stage, oneH$Stage, twoH$Stage, xlab="# hatched (out of 2)", ylab="Developmental Stage")
axis(1, at=1:3, labels=0:2)

develstage <- aov(Stage~NumH, data=devstages.df)
library(agricolae)
results2<-HSD.test(develstage, "NumH", group=TRUE)
results2
## 0 hatched: a, 1 hatched: b, 2 hatched: b

##################################
####Show karen 6/4/16

# of the ones that hatch first ("oneH") are the ones that hatched developmentally ahead of their sibs that didn't hatch?
firstHt <- subset(tactile, NumH ==1, na.rm=T)
HfirstHt <- subset(firstHt, Response == "Hatched")
NHfirstHt <- subset(firstHt, Response == "Not Hatched")
boxplot(HfirstHt$SUM.of.trait.values, NHfirstHt$SUM.of.trait.values, xlab="Response", ylab="Developmental Stage")
axis(1, at=1:2, labels=c("Hatched", "Not Hatched"))
wilcox.test(HfirstHt$SUM.of.trait.values, NHfirstHt$SUM.of.trait.values, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
##Of the ones that hatched first (1 of 2 hatched), the ones that hatched are not significantly ahead of their sibs that didn’t hatch (P=0.6448). 


firstHh <- subset(hypoxic, NumH ==1, na.rm=T)
HfirstHh <- subset(firstHh, Response == "Hatched")
NHfirstHh <- subset(firstHh, Response == "Not Hatched")
boxplot(HfirstHh$SUM.of.trait.values, NHfirstHh$SUM.of.trait.values, xlab="Response", ylab="Developmental Stage")
axis(1, at=1:2, labels=c("Hatched", "Not Hatched"))
wilcox.test(HfirstHh$SUM.of.trait.values, NHfirstHh$SUM.of.trait.values, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)


##Of the ones that hatched first (1 of 2 hatched), the ones that hatched are not significantly ahead of their sibs that didn’t hatch (P=0.6448). 


firstHt <- subset(tactile, NumH ==1, na.rm=T)
#firstHt$TadLength[firstHt$TadLength==8.852] <- NA  #outlier
#firstHt$TadLength[firstHt$TadLength==7.942] <- NA
HfirstHt <- subset(firstHt, Response == "Hatched")
NHfirstHt <- subset(firstHt, Response == "Not Hatched")
boxplot(HfirstHt$TadLength, NHfirstHt$TadLength, xlab="Response", ylab="Tad Length")
axis(1, at=1:2, labels=c("Hatched", "Not Hatched"))
#wilcox.test(HfirstHt$TadLength, NHfirstHt$TadLength, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
t.test(HfirstHt$TadLength, NHfirstHt$TadLength,paired=TRUE)
##Of the ones that hatched first (1 of 2 hatched), the ones that hatched are not significantly ahead of their sibs that didn’t hatch (P=0.6448). 
plot(HfirstHt$TadLength,NHfirstHt$TadLength)



firstHh <- subset(hypoxic, NumH ==1, na.rm=T)
HfirstHh <- subset(firstHh, Response == "Hatched")
NHfirstHh <- subset(firstHh, Response == "Not Hatched")
boxplot(HfirstHh$TadLength, NHfirstHh$TadLength, xlab="Response", ylab="Tad Length")
axis(1, at=1:2, labels=c("Hatched", "Not Hatched"))
wilcox.test(HfirstHh$TadLength, NHfirstHh$TadLength, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
t.test(HfirstHh$TadLength, NHfirstHh$TadLength,paired=TRUE)
plot(HfirstHh$TadLength, NHfirstHh$TadLength)
##Of the ones that hatched first (1 of 2 hatched), the ones that hatched are not significantly ahead of their sibs that didn’t hatch (P=0.6448). 

###########################################

# "of the ones that hatched first" = first hatch of the clutch (1 time point per clutch)
dat <- rbind(c254t[1:2,],          
             c255t[3:4,],
             c256t[5:6,],
             c257t[1:2,],
             c258t[5:6,],
             c259t[1:2,],
             c262t[7:8,],
             c263t[5:6,],
             c264t [3:4,],
             c265t [3:4,],
             c266t [5:6,],
             
             c254h [1:2,] ,          
             c255h [3:4,],
             c256h [1:2,],
             c257h [7:8,],
             c258h [5:6,],
             c259h [5:6,],
             c262h [5:6,],
             c263h [3:4,],
             c264h [1:2,],
             c265h [7:8,],
             c266h [7:8,])

# compare hatched vs. not hatched (unpaired)
Hat <- subset(dat, Response == "Hatched")
NotHat <- subset(dat, Response == "Not Hatched")

wilcox.test(Hat$SUM.of.trait.values, NotHat$SUM.of.trait.values, mu = 0, alt="two.sided", paired = F, conf.int=T, conf.level=0.99)






## does developmental stage predict latency to hatch after stimulus begins. 
library(car)

hist(onset.df$SUM.of.trait.values) 
hist(hypoxic$SUM.of.trait.values) 
hist(tactile$SUM.of.trait.values) 
scatterplot (onset.df$SUM.of.trait.values, onset.df$TtoH, log = "y", ylab="Latency to hatch (h)", xlab="Developmental Stage")
scatterplot (onset.df$TtoH ~ onset.df$SUM.of.trait.values | onset.df$Stimulus, reg.line=TRUE, col.lab="black", by.groups=T, pch=c(16,1), boxplots=F, lwd=2, lty=1, legend.title="Stimulus", levels=c("hypoxia", "tactile"), legend.coords="topright", ylab="Latency to hatch (h)", reset.par=T, xlab="Developmental Stage")

library(ggplot2)

#normal scale
ggplot(onset.df, aes(x = SUM.of.trait.values, y = TtoH, shape = Stimulus)) + 
  geom_point(size=3) +
  geom_smooth(method=lm, se=FALSE) +
  scale_shape_manual(values=c(16,1)) +
  ylab("Latency to Hatch (h)\n") +  
  theme_bw(20) +
  xlab("\nDevelopmental Stage")

#log scale
ggplot(onset.df, aes(x = SUM.of.trait.values, y = TtoH, shape = Stimulus)) + 
  scale_y_log10() +
  geom_point(size=3) +
  scale_shape_manual(values=c(15,0)) +
  geom_smooth(method=lm, se=FALSE) +
  ylab("Latency to Hatch (h)\n") +  
  theme_bw(20) +
  xlab("\nDevelopmental Stage")

install.packages("devtools")
library(devtools)
install_github("easyGgplot2", "kassambara")
library(easyGgplot2)
ggplot2.scatterplot(data=onset.df, xName='SUM.of.trait.values',yName='TtoH',
                    groupName='Stimulus', size=30, backgroundColor="white",
                    groupColors=c('black', 'black'), addRegLine=TRUE, 
                    addConfidenceInterval=F, setShapeByGroupName=FALSE,
                    removePanelGrid=TRUE)  

#yScale=“log10”

cor.test(onset.df$SUM.of.trait.values, onset.df$TtoH)
scatterplot (hypoxic$SUM.of.trait.values, hypoxic$TtoH, main = "hypoxia hatch", ylab="Latency to hatch (h)", xlab="Developmental Stage")
cor.test(hypoxic$SUM.of.trait.values, hypoxic$TtoH)
scatterplot (tactile$SUM.of.trait.values, tactile$TtoH, main = "tactile hatch", ylab="Latency to hatch (h)", xlab="Developmental Stage")
cor.test(tactile$SUM.of.trait.values, tactile$TtoH)



## does tadpole length predict Latency to hatch after stimulus begins. 
hist(onset.df$TadLength) 
hist(hypoxic$TadLength) 
hist(tactile$TadLength) 
scatterplot (onset.df$TadLength, onset.df$TtoH, ylab="Latency to hatch (h)", xlab="Tadpole Length (mm)")
cor.test(onset.df$TadLength, onset.df$TtoH)
scatterplot (hypoxic$TadLength, hypoxic$TtoH, main = "hypoxia hatch", ylab="Latency to hatch (h)", xlab="Tadpole Length (mm)")
cor.test(hypoxic$TadLength, hypoxic$TtoH)
scatterplot (tactile$TadLength, tactile$TtoH, main = "tactile hatch", ylab="Latency to hatch (h)", xlab="Tadpole Length (mm)")
cor.test(tactile$TadLength, tactile$TtoH)

plot (onset.df$SUM.of.trait.values, onset.df$TtoH, ylab="Latency to hatch (h)", xlab="Developmental Stage")
plot (hypoxic$SUM.of.trait.values, hypoxic$TtoH, main = "hypoxia hatch", ylab="Latency to hatch (h)", xlab="Developmental Stage")
plot (tactile$SUM.of.trait.values, tactile$TtoH, main = "tactile hatch", ylab="Latency to hatch (h)", xlab="Developmental Stage")

## tadpole length does not predict Latency to hatch after stimulus begins. 
plot (onset.df$TadLength, onset.df$TtoH, ylab="Latency to hatch (h)", xlab="Tadpole Length (mm)")
plot (hypoxic$TadLength, hypoxic$TtoH, main = "hypoxia hatch", ylab="Latency to hatch (h)", xlab="Tadpole Length (mm)")
plot (tactile$TadLength, tactile$TtoH, main = "tactile hatch", ylab="Latency to hatch (h)", xlab="Tadpole Length (mm)")

################################### 
################################### 
####### NEW FIGURE oct26, 2016 ####
################################### 
################################### 
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
# par(mfrow=c(1,2)) 
# 
# LtoHdata<-read.csv(file="LtoHdata.csv")
# LtoHdata<-na.omit(LtoHdata)
# str(LtoHdata)
# boxplot(log10(LtoHmins)~FirstVsConsistent*Stimulus, data=LtoHdata, notch=TRUE, ylab="Log of Latency to Hatch in Minutes", xlab="Stimulus")

p1<- ggplot(LtoHdata, aes( Stimulus,log10(LtoHmins), Colour=FirstVsConsistent))+
  geom_boxplot()+
  ylab("Log of Latency to Hatch (mins)\n") +  
  theme_bw(16) +
  xlab("\nStimulus")

#log scale
LtoHfig<-read.csv(file="LtoHfig.csv")
str(LtoHfig)
p2<- ggplot(LtoHfig, aes(x = Developmental.Stage, y = log10(Latency.to.Hatch.in.Minutes), shape = Stimulus)) + #TtoH is in mins
  geom_point(size=2) +
  geom_smooth(method=lm, se=FALSE) +
  stat_smooth(method = lm)+
  scale_shape_manual(values=c(15,0)) + #c(16,1) to make circles
  ylab("Log of Latency to Hatch (mins)\n") +  
  theme_bw(16) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  xlab("\nDevelopmental Stage")
  
multiplot(p1, p2, cols=2)

#scatter plot and boxplot overlay
max(LtoHfig$Developmental.Stage, na.rm=T)

LtoHfig$AvgLtoHmins<-as.numeric(LtoHfig$AvgLtoHmins)
library(MASS) # to access Animals data sets
library(scales) # to access break formatting functions
ggplot(LtoHfig, aes(x = Developmental.Stage, y = AvgLtoHmins, shape = Stimulus, colour=FirstVsConsistent, na.rm=T))+
  geom_boxplot(aes(fill = FirstVsConsistent))+
  geom_jitter(position=position_dodge(width=0.5))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  #scale_y_log10(breaks=c(.01,.1,1),labels=c(.01,.1,1))+
  scale_x_discrete(breaks=c(1,2,3,4,5,6,7),labels=c(1,2,3,4,5,6,7))+
  #xlim(min(LtoHfig$Developmental.Stage, na.rm=T), max(LtoHfig$Developmental.Stage, na.rm=T))+
  scale_shape_manual(values=c(15,0)) +
  scale_color_manual(values=c("black", "black")) +
  scale_fill_manual(values=c("azure3", "white")) +
  stat_smooth(method = lm, se=FALSE, color="blue", lty=2)+
  ylab("Latency to Hatch (min)\n") +  
  theme_bw(20) +
  xlab("\nDevelopmental Stage")

## Order 156, 220 -->NAs

min(LtoHfig$AvgLtoHmins, na.rm=T)
max(LtoHfig$AvgLtoHmins, na.rm=T)
min(LtoHfig$Developmental.Stage, na.rm=T)
max(LtoHfig$Developmental.Stage, na.rm=T)
#normal scale
LtoHfig$Log.Latency.to.Hatch.in.Minutes<- log10(LtoHfig$AvgLtoHmins)

ggplot(LtoHfig, aes(x = Developmental.Stage, y = Latency.to.Hatch.in.Minutes, shape = Stimulus)) + 
  geom_point(size=3) +
  scale_shape_manual(values=c(15,0)) +
  stat_smooth(method = lm, se=FALSE)+
  ylab("Latency to Hatch (h)\n") +  
  theme_bw(20) +
  xlab("\nDevelopmental Stage")

library(reshape2)


ggplot(LtoHfig,aes(Stimulus, AvgLtoHmins, Colour=FirstVsConsistent)) +
  geom_boxplot(aes(colour=FirstVsConsistent), show.legend=FALSE) +
  facet_grid(Stimulus ~ .) +
  theme_bw()

hypLtoHfig<-subset(LtoHfig, LtoHfig$Stimulus=="H")
tacLtoHfig<-subset(LtoHfig, LtoHfig$Stimulus=="T")

boxplot(hypLtoHfig$FirstVsConsistent, hypLtoHfig$AvgLtoHmins)





################################### 
################################### 
################################### 
################################### 
################################### 

#calculate means by hand from each clutch. 
# read.csv(file="my.csv.filename")
latencymeans.df<-read.csv(file="latencymeans.csv")

hyplatencymeans <- subset(latencymeans.df, Stimulus == "H")
meclatencymeans <- subset(latencymeans.df, Stimulus == "T")


###################### Q 2 ##########################
## Does latency to hatch after stimulus begins (TtoH) change from first hatch to consistent hatching (end criteria)
###################### Q 2 ##########################

#hist(latencymeans.df$First) #poisson or negative binomial
#hist(latencymeans.df$Last) #geometric or negative binomial
mean(latencymeans.df$FirstLtoHhours)
mean(latencymeans.df$ConsistentLtoHhours, na.rm=T) 
boxplot(latencymeans.df$ConsistentLtoHhours, latencymeans.df$FirstLtoHhours, xlab="hatching", ylab="Latency to hatch (h)")
axis(1, at=1:2, labels=c("first", "consistent"))
############# visual evidence of how different ##############

###################### ANS 2 ##########################
# Ho: Midean change in TtoH is 0
# two.sided test
wilcox.test(latencymeans.df$FirstLtoHhours, latencymeans.df$ConsistentLtoHhours, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
##nonparametric -->
##tests median <http://www.r-tutor.com/elementary-statistics/non-parametric-methods/wilcoxon-signed-rank-test>
###################### ANS 2 ##########################

hist(latencymeans.df$AgeLag)

hyp <- subset(latencymeans.df, Stimulus == "H")
tac <- subset(latencymeans.df, Stimulus == "T")

mean(hyp$FirstLtoHhours)
mean(hyp$ConsistentLtoHhours)
boxplot(hyp$FirstLtoHhours, hyp$ConsistentLtoHhours, xlab="hatching", ylab="Latency to hatch (h)")
axis(1, at=1:2, labels=c("first", "consistent"))
wilcox.test(hyp$FirstLtoHhours, hyp$ConsistentLtoHhours, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)

mean(tac$FirstLtoHhours)
mean(tac$ConsistentLtoHhours, na.rm=T)
boxplot(tac$FirstLtoHhours, tac$ConsistentLtoHhours, xlab="hatching", ylab="Latency to hatch (h)")
axis(1, at=1:2, labels=c("first", "consistent"))
wilcox.test(tac$FirstLtoHhours, tac$ConsistentLtoHhours, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)

## Age block when first vs. consistent // hyp vs. tactile

mean(hyp$FirstAgeBlock)
mean(hyp$ConsistentAgeBlock)
boxplot(hyp$FirstAgeBlock, hyp$ConsistentAgeBlock, xlab="hatching", ylab="Age Block (h)")
axis(1, at=1:2, labels=c("first", "consistent"))
wilcox.test(hyp$FirstAgeBlock, hyp$ConsistentAgeBlock, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)

mean(tac$FirstAgeBlock)
mean(tac$ConsistentAgeBlock, na.rm=T)
boxplot(tac$FirstAgeBlock, tac$ConsistentAgeBlock, xlab="hatching", ylab="Latency to hatch (h)")
axis(1, at=1:2, labels=c("first", "consistent"))
wilcox.test(tac$FirstAgeBlock, tac$ConsistentAgeBlock, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)


wilcox.test(tac$AgeLag, hyp$AgeLag, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
t.test(tac$AgeLag, hyp$AgeLag, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)

sum(tac$AgeLag)
sum(hyp$AgeLag)


##########
##########
##   Across clutches, HYP hatching began and became consistent earlier than MEC hatching
##       --> Age
##       --> Embryo size
##       --> Dev stage
##########
##########

#calculate means by hand from each clutch. 
# read.csv(file="my.csv.filename")
latencymeans.df<-read.csv(file="latencymeans.csv")

hyplatencymeans <- subset(latencymeans.df, Stimulus == "H")
meclatencymeans <- subset(latencymeans.df, Stimulus == "T")

# AGE ANALYSIS (significant)

# #hypoxia
# hist(hyplatencymeans$Agefirst) # normal --> t - test??
# hist(hyplatencymeans$Agelast) # normal --> t - test??
# #wilcox.test(hyplatencymeans$Agefirst, hyplatencymeans$Agelast, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
# t.test(hyplatencymeans$Agefirst, hyplatencymeans$Agelast, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
# plot(hyplatencymeans$Agefirst ~ hyplatencymeans$Agelast)

t.test(hyplatencymeans$Agefirst, meclatencymeans$Agefirst, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
plot(Agefirst ~ Stimulus, data=latencymeans.df)

t.test(hyplatencymeans$Agelast, meclatencymeans$Agelast, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
plot(Agelast ~ Stimulus, data=latencymeans.df)

# EMBRYO SIZE ANALYSIS

t.test(hyplatencymeans$Embryosizefirst, meclatencymeans$Embryosizefirst, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
plot(Embryosizefirst ~ Stimulus, data=latencymeans.df)

t.test(hyplatencymeans$Embryosizelast, meclatencymeans$Embryosizelast, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
plot(Embryosizelast ~ Stimulus, data=latencymeans.df)

# DEV STAGE ANALYSIS

t.test(hyplatencymeans$Devstagefirst, meclatencymeans$Devstagefirst, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
plot(Devstagefirst ~ Stimulus, data=latencymeans.df)

t.test(hyplatencymeans$Devstagelast, meclatencymeans$Devstagelast, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
plot(Devstagelast ~ Stimulus, data=latencymeans.df)


# #mecanosensory
# hist(meclatencymeans$Agefirst) # normal --> t - test??
# hist(meclatencymeans$Agelast) # normal --> t - test??
# #wilcox.test(meclatencymeans$Agefirst, meclatencymeans$Agelast, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
# t.test(meclatencymeans$Agefirst, meclatencymeans$Agelast, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
# plot(meclatencymeans$Agefirst ~ meclatencymeans$Agelast)
# 
# #first
# hist(hyplatencymeans$Agefirst) # normal --> t - test??
# hist(hyplatencymeans$Agelast) # normal --> t - test??
# #wilcox.test(hyplatencymeans$Agefirst, hyplatencymeans$Agelast, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
# t.test(hyplatencymeans$Agefirst, hyplatencymeans$Agelast, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
# plot(hyplatencymeans$Agefirst ~ hyplatencymeans$Agelast)
# 
# #consistent
# hist(meclatencymeans$Agefirst) # normal --> t - test??
# hist(meclatencymeans$Agelast) # normal --> t - test??
# #wilcox.test(meclatencymeans$Agefirst, meclatencymeans$Agelast, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
# t.test(meclatencymeans$Agefirst, meclatencymeans$Agelast, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
# plot(meclatencymeans$Agefirst ~ meclatencymeans$Agelast)


#DONE STAGE ANALYSIS (not significantly different between 2015 and 2016)

hist(compyrs.df$DoneStage) #normalish. 
wilcox.test(dat2015$DoneStage, dat2016$DoneStage, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
t.test(dat2015$DoneStage, dat2016$DoneStage, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)

compyrs.df$Year <- as.factor(compyrs.df$Year)
plot(DoneStage~Year, data=compyrs.df)


### "Stage at the onset of hatching was quite consistent under hypoxia and more variable in response to the mechanosensory cue (Fig. 3B; Levene’s test, F1,20 = 15.116, P = 0.0009). "
var(hyplatencymeans$Devstagefirst)
var(meclatencymeans$Devstagefirst)
var(hyplatencymeans$Devstagelast)
var(meclatencymeans$Devstagelast)

# load leveneTest function
library(car)
# run the levene test centered around the mean
leveneTest(latencymeans.df$Devstagefirst, latencymeans.df$Stimulus, center=mean)
#data frame with two columns, height (in inches) and sex (Male or Female) 
#and I want to run levene's test to see if the variance is the same for 
#Male and Female height. 
###################### Q 2 ##########################
## Does latency to hatch after stimulus begins (TtoH) change from first hatch to consistent hatching (end criteria)
###################### Q 2 ##########################

#hist(latencymeans.df$First) #poisson or negative binomial
#hist(latencymeans.df$Last) #geometric or negative binomial
mean(latencymeans.df$FirstLtoHhours)
mean(latencymeans.df$ConsistentLtoHhours, na.rm=T) 
boxplot(latencymeans.df$ConsistentLtoHhours, latencymeans.df$FirstLtoHhours, xlab="hatching", ylab="Latency to hatch (h)")
axis(1, at=1:2, labels=c("first", "consistent"))
############# visual evidence of how different ##############

###################### ANS 2 ##########################
# Ho: Midean change in TtoH is 0
# two.sided test
wilcox.test(latencymeans.df$FirstLtoHhours, latencymeans.df$ConsistentLtoHhours, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
##nonparametric -->
##tests median <http://www.r-tutor.com/elementary-statistics/non-parametric-methods/wilcoxon-signed-rank-test>
###################### ANS 2 ##########################

hist(latencymeans.df$AgeLag)
  
hyp <- subset(latencymeans.df, Stimulus == "H")
tac <- subset(latencymeans.df, Stimulus == "T")
  
  mean(hyp$FirstLtoHhours)
  mean(hyp$ConsistentLtoHhours)
  boxplot(hyp$FirstLtoHhours, hyp$ConsistentLtoHhours, xlab="hatching", ylab="Latency to hatch (h)")
  axis(1, at=1:2, labels=c("first", "consistent"))
  wilcox.test(hyp$FirstLtoHhours, hyp$ConsistentLtoHhours, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
  
  mean(tac$FirstLtoHhours)
  mean(tac$ConsistentLtoHhours, na.rm=T)
  boxplot(tac$FirstLtoHhours, tac$ConsistentLtoHhours, xlab="hatching", ylab="Latency to hatch (h)")
  axis(1, at=1:2, labels=c("first", "consistent"))
  wilcox.test(tac$FirstLtoHhours, tac$ConsistentLtoHhours, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
  
  
  ## Age block when first vs. consistent // hyp vs. tactile
  
  mean(hyp$FirstAgeBlock)
  mean(hyp$ConsistentAgeBlock)
  boxplot(hyp$FirstAgeBlock, hyp$ConsistentAgeBlock, xlab="hatching", ylab="Age Block (h)")
  axis(1, at=1:2, labels=c("first", "consistent"))
  wilcox.test(hyp$FirstAgeBlock, hyp$ConsistentAgeBlock, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
  
  mean(tac$FirstAgeBlock)
  mean(tac$ConsistentAgeBlock, na.rm=T)
  boxplot(tac$FirstAgeBlock, tac$ConsistentAgeBlock, xlab="hatching", ylab="Latency to hatch (h)")
  axis(1, at=1:2, labels=c("first", "consistent"))
  wilcox.test(tac$FirstAgeBlock, tac$ConsistentAgeBlock, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
  
  
  wilcox.test(tac$AgeLag, hyp$AgeLag, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
  t.test(tac$AgeLag, hyp$AgeLag, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
  
  sum(tac$AgeLag)
  sum(hyp$AgeLag)

min(LtoHfig$Latency.to.Hatch.in.Minutes, na.rm=T)



### checking if same results when use these 2 diff

latencymeansKW.df<-read.csv(file="SmallDatasetforJulie.csv")
newlatencymeansKW.df<-read.csv(file="NewSmallDatasetforJulie.csv")

hyplatencymeansKW <- subset(latencymeansKW.df, Stimulus == "H")
meclatencymeansKW <- subset(latencymeansKW.df, Stimulus == "T")

newhyplatencymeansKW <- subset(newlatencymeansKW.df, Stimulus == "H")
newmeclatencymeansKW <- subset(newlatencymeansKW.df, Stimulus == "T")

# AGE ANALYSIS (significant)

# #hypoxia
hyplatencymeansKW$FirstAgeBlock<-as.numeric(as.character(hyplatencymeansKW$FirstAgeBlock))

newhyplatencymeansKW$FirstAgeBlock<-numeric(newhyplatencymeansKW$FirstAgeBlock)

t.test(hyplatencymeansKW$FirstAgeBlock, meclatencymeansKW$FirstAgeBlock, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
#different
t.test(hyplatencymeansKW$ConsistentAgeBlock, meclatencymeansKW$ConsistentAgeBlock, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
#same

t.test(newhyplatencymeansKW$FirstAgeBlock, newmeclatencymeansKW$FirstAgeBlock, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
#different
t.test(newhyplatencymeansKW$ConsistentAgeBlock, newmeclatencymeansKW$ConsistentAgeBlock, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
#same

#wilcox.test(hyplatencymeansKW$FirstAgeBlock, meclatencymeansKW$FirstAgeBlock, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
#wilcox.test(hyplatencymeansKW$ConsistentAgeBlock, meclatencymeansKW$ConsistentAgeBlock, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)

# EMBRYO SIZE ANALYSIS
bothsize <- rbind(latencymeansKW.df$Embryosizefirst, latencymeansKW.df$Embryosizelast)
t.test(hyplatencymeansKW$Embryosizefirst, meclatencymeansKW$Embryosizefirst, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
t.test(hyplatencymeansKW$Embryosizelast, meclatencymeansKW$Embryosizelast, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)

bothsize <- rbind(latencymeansKW.df$Embryosizefirst, latencymeansKW.df$Embryosizelast)
t.test(newhyplatencymeansKW$Embryosizefirst, newmeclatencymeansKW$Embryosizefirst, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
t.test(newhyplatencymeansKW$Embryosizelast, newmeclatencymeansKW$Embryosizelast, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)

# DEV STAGE ANALYSIS
library(exactRankTests)
wilcox.exact(hyplatencymeansKW$Devstagefirst, meclatencymeansKW$Devstagefirst, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
#first stage - with correction factor (V=0, P=0.0009766)
wilcox.exact(hyplatencymeansKW$Devstagelast, meclatencymeansKW$Devstagelast, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
#last stage - with correction factor (V=0, P=0.0009766)

wilcox.exact(newhyplatencymeansKW$Devstagefirst, newmeclatencymeansKW$Devstagefirst, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
#first stage - with correction factor (V=0, P=0.0009766)
wilcox.exact(newhyplatencymeansKW$Devstagelast, newmeclatencymeansKW$Devstagelast, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
#last stage - with correction factor (V=0, P=0.0009766)


wilcox.exact(hyplatencymeansKW$Devstagefirst, meclatencymeansKW$Devstagefirst, mu = 0, alt="two.sided", paired = T, correct=F, conf.int=T, conf.level=0.99)
wilcox.exact(hyplatencymeansKW$Devstagelast, meclatencymeansKW$Devstagelast, mu = 0, alt="two.sided", paired = T, correct=F, conf.int=T, conf.level=0.99)
#same without correction factor

wilcox.test(hyplatencymeansKW$Devstagefirst, meclatencymeansKW$Devstagefirst, mu = 0, alt="two.sided", paired = T, correct=F, conf.int=T, conf.level=0.99)
wilcox.test(hyplatencymeansKW$Devstagelast, meclatencymeansKW$Devstagelast, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)

### "Stage at the onset of hatching was quite consistent under hypoxia and more variable in response to the mechanosensory cue (Fig. 3B; Levene’s test, F1,20 = 15.116, P = 0.0009). "

# load leveneTest function
library(car)
# run the levene test centered around the mean
leveneTest(latencymeansKW.df$Devstagefirst, latencymeansKW.df$Stimulus, center=mean)
#data frame with two columns, height (in inches) and sex (Male or Female) 
#and I want to run levene's test to see if the variance is the same for 
#Male and Female height. 
leveneTest(newlatencymeansKW.df$Devstagefirst, newlatencymeansKW.df$Stimulus, center=mean)


#### "However the lag time from first until consistent hatching did not differ between stimulus types 
#### (t10 = 1.047, P = 0.32)."

latencymeansKW.df$AgeLag<-latencymeansKW.df$ConsistentAgeBlock - latencymeansKW.df$FirstAgeBlock
meclatencymeansKW$AgeLag<-meclatencymeansKW$ConsistentAgeBlock - meclatencymeansKW$FirstAgeBlock
hyplatencymeansKW$AgeLag<-hyplatencymeansKW$ConsistentAgeBlock - hyplatencymeansKW$FirstAgeBlock
hist(latencymeansKW.df$AgeLag)
hist(meclatencymeansKW$AgeLag)
hist(hyplatencymeansKW$AgeLag)

# if want non-parametric
wilcox.test(meclatencymeansKW$AgeLag, hyplatencymeansKW$AgeLag, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
# currently reporting parametric test
t.test(meclatencymeansKW$AgeLag, hyplatencymeansKW$AgeLag, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)

# NEW ##
#### "However the lag time from first until consistent hatching did not differ between stimulus types 
#### (t10 = 1.047, P = 0.32)."

newlatencymeansKW.df$AgeLag<-newlatencymeansKW.df$ConsistentAgeBlock - newlatencymeansKW.df$FirstAgeBlock
newmeclatencymeansKW$AgeLag<-newmeclatencymeansKW$ConsistentAgeBlock - newmeclatencymeansKW$FirstAgeBlock
newhyplatencymeansKW$AgeLag<-newhyplatencymeansKW$ConsistentAgeBlock - newhyplatencymeansKW$FirstAgeBlock
hist(newlatencymeansKW.df$AgeLag)
hist(newmeclatencymeansKW$AgeLag)
hist(newhyplatencymeansKW$AgeLag)

# currently reporting parametric test
t.test(newmeclatencymeansKW$AgeLag, newhyplatencymeansKW$AgeLag, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)


