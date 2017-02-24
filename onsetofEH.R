# Analysis for "onset of escape-hatching"
# title: Developmental onset of the escape-hatching response in red-eyed treefrogs depends on cue type
# May-Aug 2016
# Julie Jung

rm(list=ls())
setwd('/Users/juliejung/Desktop/2cues m.s.')        

##### Q1
##### Is the age when "done" (i.e. "consistent" hatching) different among years (2015 vs. 2016)
#####

compyrs.df<-read.csv(file="compyrs.csv")
#str(compyrs.df)

# compare 2015 vs. 2016 (paired)
dat2015 <- subset(compyrs.df, Year == 2015)
dat2016 <- subset(compyrs.df, Year == 2016)

#DONE AGE ANALYSIS (significant or nearly significant - depending on test)

hist(compyrs.df$DoneAge)
mean(dat2016$DoneAge)
sd(dat2016$DoneAge)

summary(dat2015$DoneAge)
mean(dat2015$DoneAge)
sd(dat2015$DoneAge)

#t-test, paired
t.test(dat2015$DoneAge, dat2016$DoneAge, mu = 0, alt="two.sided", paired = T)
#######t-test, unpaired (THIS IS THE ONE WE WANT)
t.test(dat2015$DoneAge, dat2016$DoneAge, mu = 0)
#wilcox signed rank (paired) test with continuity correction
wilcox.test(dat2015$DoneAge, dat2016$DoneAge, mu = 0, alt="two.sided", paired = T)
#wilcox signed rank (paired) test withOUT continuity correction
wilcox.test(dat2015$DoneAge, dat2016$DoneAge, mu = 0, alt="two.sided", paired = T, correct=F)
#wilcox rank sum test WITH continuity correction
wilcox.test(dat2015$DoneAge, dat2016$DoneAge)
#wilcox rank sum test WITHOUT continuity correction
wilcox.test(dat2015$DoneAge, dat2016$DoneAge, correct=F)
#wilcox rank sum test WITHOUT continuity correction
wilcox.test(DoneAge~Year, compyrs.df, correct=F)


compyrs.df$Year <- as.factor(compyrs.df$Year)
plot(DoneAge~Year, data=compyrs.df)

#DONE STAGE ANALYSIS (not significantly different between 2015 and 2016)

hist(compyrs.df$DoneStage) #normalish. 
wilcox.test(dat2015$DoneStage, dat2016$DoneStage, mu = 0, alt="two.sided", paired = T)
t.test(dat2015$DoneStage, dat2016$DoneStage, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
#### use this unpaired wilcox.test
wilcox.test(dat2015$DoneStage, dat2016$DoneStage, mu = 0)

t.test(dat2015$DoneStage, dat2016$DoneStage, mu = 0)

compyrs.df$Year <- as.factor(compyrs.df$Year)
plot(DoneStage~Year, data=compyrs.df)

##" The age of consistent hatching in response to mechanosensory cues is different between 2015 and 2016 (t10=-4.9796, P=0.0005538); however the stage of consistent hatching is not (t10=-0.6708, P=0.5175). " 








##### Q2
##### Across clutches, hypoxia-cued hatching both began and became consistent earlier than did mechanosensory cued hatching, whether 
##### measured by age (Fig. 2; first hatching: t10=7.7198, P<0.0001; consistent: t10=6.8472, P<0.0001), 
##### embryo size (Fig. 3A; first hatching: t10=4.6818, P=0.0004; consistent: t10=6.2057, P<0.0001), 
##### or developmental stage (Fig. 3B; both first and consistent hatching: S10=33.0, N = 11, P=0.0005).
#####

###############
###############
############## Using Karen's SmallDatasetforJulie.xlsx instead of latencymeans.csv
##############
################

latencymeansKW.df<-read.csv(file="SmallDatasetforJulie.csv")

hyplatencymeansKW <- subset(latencymeansKW.df, Stimulus == "H")
meclatencymeansKW <- subset(latencymeansKW.df, Stimulus == "T")

# AGE ANALYSIS (significant)

# #hypoxia
hyplatencymeansKW$FirstAgeBlock<-as.numeric(as.character(hyplatencymeansKW$FirstAgeBlock))
hist(hyplatencymeansKW$FirstAgeBlock) # normal --> t - test??
hist(hyplatencymeansKW$ConsistentAgeBlock) # normal --> t - test??
hist(meclatencymeansKW$FirstAgeBlock) # not normal
hist(meclatencymeansKW$ConsistentAgeBlock) # not normal

hist(latencymeansKW.df$FirstAgeBlock) # not-normal
hist(latencymeansKW.df$ConsistentAgeBlock) # not-normal
shapiro.test(latencymeansKW.df$FirstAgeBlock) #not normal
shapiro.test(latencymeansKW.df$ConsistentAgeBlock) # not normal
both <- rbind(latencymeansKW.df$FirstAgeBlock, latencymeansKW.df$ConsistentAgeBlock)
hist(both) # = visually normal!! so can do t-test!!!
shapiro.test(both) # yes normal

wilcox.test(hyplatencymeansKW$FirstAgeBlock, hyplatencymeansKW$ConsistentAgeBlock, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
wilcox.test(meclatencymeansKW$FirstAgeBlock, meclatencymeansKW$ConsistentAgeBlock, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)

# t.test(hyplatencymeansKW$FirstAgeBlock, hyplatencymeansKW$ConsistentAgeBlock, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
# plot(hyplatencymeansKW$FirstAgeBlock ~ hyplatencymeansKW$ConsistentAgeBlock)

t.test(hyplatencymeansKW$FirstAgeBlock, meclatencymeansKW$FirstAgeBlock, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
#different
t.test(hyplatencymeansKW$ConsistentAgeBlock, meclatencymeansKW$ConsistentAgeBlock, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
#same

#wilcox.test(hyplatencymeansKW$FirstAgeBlock, meclatencymeansKW$FirstAgeBlock, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
#wilcox.test(hyplatencymeansKW$ConsistentAgeBlock, meclatencymeansKW$ConsistentAgeBlock, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)

# EMBRYO SIZE ANALYSIS
bothsize <- rbind(latencymeansKW.df$Embryosizefirst, latencymeansKW.df$Embryosizelast)
hist(bothsize) # = visually normal!! so can do t-test!!!
shapiro.test(bothsize) # yes normal

t.test(hyplatencymeansKW$Embryosizefirst, meclatencymeansKW$Embryosizefirst, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
#plot(Embryosizefirst ~ Stimulus, data=latencymeansKW.df)

t.test(hyplatencymeansKW$Embryosizelast, meclatencymeansKW$Embryosizelast, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
plot(Embryosizelast ~ Stimulus, data=latencymeansKW.df)

# DEV STAGE ANALYSIS
bothstage <- rbind(latencymeansKW.df$Devstagefirst, latencymeansKW.df$Devstagelast)
hist(bothstage) # not normal
shapiro.test(bothstage) #not normal

t.test(meclatencymeansKW$Devstagefirst,hyplatencymeansKW$Devstagefirst,  mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
plot(Devstagefirst ~ Stimulus, data=latencymeans.df)

t.test(hyplatencymeansKW$Devstagelast, meclatencymeansKW$Devstagelast, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
plot(Devstagelast ~ Stimulus, data=latencymeans.df)

#################
#################
################# WHY IN THE WORLD ARE THESE DIFFERENT from kw's results?? check corrections
wilcox.test(hyplatencymeansKW$Devstagefirst, meclatencymeansKW$Devstagefirst, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
wilcox.test(hyplatencymeansKW$Devstagelast, meclatencymeansKW$Devstagelast, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
wilcox.test(hyplatencymeansKW$Devstagefirst, meclatencymeansKW$Devstagefirst, mu = 0, alt="two.sided", paired = T, correct=F, conf.int=T, conf.level=0.99)
wilcox.test(hyplatencymeansKW$Devstagelast, meclatencymeansKW$Devstagelast, mu = 0, alt="two.sided", paired = T, correct=F, conf.int=T, conf.level=0.99)

library(exactRankTests)
wilcox.exact(hyplatencymeansKW$Devstagefirst, meclatencymeansKW$Devstagefirst, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
#first stage - with correction factor (V=0, P=0.0009766)
wilcox.exact(hyplatencymeansKW$Devstagelast, meclatencymeansKW$Devstagelast, mu = 0, alt="two.sided", paired = T, conf.int=T, conf.level=0.99)
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




####SEE ABOVE Q1 for this analysis

#### Mechanosensory cued hatching became consistent later in 2016 than 2015 
#### (2016 mode, 4 d noon, 108 h; mean ± SD = 108.8 ± 2.7 h; 
#### Wilcoxon rank-sum test, Z = 3.3079, N2015 = N2016 = 11, P = 0.0009), 

####  #DONE STAGE ANALYSIS (not significantly different between 2015 and 2016)
#### Mechanosensory cued hatching became consistent at the same developmental stage in 2016 than 2015 
#### (stage 6, Wilcoxon rank-sum test, Z = 0.5570, N2015 = N2016 = 11, P = 0.58). 






###################### Q1 ##########################
## Does latency to hatch after stimulus begins differ between hypoxia and mechanically cued embryos?
###################### Q1 ##########################

# read.csv(file="my.csv.filename")
onset.df<-read.csv(file="ontogeny.csv")

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

#"The latency to hatch after onset of the stimulus was longer for hypoxia than for mechanosensory cues (Wilcoxon signed-rank test on clutch mean values, V = 0, N = 11, P = 0.00098""
wilcox.test(means[,1], means[,2], mu = 0, alt="two.sided", paired = TRUE, conf.int=T, conf.level=0.99)
mean(means[,1])*60
mean(means[,2])*60


ramp <- matrix(c(9,6,3,6,6,9,15,9,15,6,9,6,9,9,6,6,6,6,12,12,3,3), ncol=2, byrow=FALSE)
colnames(ramp) <- c("tactile", "hypoxia")
rownames(ramp) <- c("c254", "c255", "c256", "c257", "c258", "c259", "c262", "c263", "c264", "c265", "c266")
ramp <- as.dataframe(ramp)
wilcox.test(ramp[,1], ramp[,2], mu = 0, alt="two.sided", paired = TRUE, conf.int=T, conf.level=0.99)


