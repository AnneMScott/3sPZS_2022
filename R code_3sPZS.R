#####################
#3sPZS electro-olfactogram limit of detection
#Fig. 1A
#####################

x = c(-0.24292, -0.251355, -0.636902, -0.216838, -0.239563, -0.316772, -0.4611205)
y = c(-0.320107, -0.223083, -0.331421, -0.231852, -0.1297, -0.086975, -0.125630333)
t.test (x, y, alternative = c("less"), paired= TRUE)


#####################
#PZS electro-olfactogram limit of detection
#Fig. 1B
#####################

x = c(-1.594238, -0.856226, -0.050659, -1.230469, -1.698303, -0.636189667, -0.548706, -1.628723333)
y = c(-0.596415, -0.265503, -0.014648, -0.028687, -0.144958, -0.028381, -0.159428917, -0.148010333)
t.test (x, y, alternative = c("less"), paired= TRUE)


#####################
#3kPZS electro-olfactogram limit of detection
#Fig. 1C
#####################

x = c(-1.733704, -3.406067, -0.39032, -0.2948, -0.351868, -0.624695, -1.096497)
y = c(-0.33905, -0.411987, -0.1867675, -0.146637, -0.137329, -0.230408, -0.1683045)
t.test (x, y, alternative = c("less"), paired= TRUE)


#####################
#Behavioral responses of sexually mature sea lamprey exposed to  in a two-choice flume
#Fig. 2
#Wilcoxon Signed-rank test for two-choice behavioral flume 
# Vehicle (50% methanol) vs treatment applied to reach a concentration in the flume of 10-12 M unless otherwise noted
#####################
setwd()

#Ovulated female: 3kPZS 10-12 M flume trials
flume <- read.csv("OF_3kPZS 10-12 M.csv", header= T) 
wilcox.test(flume$Var1_ae, flume$Var2_ac, alternative =c("two.sided"), paired = TRUE, conf.int = TRUE, conf.level =0.95)

#Ovulated female: 3kPZS 10-12 M + 3sPZS 10-12 M flume trials
flume <- read.csv("OF_3kPZS 10-12 M + 3sPZS 10-12 M.csv", header= T) 
wilcox.test(flume$Var1_ae, flume$Var2_ac, alternative =c("two.sided"), paired = TRUE, conf.int = TRUE, conf.level =0.95)

#Ovulated female: 3sPZS 10-12 M flume trials
flume <- read.csv("OF_3sPZS 10-12 M.csv", header= T) 
wilcox.test(flume$Var1_ae, flume$Var2_ac, alternative =c("two.sided"), paired = TRUE, conf.int = TRUE, conf.level =0.95)

#Ovulated female: PZS 10-12 M flume trials
flume <- read.csv("OF_PZS 10-12 M.csv", header= T) 
wilcox.test(flume$Var1_ae, flume$Var2_ac, alternative =c("two.sided"), paired = TRUE, conf.int = TRUE, conf.level =0.95)

#Ovulated female: PZS 10-12 M + 3sPZS 10-12 M flume trials
flume <- read.csv("OF_PZS 10-12 M + 3sPZS 10-12 M.csv", header= T) 
wilcox.test(flume$Var1_ae, flume$Var2_ac, alternative =c("two.sided"), paired = TRUE, conf.int = TRUE, conf.level =0.95)

#Ovulated female: Male Pheromone applied to reach 3kPZS at 10-12 M flume trials
flume <- read.csv("OF_Male Pher_3kPZS 10-12 M.csv", header= T) 
wilcox.test(flume$Var1_ae, flume$Var2_ac, alternative =c("two.sided"), paired = TRUE, conf.int = TRUE, conf.level =0.95)

#Ovulated female: Male Pheromone applied to reach 3kPZS at 10-12 M + 3sPZS 10-12 M flume trials
flume <- read.csv("OF_Male Pher_3kPZS 10-12 M + 3sPZS 10-12 M.csv", header= T) 
wilcox.test(flume$Var1_ae, flume$Var2_ac, alternative =c("two.sided"), paired = TRUE, conf.int = TRUE, conf.level =0.95)

#Ovulated female: Male Pheromone applied to reach 3kPZS at 10-14 M flume trials
flume <- read.csv("OF_Male Pher_3kPZS 10-14 M.csv", header= T) 
wilcox.test(flume$Var1_ae, flume$Var2_ac, alternative =c("two.sided"), paired = TRUE, conf.int = TRUE, conf.level =0.95)

#Ovulated female: Male Pheromone applied to reach 3kPZS at 10-14 M + 3sPZS 10-12 M + PZS 10-12 M flume trials
flume <- read.csv("OF_Male Pher_3kPZS 10-14 M + 3sPZS 10-12 M + PZS 10-12 M.csv", header= T) 
wilcox.test(flume$Var1_ae, flume$Var2_ac, alternative =c("two.sided"), paired = TRUE, conf.int = TRUE, conf.level =0.95)


#Spermiated male: 3kPZS 10-12 M flume trials
flume <- read.csv("SM_3kPZS 10-12 M.csv", header= T) 
wilcox.test(flume$Var1_ae, flume$Var2_ac, alternative =c("two.sided"), paired = TRUE, conf.int = TRUE, conf.level =0.95)

#Spermiated male: 3kPZS 10-12 M + 3sPZS 10-12 M flume trials
flume <- read.csv("SM_3kPZS 10-12 M + 3sPZS 10-12 M.csv", header= T) 
wilcox.test(flume$Var1_ae, flume$Var2_ac, alternative =c("two.sided"), paired = TRUE, conf.int = TRUE, conf.level =0.95)

#Spermiated male: 3sPZS 10-12 M flume trials
flume <- read.csv("SM_3sPZS 10-12 M.csv", header= T) 
wilcox.test(flume$Var1_ae, flume$Var2_ac, alternative =c("two.sided"), paired = TRUE, conf.int = TRUE, conf.level =0.95)


##Between treatment groups comparisons

#Ovulated female: 3kPZS 10-12M vs 3kPZS 10-12 M + 3sPZS 10-12 M
flume <- read.csv("OF_3kPZS 10-12M vs 3kPZS 10-12 M + 3sPZS 10-12 M.csv", header= T) 
t.test (flume$Var1_3kPZS, flume$Var2_3kPZS.3sPZS, alternative = c("two.sided"), paired= FALSE)

#Ovulated female: PZS 10-12M vs PZS 10-12 M + 3sPZS 10-12 M
flume <- read.csv("OF_PZS 10-12M vs PZS 10-12 M + 3sPZS 10-12 M.csv", header= T) 
t.test (flume$Var1_PZS, flume$Var2_PZS.3sPZS, alternative = c("two.sided"), paired= FALSE)

#Ovulated female: Male Pheromone applied to reach 3kPZS at 10-14 M vs Male Pheromone applied to reach 3kPZS at 10-14 M + 3sPZS 10-12 M + PZS 10-12 M flume trials
flume <- read.csv("OF_Male Pher vs Male Pher_3kPZS 10-14 M + 3sPZS 10-12 M + PZS 10-12 M.csv", header= T) 
t.test (flume$Var1_MalePher, flume$Var2_MalePher.3sPZS.PZS, alternative = c("two.sided"), paired= FALSE)

#Ovulated female: Male Pheromone applied to reach 3kPZS at 10-12 M vs Male Pheromone applied to reach 3kPZS at 10-12 M + 3sPZS 10-12 M flume trials
flume <- read.csv("OF_Male Pher vs Male Pher_3kPZS 10-12 M + 3sPZS 10-12 M.csv", header= T) 
wilcox.test(flume$Var1_MalePher, flume$Var2_MalePher.3sPZS, alternative =c("two.sided"), paired = FALSE, conf.int = TRUE, conf.level =0.95)

#Spermiated male: 3kPZS 10-12M vs 3kPZS 10-12 M + 3sPZS 10-12 M
flume <- read.csv("SM_3kPZS 10-12M vs 3kPZS 10-12 M + 3sPZS 10-12 M.csv", header= T) 
wilcox.test(flume$Var1_3kPZS, flume$Var2_3kPZS.3sPZS, alternative = c("two.sided"), paired= FALSE, conf.int = TRUE, conf.level =0.95)


#####################
#Behavioral responses of sexually mature sea lamprey exposed to 3kPZS + 3sPZS
#Fig. 3 A-D
#Ovulated females exposed to a side-by-side odorant comparison of 3kPZS (5 x 10-13 M, final in-stream concentration) vs. 3kPZS (5 x 10-13 M) (control trials) 
#or to 3kPZS (5 x 10-13 M) vs. a mixture of 3kPZS and 3sPZS (1:1, each at 5 x 10-13 M) (experimental trials).

#Treatments:
#tkControl: 3kPZS (5E-13 M) applied to both adjacent nest at the same time.
#tkpzs: 3kPZS (5E-13 M) + 3sPZS (5E-13 M) mixture (1:1) applied to one nest, 3kPZS alone (5E-13 M) applied to other
#####################

#Response of interest: Remaining in the release cage
#cge = 1 = Yes, remained in release cage
#cge = 0 = No, did not remain in release cage

setwd()

cage<-read.table("cage_3sPZS + 3kPZS.csv", sep=",", header=T)
summary(cage)

cage$Treatment<-factor(cage$Treatment)
cage$cge<-factor(cage$cge)
summary(cage)

cage$Treatment<-relevel(cage$Treatment, ref="tkControl") #Change reference factor

cageglm<-glm(cge~Treatment, family=binomial, data= cage, na.action=na.omit) 

anova(cageglm,test="Chi")


#Response of interest: Downstream movement
#ds = 1 = Yes, moved downstream
#ds = 0 = No, did not move downstream

setwd()

dn<-read.table("dn_3sPZS + 3kPZS.csv", sep=",", header=T)
summary(dn)

dn$Treatment<-factor(dn$Treatment)
dn$ds<-factor(dn$ds)
summary(dn)

dn$Treatment<-relevel(dn$Treatment, ref="tkControl") 

dnglm<-glm(ds~Treatment, family=binomial, data= dn, na.action=na.omit)

anova(dnglm,test="Chi")


#Response of interest: Upstream movement
#us = 1 = Yes, moved upstream
#us = 0 = No, did not move upstream

setwd()

up<-read.table("up_3sPZS + 3kPZS.csv", sep=",", header=T)
summary(up)

up$Treatment<-factor(up$Treatment)
up$us<-factor(up$us)
summary(up)

up$Treatment<-relevel(up$Treatment, ref="tkControl") 

upglm<-glm(us~Treatment, family=binomial, data= up, na.action=na.omit) 

anova(upglm,test="Chi")


#Response of interest: First odor choice (enter odorant A or odorant B within control or experimental trials)
## Comparing first nest entry within 3kPZS vs 3kPZS  trials
setwd()

nest<- read.csv("nest.control_3sPZS + 3kPZS.csv", header= T)

##Description of variables
#trial.date:  Julian date…
#Treatment: tkControl= 3kPZS vs 3kPZS control trials; tkPZS= 3kPZS + 3sPZS vs 3kPZS treatment trials
#left.odor: odor applied to left nest. 
#right.odor: odor applied to right nest
#enter.left: number of females that chose the left nest first
#enter.right: number of females that chose the right nest first
#odor: the treatment that is designated as the "odor". For 3kPZS vs 3kPZS control trials, one nest was arbitrarily assigned as treatment. For 3kPZS + 3sPZS vs 3kPZS treatment trials, the 3kPZS + PZS is the treatment. 
#o.right: odor was on the right 
#e.right: individual entered the right nest first
#e.odor: individual entered the odor nest first 

## Changing parameters to factors
nest$o.right<-factor(nest$o.right) 
nest$e.right<-factor(nest$e.right) 
nest$e.odor<-factor(nest$e.odor) 
nest$trial<-factor(nest$trial)
summary(nest)

glm.nat <- glm(e.right ~ o.right, family=binomial,data = nest, na.action=na.omit)
summary(glm.nat)

anova(glm.nat,test = "Chi") 
#Output results: Chi square= 0.096104, df = 1, P = 0.7566


## Comparing first nest entry within 3kPZS + 3sPZS vs 3kPZS treatment trials###

nest<- read.csv("nest.antagonist_3sPZS + 3kPZS.csv", header= T)

##Description of variables
#trial.date:  Julian date…
#Treatment:  tkPZS= 3kPZS + 3sPZS vs 3kPZS treatment trials
#left.odor: odor applied to left nest. 
#right.odor: odor applied to right nest
#enter.left: number of females that chose the left nest first
#enter.right: number of females that chose the right nest first
#odor: the treatment that is designated as the "odor". For 3kPZS vs 3kPZS control trials, one nest was arbitrarily assigned as treatment. For 3kPZS + 3sPZS vs 3kPZS treatment trials, the 3kPZS + PZS is the treatment. 
#o.right: odor was on the right 
#e.right: individual entered the right nest first
#e.odor: individual entered the odor nest first 

## Changing parameters to factors
nest$o.right<-factor(nest$o.right) 
nest$e.right<-factor(nest$e.right) 
nest$e.odor<-factor(nest$e.odor) 
nest$trial<-factor(nest$trial)
summary(nest)

glm.nat <- glm(e.right ~ o.right, family=binomial,data = nest, na.action=na.omit)
summary(glm.nat)

anova(glm.nat,test = "Chi")
#Output results: Chi square= 14.873, df = 1, P = 0.000115 --> < 0.001


#####################
#Fig 3A-D retention
#####################

#Paired retention at odorant nest A versus odorant nest B for 3kPZS vs 3kPZS trials
setwd()
retention <- read.csv("retention_3sPZS + 3kPZS.csv", header= T) 
wilcox.test(retention$Odorant.A.control, retention$Odorant.B.control, alternative =c("two.sided"), paired = TRUE, conf.int = TRUE, conf.level =0.95)

#Paired retention at odorant nest A versus odorant nest B for 3sPZS + 3kPZS vs 3kPZS trials
retention <- read.csv("retention_3sPZS + 3kPZS.csv", header= T) 
wilcox.test(retention$Odorant.A.exp, retention$Odorant.B.exp, alternative =c("two.sided"), paired = TRUE, conf.int = TRUE, conf.level =0.95)


#####################
#Behavioral responses of sexually mature sea lamprey exposed to 3kPZS + 3sPZS + PZS
#Fig. 3 E-H
#Ovulated females exposed to a side-by-side odorant comparison of 3kPZS (5 x 10-13 M, final in-stream concentration) vs. 3kPZS (5 x 10-13 M) (control trials) 
#or to 3kPZS (5 x 10-13 M) vs. a mixture of 3sPZS, PZS, and 3kPZS (10:10:1, 3kPZS at 5 x 10-13 M) (experimental trials).

#Treatments:
#tkControl: 3kPZS (5E-13 M) applied to both adjacent nest at the same time.
#tkpzs: 3sPZS (5E-12 M) + PZS (5E-12 M) + 3kPZS (5E-13 M) mixture (10:10:1) applied to one nest, 3kPZS alone (5E-13 M) applied to other
#####################

#Response of interest: Remaining in the release cage
#cge = 1 = Yes, remained in release cage
#cge = 0 = No, did not remain in release cage

setwd()

cage<-read.table("cage_3sPZS + PZS + 3kPZS.csv", sep=",", header=T)
summary(cage)

cage$Treatment<-factor(cage$Treatment)
cage$cge<-factor(cage$cge)
summary(cage)

cage$Treatment<-relevel(cage$Treatment, ref="tkControl") #Change reference factor

cageglm<-glm(cge~Treatment, family=binomial, data= cage, na.action=na.omit) 

anova(cageglm,test="Chi")


#Response of interest: Downstream movement
#ds = 1 = Yes, moved downstream
#ds = 0 = No, did not move downstream

setwd()

dn<-read.table("dn_3sPZS + PZS + 3kPZS.csv", sep=",", header=T)
summary(dn)

dn$Treatment<-factor(dn$Treatment)
dn$ds<-factor(dn$ds)
summary(dn)

dn$Treatment<-relevel(dn$Treatment, ref="tkControl") 

dnglm<-glm(ds~Treatment, family=binomial, data= dn, na.action=na.omit)

anova(dnglm,test="Chi")


#Response of interest: Upstream movement
#us = 1 = Yes, moved upstream
#us = 0 = No, did not move upstream

setwd()

up<-read.table("up_3sPZS + PZS + 3kPZS.csv", sep=",", header=T)
summary(up)

up$Treatment<-factor(up$Treatment)
up$us<-factor(up$us)
summary(up)

up$Treatment<-relevel(up$Treatment, ref="tkControl") 

upglm<-glm(us~Treatment, family=binomial, data= up, na.action=na.omit) 

anova(upglm,test="Chi")


#Response of interest: First odor choice (enter odorant A or odorant B within control or experimental trials)
## Comparing first nest entry within 3kPZS vs 3kPZS  trials
setwd()

nest<- read.csv("nest.control_3sPZS + PZS + 3kPZS.csv", header= T)

##Description of variables
#trial.date:  Julian date
#Treatment: tkControl= 3kPZS vs 3kPZS control trials; tkPZS= 3sPZS + PZS + 3kPZS vs 3kPZS treatment trials
#left.odor: odor applied to left nest. 
#right.odor: odor applied to right nest
#enter.left: number of females that chose the left nest first
#enter.right: number of females that chose the right nest first
#odor: the treatment that is designated as the "odor". For 3kPZS vs 3kPZS control trials, one nest was arbitrarily assigned as treatment. For 3sPZS + PZS + 3kPZS vs 3kPZS treatment trials, the 3kPZS + PZS is the treatment. 
#o.right: odor was on the right 
#e.right: individual entered the right nest first
#e.odor: individual entered the odor nest first 

## Changing parameters to factors
nest$o.right<-factor(nest$o.right) 
nest$e.right<-factor(nest$e.right) 
nest$e.odor<-factor(nest$e.odor) 
nest$trial<-factor(nest$trial)
summary(nest)

glm.nat <- glm(e.right ~ o.right, family=binomial,data = nest, na.action=na.omit)
summary(glm.nat)

anova(glm.nat,test = "Chi") 
#Output results: Chi square= 0.0058001, df = 1, P = 0.9393


## Comparing first nest entry within 3sPZS + PZS + 3kPZS vs 3kPZS treatment trials###

nest<- read.csv("nest.antagonist_3sPZS + PZS + 3kPZS.csv", header= T)

##Description of variables
#trial.date:  Julian date
#Treatment:  tkPZS= 3sPZS + PZS + 3kPZS vs 3kPZS treatment trials
#left.odor: odor applied to left nest. 
#right.odor: odor applied to right nest
#enter.left: number of females that chose the left nest first
#enter.right: number of females that chose the right nest first
#odor: the treatment that is designated as the "odor". For 3kPZS vs 3kPZS control trials, one nest was arbitrarily assigned as treatment. For 3sPZS + PZS + 3kPZS vs 3kPZS treatment trials, the 3kPZS + PZS is the treatment. 
#o.right: odor was on the right 
#e.right: individual entered the right nest first
#e.odor: individual entered the odor nest first 

## Changing parameters to factors
nest$o.right<-factor(nest$o.right) 
nest$e.right<-factor(nest$e.right) 
nest$e.odor<-factor(nest$e.odor) 
nest$trial<-factor(nest$trial)
summary(nest)
nest


glm.nat <- glm(e.right ~ o.right, family=binomial,data = nest, na.action=na.omit)
summary(glm.nat)

anova(glm.nat,test = "Chi")
#Output results: Chi square= 31.841, df = 1, P = 1.673e-08 --> < 0.001


#####################
#Fig 3E-H retention
#####################

#Paired retention for 3kPZS vs 3kPZS trials
#Alternative input method by making vectors of data
#x = c(0, 49, 1289, 70, 3, 3 ,10, 0, 33, 0, 21, 0, 0, 0 ,0, 0, 0, 0, 3, 4, 2, 13, 5, 0, 0, 0 ,5, 9, 45, 5, 2, 2, 1, 340, 5, 1, 77, 102, 22, 13, 139, 3, 2, 42, 146, 103, 0, 0, 0, 147, 76, 0, 3, 33) #odorant source a
#y = c(8, 0, 213, 0, 0, 0, 355, 307, 93, 3, 44, 51, 1, 52, 135, 7, 2, 11, 0, 95, 0, 0, 0, 113, 7, 61, 143, 0, 0, 0, 0, 0, 0, 0, 5, 20, 0, 0, 0, 130, 0, 78, 0, 298, 0, 0, 4, 86, 15, 126, 48, 85, 24, 0)#odorant source b
#wilcox.test (x, y, alternative = c("two.sided"), paired = TRUE, conf.int = TRUE, conf.level =0.95)

#Paired retention at odorant nest A versus odorant nest B for 3kPZS vs 3kPZS trials
setwd()
retention <- read.csv("retention_3sPZS + PZS + 3kPZS.csv", header= T) 
wilcox.test(retention$Odorant.A.control, retention$Odorant.B.control, alternative =c("two.sided"), paired = TRUE, conf.int = TRUE, conf.level =0.95)

#Paired retention at odorant nest A versus odorant nest B for 3sPZS + PZS + 3kPZS vs 3kPZS trials
retention <- read.csv("retention_3sPZS + PZS + 3kPZS.csv", header= T) 
wilcox.test(retention$Odorant.A.exp, retention$Odorant.B.exp, alternative =c("two.sided"), paired = TRUE, conf.int = TRUE, conf.level =0.95)


#####################
#Behavioral responses of sexually mature sea lamprey exposed to 3sPZS + PZS, 3sPZS, or PZS over Male pheromone
#Fig. 4
#Ovulated females exposed to 
# 1) Vehicle (50% methanol) or 
# 2) 3sPZS and PZS each at 5 x 10-11 M (equivalent of 100x the concentration of 3kPZS in the male pheromone)
# 3) 3sPZS at 1 x 10-10 M (equivalent of 200x the concentration of 3kPZS in the male pheromone)
# 4) PZS at 1 x 10-10 M (equivalent of 200x the concentration of 3kPZS in the male pheromone)  applied 7 m upstream of 
#pheromone activated nest with river water or the full male sex pheromone (Male Pher.) applied to reach a concentration of 3kPZS at 5 x 10-13 M.  

#Description of variables in the CSV data files: 
# 1) Date: Julian date of individual trials.  Number of days since Jan 1st.  0.5 indicates second trial of the day. This is a categorical variable.
# 2) Temp: Average temperature of trial.
# 3) Treatment: MeOH_Riverwater, MeOH_SMW, TriSPZS100X+PZS100X_SMW, TriSPZS200X_SMW, and PZS200X_SMW where SMW = Male pheromone
# 4) Floy: Streamer tag (also called floy tag) color combination on dorsal fin
# 5) Tag: Passive integrated transponder identification tag
#####################

#Response of interest: Remaining in the release cage
#inside = 1 = Yes, remained in release cage
#inside = 0 = No, did not remain in release cage

setwd()

cage<-read.table("cage_antag.over.male.pher.csv", sep=",", header=T)
summary(cage)

cage$Treatment<-factor(cage$Treatment) #change the response variable from continuous to factor
cage$inside<-factor(cage$inside)
summary(cage)

cage$Treatment<-relevel(cage$Treatment, ref="MeOH_Riverwater") #Change reference factor

cageglm<-glm(inside~Treatment, family=binomial, data= cage, na.action=na.omit)

anova(cageglm,test="Chi")#Indicates that the overall logistic regression model for CAGE (remaining in release cage) is significant (X2 = 20.828; df= 4; p= 0.0003425).

summary(cageglm)

#Multiple Comparisons of Means with Tukey Contrasts
install.packages("multcomp")
library("multcomp")

compare <-glht(cageglm, mcp(Treatment="Tukey"))
summary(compare)
summary(compare, test = adjusted("holm"))


#Calculates the expected response on the observed scale of 100x 3sPZS + 100x PZS if effect of 3sPZS and PZS were additive.
#Multiply output by 100 to get %
summary(cageglm)

#-1.8971 represents intercept estimate
#-2.0347 represents 200x PZS estimate
#-0.9708 represents 200x 3sPZS estimate
#Intercept was added to mean of 200x PZS estimate and 200x 3sPZS estimate and inserted into mean equation associated with binomial glm
#We can repeat with other response parameters by replacing intercept and other parameter estimates.  

1 / (1 + exp(-(-1.8971+((-2.0347 -0.9708)/2))))

#Is the observed response different than the expected response?
observed <- c(17, 136) 
expected <- c(0.032, 0.968) #Probabilities that must add up to 1
chisq.test(x=observed, p=expected)


#Response of interest: Downstream movement
#ds = 1 = Yes, moved downstream
#ds = 0 = No, did not move downstream

setwd()

dn<-read.table("dn_antag.over.male.pher.csv", sep=",", header=T)
summary(dn)

dn$Treatment<-factor(dn$Treatment)
dn$ds<-factor(dn$ds)
summary(dn)

dn$Treatment<-relevel(dn$Treatment, ref="MeOH_Riverwater")

dnglm<-glm(ds~Treatment, family=binomial, data= dn, na.action=na.omit)
anova(dnglm,test="Chi")
#Indicates that the overall logistic regression model for DN (moving downstream from the release cage) is significant (X2 = 98.369; df= 4; p= < 2.2e-16).

summary(dnglm)

#Multiple Comparisons of Means with Tukey Contrasts
library("multcomp")
compare <-glht(dnglm, mcp(Treatment="Tukey"))
summary(compare)
summary(compare, test = adjusted("holm"))


#Calculates the expected response on the observed scale of 100x 3sPZS + 100x PZS if effect of 3sPZS and PZS were additive.
#Multiply output by 100 to get %
summary(dnglm)

#0.17435 represents intercept estimate
#0.01855 represents 200x PZS estimate
#-1.60147 represents 200x 3sPZS estimate
#Intercept was added to mean of 200x PZS estimate and 200x 3sPZS estimate and inserted into mean equation associated with binomial glm
#We can repeat with other response parameters by replacing intercept and other parameter estimates.  

1 / (1 + exp(-(0.17435+((0.01855 -1.60147)/2))))

#Is the observed response different than the expected response?
observed <- c(95, 58) 
expected <- c(0.35, 0.65) #Probabilities that must add up to 1
chisq.test(x=observed, p=expected)


#Response of interest: Upstream movement
#us = 1 = Yes, moved upstream
#us = 0 = No, did not move upstream

setwd()

up<-read.table("up_antag.over.male.pher.csv", sep=",", header=T)
summary(up)

up$Treatment<-factor(up$Treatment)
up$us<-factor(up$us)
summary(up)

up$Treatment<-relevel(up$Treatment, ref="MeOH_Riverwater")

upglm<-glm(us~Treatment, family=binomial, data= up, na.action=na.omit)
anova(upglm,test="Chi")
#Indicates that the overall logistic regression model for UP (moving upstream from the release cage) is not significant (X2 = 134.49; df= 4; p< 2.2e-16).

summary(upglm)

#Multiple Comparisons of Means with Tukey Contrasts
#install.packages("multcomp")
library("multcomp")
compare <-glht(upglm, mcp(Treatment="Tukey"))
summary(compare)
summary(compare, test = adjusted("holm"))


#Calculates the expected response on the observed scale of 100x 3sPZS + 100x PZS if effect of 3sPZS and PZS were additive.
#Multiply output by 100 to get %
summary(upglm)

#-0.8267 represents intercept estimate
#0.5558 represents 200x PZS estimate
#1.9397 represents 200x 3sPZS estimate
#Intercept was added to mean of 200x PZS estimate and 200x 3sPZS estimate and inserted into mean equation associated with binomial glm
#We can repeat with other response parameters by replacing intercept and other parameter estimates.  

1 / (1 + exp(-(-0.8267+((0.5558 +1.9397)/2))))

#Is the observed response different than the expected response?
observed <- c(40, 113) 
expected <- c(0.604, 0.396) #Probabilities that must add up to 1
chisq.test(x=observed, p=expected)


#Response of interest: Between upstream antenna (upstream of release cages) and channel choice at confluence
#bw = 1 = Yes, between upstream antenna and channel choice at end of trial
#bw = 0 = No, not between upstream antenna and channel choice at end of trial

setwd()

between<-read.table("between_antag.over.male.pher.csv", sep=",", header=T)
summary(between)

between$Treatment<-factor(between$Treatment)
between$bw<-factor(between$bw)
summary(between)

between$Treatment<-relevel(between$Treatment, ref="MeOH_Riverwater")

betweenglm<-glm(bw~Treatment, family=binomial, data= between, na.action=na.omit)
anova(betweenglm,test="Chi")
# Indicates that the overall logistic regression model for BETWEEN (moving upstream of the release cages, but did not arriving at the channel confluence) is significant (X2 = 14.087; df= 4; p= 0.007024).

summary(betweenglm)

#Multiple Comparisons of Means with Tukey Contrasts
#install.packages("multcomp")
library("multcomp")
compare <-glht(betweenglm, mcp(Treatment="Tukey"))
summary(compare)
summary(compare, test = adjusted("holm"))

#Calculates the expected response on the observed scale of 100x 3sPZS + 100x PZS if effect of 3sPZS and PZS were additive.
#Multiply output by 100 to get %
summary(betweenglm)

#-1.2809 represents intercept estimate
#-0.1542 represents 200x PZS estimate
#0.5390 represents 200x 3sPZS estimate
#Intercept was added to mean of 200x PZS estimate and 200x 3sPZS estimate and inserted into mean equation associated with binomial glm
#We can repeat with other response parameters by replacing intercept and other parameter estimates.  

1 / (1 + exp(-(-1.2809+((-0.1542 +0.5390)/2))))

#Is the observed response different than the expected response?
observed <- c(20, 133) 
expected <- c(0.252, 0.748) #Probabilities that must add up to 1
chisq.test(x=observed, p=expected)


#Response of interest: Enter treatment channel
#trmt = 1 = Yes, entered the activated treatment subchannel compared to the adjacent subchannel with river water
#trmt = 0 = No, did not enter the activated treatment subchannel compared to the adjacent subchannel with river water

setwd()

treatmentchannel<-read.table("treatmentchannel_antag.over.male.pher.csv", sep=",", header=T)
summary(treatmentchannel)

treatmentchannel$Treatment<-factor(treatmentchannel$Treatment)
treatmentchannel$trmt<-factor(treatmentchannel$trmt)
summary(treatmentchannel)

treatmentchannel$Treatment<-relevel(treatmentchannel$Treatment, ref="MeOH_Riverwater")

treatmentchannelglm<-glm(trmt~Treatment, family=binomial, data= treatmentchannel, na.action=na.omit)
anova(treatmentchannelglm,test="Chi")
#Indicates that the overall logistic regression model for TREATMENT CHANNEL (entering the trmt channel (odorant application in this channel)) is significant (X2 = 131.41; df= 4; p< 2.2e-16).

summary(treatmentchannelglm)

#Multiple Comparisons of Means with Tukey Contrasts
#install.packages("multcomp")
library("multcomp")
compare <-glht(treatmentchannelglm, mcp(Treatment="Tukey"))
summary(compare)
summary(compare, test = adjusted("holm"))


#Calculates the expected response on the observed scale of 100x 3sPZS + 100x PZS if effect of 3sPZS and PZS were additive.
#Multiply output by 100 to get %
summary(treatmentchannelglm)

#-3.0910 represents intercept estimate
#1.9405 represents 200x PZS estimate
#2.8096 represents 200x 3sPZS estimate
#Intercept was added to mean of 200x PZS estimate and 200x 3sPZS estimate and inserted into mean equation associated with binomial glm
#We can repeat with other response parameters by replacing intercept and other parameter estimates.  

1 / (1 + exp(-(-3.0910+((1.9405 +2.8096)/2))))

#Is the observed response different than the expected response?
observed <- c(18, 135) 
expected <- c(0.328, 0.672) #Probabilities that must add up to 1
chisq.test(x=observed, p=expected)


#Response of interest: Approaching pheromone source
#app = 1 = Yes, approached the pheromone activated nest by passing through the PIT antenna located 5 m downstream of the pheromone
#app = 0 = No, did not approach the pheromone activated nest by passing through the PIT antenna located 5 m downstream of the pheromone

setwd()

approach<-read.table("approach_antag.over.male.pher.csv", sep=",", header=T)
summary(approach)

approach$Treatment<-factor(approach$Treatment)
approach$app<-factor(approach$app)
summary(approach)

approach$Treatment<-relevel(approach$Treatment, ref="MeOH_Riverwater")

approachglm<-glm(app~Treatment, family=binomial, data= approach, na.action=na.omit)
anova(approachglm,test="Chi")
#Indicates that the overall logistic regression model for APPROACH (moving upstream from the release cage and entering the approaching the pheromone source antenna) is significant (X2 = 120.78; df= 4; p< 2.2e-16).

summary(approachglm)

#Multiple Comparisons of Means with Tukey Contrasts
#install.packages("multcomp")
library("multcomp")
compare <-glht(approachglm, mcp(Treatment="Tukey"))
summary(compare)
summary(compare, test = adjusted("holm"))

#Calculates the expected response on the observed scale of 100x 3sPZS + 100x PZS if effect of 3sPZS and PZS were additive.
#Multiply output by 100 to get %
summary(approachglm)

#-4.511 represents intercept estimate
#3.252 represents 200x PZS estimate
#4.096 represents 200x 3sPZS estimate
#Intercept was added to mean of 200x PZS estimate and 200x 3sPZS estimate and inserted into mean equation associated with binomial glm
#We can repeat with other response parameters by replacing intercept and other parameter estimates.  

1 / (1 + exp(-(-4.511+((3.252 +4.096)/2))))

#Is the observed response different than the expected response?
observed <- c(16, 137) 
expected <- c(0.302, 0.698) #Probabilities that must add up to 1
chisq.test(x=observed, p=expected)


#Response of interest: Enter pheromone nest
#enter = 1 = Yes, entered pheromone nest
#enter = 0 = No, did not enter pheromone nest

setwd()

nest<-read.table("nest_antag.over.male.pher.csv", sep=",", header=T)
summary(nest)

nest$Treatment<-factor(nest$Treatment)
nest$enter<-factor(nest$enter)
summary(nest)

nest$Treatment<-relevel(nest$Treatment, ref="MeOH_Riverwater")

nestglm<-glm(enter~Treatment, family=binomial, data= nest, na.action=na.omit)
anova(nestglm,test="Chi")
#Indicates that the overall logistic regression model for Nest (moving upstream from the release cage and entering the pheromone source antenna) is significant (X2 = 114.7; df= 4; p< 2.2e-16).

summary(nestglm)

#Multiple Comparisons of Means with Tukey Contrasts
#install.packages("multcomp")
library("multcomp")
compare <-glht(nestglm, mcp(Treatment="Tukey"))
summary(compare)
summary(compare, test = adjusted("holm"))

#Calculates the expected response on the observed scale of 100x 3sPZS + 100x PZS if effect of 3sPZS and PZS were additive.
#Multiply output by 100 to get %
summary(nestglm)

#-3.8067 represents intercept estimate
#2.5477 represents 200x PZS estimate
#3.3922 represents 200x 3sPZS estimate
#Intercept was added to mean of 200x PZS estimate and 200x 3sPZS estimate and inserted into mean equation associated with binomial glm
#We can repeat with other response parameters by replacing intercept and other parameter estimates.  
1 / (1 + exp(-(-3.8067+((2.5477 +3.3922)/2))))

#Is the observed response different than the expected response?
observed <- c(15, 138) 
expected <- c(0.302, 0.698) #Probabilities that must add up to 1
chisq.test(x=observed, p=expected)


#####################
#Carp Lake Outlet 2019 unplugged ovulated females 
#Fig. 5A-E
# Vehicle (50% methanol) or 3sPZS and PZS (each at 6.2 x 10-12 M) were applied over the sea lamprey spawning grounds in 2019
#####################

setwd()

library(lme4) #for lmer & glmer models
library(lmerTest) # to generate p-values for your mixed models
library(sjPlot) #for plotting lmer and glmer mods
library(sjmisc) 
library(effects)
library(sjstats) #use for r2 functions
library(cowplot) #for manuscript ready figures
library(bbmle) #use to calculate AICc
library(MASS)
library(ggplot2)
library(brglm)

unplugged_19<-read.csv("Females_unplugged_2019_20221201.csv")
summary(unplugged_19)
unplugged_19$Treatment<-factor(unplugged_19$Treatment)
unplugged_19$Year<-factor(unplugged_19$Year)
unplugged_19$JulianDate<-factor(unplugged_19$JulianDate)
unplugged_19$Cage<-factor(unplugged_19$Cage)
unplugged_19$Upstream<-factor(unplugged_19$Upstream)
unplugged_19$Downstream<-factor(unplugged_19$Downstream)
unplugged_19$Nest<-factor(unplugged_19$Nest)
unplugged_19$Mating<-factor(unplugged_19$Mating)
unplugged_19$FishID<-factor(unplugged_19$FishID)
summary(unplugged_19)

unplugged_19$Treatment<-relevel(unplugged_19$Treatment, ref="Control") #Change my reference factor for "Treatment" to the level "Control" so that when I look at the summary output of models "Control" is the reference.



#####################
# Mating glms 2019
# Separate analysis for each year
# Include JulianDate as random effect
#####################


glm.mate.m1 <- glmer (Mating~ Treatment + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.mate.m2 <- glmer (Mating~ Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.mate.m3 <- glmer (Mating~ Treatment + Temp + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.mate.m4 <- glmer (Mating~ Treatment + Temp + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.mate.m5 <- glmer (Mating~ Treatment + Temp + tkPZS_ng_L + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.mate.m6 <- glmer (Mating~ Treatment + Temp + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.mate.m7 <- glmer (Mating~ Treatment + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.mate.m8 <- glmer (Mating~ Treatment + GaugeHt + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.mate.m9 <- glmer (Mating~ Treatment + PZS_ng_L + (1 | JulianDate) , family= binomial, data= unplugged_19, na.action=na.omit)

glm.mate.m10 <- glmer (Mating~ Treatment + tkPZS_ng_L + PZS_ng_L + (1 | JulianDate) , family= binomial, data= unplugged_19, na.action=na.omit)

glm.mate.m11 <- glmer (Mating~ Treatment + tkPZS_ng_L + DKPES_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.mate.m12 <- glmer (Mating~ Temp * Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.mate.m13 <- glmer (Mating~ GaugeHt + Temp * Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.mate.m14 <- glmer (Mating~ Temp * Treatment  + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)




Select_model_mate <- list("m1" = glm.mate.m1, "m2" = glm.mate.m2, "m3" =  glm.mate.m3, "m4" = glm.mate.m4, 
                          "m5" = glm.mate.m5, "m6" = glm.mate.m6, "m7" = glm.mate.m7, "m8" = glm.mate.m8, 
                          "m9" = glm.mate.m9, "m10" = glm.mate.m10, "m11" = glm.mate.m11, 
                          "m13" = glm.mate.m13, "m14" = glm.mate.m14)


AICctab(Select_model_mate, base = TRUE)

# AICc dAICc dfb <old>
# m1  67.9  0.0  3 
# m7  69.4  1.4  4 
# m2  69.9  2.0  4 
# m13 70.0  2.1  5 
# m11 70.2  2.3  4 
# m4  70.3  2.4  4 
# m8  71.1  3.1  5 
# m6  71.6  3.6  5 
# m16 72.1  4.2  5 
# m10 72.3  4.3  3 
# m3  72.4  4.5  5 
# m9  72.8  4.9  3 
# m5  73.3  5.4  6 
# m14 74.3  6.4  6 
# m15 74.6  6.7  7 

#AICc dAICc df <new>
#m1  67.8  0.0  3 
#m7  69.4  1.6  4 
#m2  69.8  2.0  4 
#m11 70.0  2.2  5 
#m9  70.1  2.3  4 
#m4  70.2  2.4  4 
#m8  71.1  3.3  5 
#m6  71.6  3.8  5 
#m10 71.9  4.1  5 
#m14 72.0  4.2  5 
#m3  72.3  4.5  5 
#m5  73.3  5.5  6 
#m13 74.6  6.8  7 


summary(glm.mate.m1)


#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept)              0.5022     0.5071   0.990   0.3220  
#TreatmentAnt_6.21E-12M  -1.4886     0.7294  -2.041   0.0413 *



#########################################################
#PLOT

glm.mate.m1 <- glmer (Mating~ Treatment + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

effects_Trt <- effect(term = c("Treatment"), mod = glm.mate.m1) #Using the top model 
summary(effects_Trt)


x_Trt <- as.data.frame(effects_Trt)
x_Trt

ggplot(data = x_Trt, aes(x= Treatment, y = fit ))+
  geom_point(data = x_Trt, aes(x = Treatment, y = fit), size = 3.5)+
  geom_errorbar(data = x_Trt, aes(x = Treatment, ymin = fit-se, ymax = fit+se),
                alpha = 0.7, width = 0.2 )+
  labs(x = "Treatment", y = "Spawn (%)")+
  ylim(min = 0, max = 1)+
  theme_classic()+
  theme(text=element_text(size= 15))


# Treatment       fit         se     lower     upper
#1       Control 0.6229717 0.1190960 0.37951 0.8169757
#2 Ant_6.21E-12M 0.2716201 0.1000754 0.12155 0.5012493


###########################################################



#####################
# Finds a nest glms 2019
# Each includes Year as fixed effect
# Include JulianDate as random effect
#####################

unplugged_19$Treatment<-relevel(unplugged_19$Treatment, ref="Control")

glm.Nest.m1 <- glmer (Nest~ Treatment + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Nest.m2 <- glmer (Nest~ Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Nest.m3 <- glmer (Nest~ Treatment + Temp + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Nest.m4 <- glmer (Nest~ Treatment + Temp + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Nest.m5 <- glmer (Nest~ Treatment + Temp + tkPZS_ng_L + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Nest.m6 <- glmer (Nest~ Treatment + Temp + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Nest.m7 <- glmer (Nest~ Treatment + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Nest.m8 <- glmer (Nest~ Treatment + GaugeHt + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Nest.m9 <- glmer (Nest~ Treatment + PZS_ng_L + (1 | JulianDate) , family= binomial, data= unplugged_19, na.action=na.omit)

glm.Nest.m10 <- glmer (Nest~ Treatment + tkPZS_ng_L + PZS_ng_L + (1 | JulianDate) , family= binomial, data= unplugged_19, na.action=na.omit)

glm.Nest.m11 <- glmer (Nest~ Treatment + tkPZS_ng_L + DKPES_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Nest.m12 <- glmer (Nest~ Temp * Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Nest.m13 <- glmer (Nest~ GaugeHt + Temp * Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Nest.m14 <- glmer (Nest~ Temp * Treatment  + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)


Select_model_Nest <- list("m1" = glm.Nest.m1, "m2" = glm.Nest.m2, "m3" =  glm.Nest.m3, "m4" = glm.Nest.m4, 
                          "m5" = glm.Nest.m5, "m6" = glm.Nest.m6, "m7" = glm.Nest.m7, "m8" = glm.Nest.m8, 
                          "m9" = glm.Nest.m9, "m10" = glm.Nest.m10, "m11" = glm.Nest.m11, "m12" = glm.Nest.m12,
                          "m13" = glm.Nest.m13, "m14" = glm.Nest.m14)

AICctab(Select_model_Nest, base= TRUE)

summary(glm.Nest.m2)

# AICc dAICc df
# m2  65.4  0.0  4 #Doesn't make sense; dropped; 3kPZS concentration is negatively associated with probability of finding an occupied nest
# m1  65.8  0.4  3 
# m3  67.2  1.8  5 
# m11 67.5  2.1  5 
# m7  67.7  2.3  4 
# m4  67.7  2.3  4 
# m10 67.9  2.5  5 
# m8  67.9  2.5  5 
# m9  68.0  2.5  4 
# m6  68.8  3.4  5 
# m5  69.4  4.0  6 
# m12 69.8  4.4  6 
# m14 70.2  4.8  5 
# m13 72.2  6.7  7 

#Re-ran after dropped m2 for reasons stated above

Select_model_Nest <- list("m1" = glm.Nest.m1, #"m2" = glm.Nest.m2, 
                          "m3" =  glm.Nest.m3, "m4" = glm.Nest.m4, 
                          "m5" = glm.Nest.m5, "m6" = glm.Nest.m6, "m7" = glm.Nest.m7, "m8" = glm.Nest.m8, 
                          "m9" = glm.Nest.m9, "m10" = glm.Nest.m10, "m11" = glm.Nest.m11, "m12" = glm.Nest.m12,
                          "m13" = glm.Nest.m13, "m14" = glm.Nest.m14)

AICctab(Select_model_Nest, base= TRUE)

# AICc dAICc df
# m1  65.8  0.0  3 
# m3  67.2  1.4  5 
# m11 67.5  1.7  5 
# m7  67.7  1.9  4 
# m4  67.7  1.9  4 
# m10 67.9  2.1  5 
# m8  67.9  2.1  5 
# m9  68.0  2.2  4 
# m6  68.8  3.0  5 
# m5  69.4  3.6  6 
# m12 69.8  4.0  6 
# m14 70.2  4.4  5 
# m13 72.2  6.4  7 

summary(glm.Nest.m1)

# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept)              1.4469     0.5557   2.604  0.00922 **
#   TreatmentAnt_6.21E-12M  -1.4469     0.6721  -2.153  0.03132 * 


#########################################################
#PLOT

glm.Nest.m1 <- glmer (Nest~ Treatment + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

effects_Trt <- effect(term = c("Treatment"), mod = glm.Nest.m1) #Using the top model 
summary(effects_Trt)


x_Trt <- as.data.frame(effects_Trt)
x_Trt

ggplot(data = x_Trt, aes(x= Treatment, y = fit ))+
  geom_point(data = x_Trt, aes(x = Treatment, y = fit), size = 3.5)+
  geom_errorbar(data = x_Trt, aes(x = Treatment, ymin = fit-se, ymax = fit+se),
                alpha = 0.7, width = 0.2 )+
  labs(x = "Treatment", y = "Nest (%)")+
  ylim(min = 0, max = 1)+
  theme_classic()+
  theme(text=element_text(size= 15))


# Treatment       fit         se     lower     upper
# 1       Control 0.8095238 0.08568880 0.5884917 0.9266344
# 2 Ant_6.21E-12M 0.5000000 0.09449104 0.3228301 0.6771699


###########################################################



#####################
# Upstream glms 2019
# Separate analysis for each year
# Include JulianDate as random effect
#####################

unplugged_19$Treatment<-relevel(unplugged_19$Treatment, ref="Control")

glm.Upstream.m1 <- glmer (Upstream~ Treatment + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Upstream.m2 <- glmer (Upstream~ Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Upstream.m3 <- glmer (Upstream~ Treatment + Temp + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Upstream.m4 <- glmer (Upstream~ Treatment + Temp + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Upstream.m5 <- glmer (Upstream~ Treatment + Temp + tkPZS_ng_L + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Upstream.m6 <- glmer (Upstream~ Treatment + Temp + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Upstream.m7 <- glmer (Upstream~ Treatment + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Upstream.m8 <- glmer (Upstream~ Treatment + GaugeHt + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Upstream.m9 <- glmer (Upstream~ Treatment + PZS_ng_L + (1 | JulianDate) , family= binomial, data= unplugged_19, na.action=na.omit)

glm.Upstream.m10 <- glmer (Upstream~ Treatment + tkPZS_ng_L + PZS_ng_L + (1 | JulianDate) , family= binomial, data= unplugged_19, na.action=na.omit)

glm.Upstream.m11 <- glmer (Upstream~ Treatment + tkPZS_ng_L + DKPES_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Upstream.m12 <- glmer (Upstream~ Temp * Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Upstream.m13 <- glmer (Upstream~ GaugeHt + Temp * Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Upstream.m14 <- glmer (Upstream~ Temp * Treatment  + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)


Select_model_Upstream <- list("m1" = glm.Upstream.m1, "m2" = glm.Upstream.m2, "m3" =  glm.Upstream.m3, "m4" = glm.Upstream.m4, 
                              "m5" = glm.Upstream.m5, "m6" = glm.Upstream.m6, "m7" = glm.Upstream.m7, "m8" = glm.Upstream.m8, 
                              "m9" = glm.Upstream.m9, "m10" = glm.Upstream.m10, "m11" = glm.Upstream.m11, "m12" = glm.Upstream.m12,
                              "m13" = glm.Upstream.m13, "m14" = glm.Upstream.m14)

AICctab(Select_model_Upstream, base= TRUE)

# AICc dAICc df
# m1  49.7  0.0  3 
# m7  51.8  2.1  4 
# m2  51.9  2.2  4 
# m9  52.0  2.2  4 
# m4  52.1  2.3  4 
# m11 53.8  4.1  5 
# m6  53.9  4.2  5 
# m10 54.1  4.4  5 
# m8  54.2  4.5  5 
# m3  54.4  4.7  5 
# m14 54.5  4.8  5 
# m5  56.5  6.8  6 
# m12 56.9  7.2  6 
# m13 59.0  9.4  7 


summary(glm.Upstream.m1)

# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept)               2.996      1.025   2.924  0.00346 **
#   TreatmentAnt_6.21E-12M   -2.249      1.102  -2.041  0.04126 * 


#########################################################
#PLOT

glm.Upstream.m1 <- glmer (Upstream~ Treatment + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

effects_Trt <- effect(term = c("Treatment"), mod = glm.Upstream.m1) #Using the top model 
summary(effects_Trt)


x_Trt <- as.data.frame(effects_Trt)
x_Trt

ggplot(data = x_Trt, aes(x= Treatment, y = fit ))+
  geom_point(data = x_Trt, aes(x = Treatment, y = fit), size = 3.5)+
  geom_errorbar(data = x_Trt, aes(x = Treatment, ymin = fit-se, ymax = fit+se),
                alpha = 0.7, width = 0.2 )+
  labs(x = "Treatment", y = "Upstream (%)")+
  ylim(min = 0, max = 1)+
  theme_classic()+
  theme(text=element_text(size= 15))


# Treatment       fit         se     lower     upper
# 1       Control 0.9523809 0.04647145 0.7285675 0.9933343
# 2 Ant_6.21E-12M 0.6785714 0.08825941 0.4885301 0.8235107

###########################################################




#####################
# Downstream glms 2019
# Separate analysis for each year
# Include JulianDate as random effect
#####################

unplugged_19$Treatment<-relevel(unplugged_19$Treatment, ref="Control")

glm.Downstream.m1 <- glmer (Downstream~ Treatment + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Downstream.m2 <- glmer (Downstream~ Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Downstream.m3 <- glmer (Downstream~ Treatment + Temp + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Downstream.m4 <- glmer (Downstream~ Treatment + Temp + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Downstream.m5 <- glmer (Downstream~ Treatment + Temp + tkPZS_ng_L + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Downstream.m6 <- glmer (Downstream~ Treatment + Temp + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Downstream.m7 <- glmer (Downstream~ Treatment + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Downstream.m8 <- glmer (Downstream~ Treatment + GaugeHt + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Downstream.m9 <- glmer (Downstream~ Treatment + PZS_ng_L + (1 | JulianDate) , family= binomial, data= unplugged_19, na.action=na.omit)

glm.Downstream.m10 <- glmer (Downstream~ Treatment + tkPZS_ng_L + PZS_ng_L + (1 | JulianDate) , family= binomial, data= unplugged_19, na.action=na.omit)

glm.Downstream.m11 <- glmer (Downstream~ Treatment + tkPZS_ng_L + DKPES_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Downstream.m12 <- glmer (Downstream~ Temp * Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Downstream.m13 <- glmer (Downstream~ GaugeHt + Temp * Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Downstream.m14 <- glmer (Downstream~ Temp * Treatment  + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)


Select_model_Downstream <- list("m1" = glm.Downstream.m1, "m2" = glm.Downstream.m2, "m3" =  glm.Downstream.m3, "m4" = glm.Downstream.m4, 
                                "m5" = glm.Downstream.m5, "m6" = glm.Downstream.m6, "m7" = glm.Downstream.m7, "m8" = glm.Downstream.m8, 
                                "m9" = glm.Downstream.m9, "m10" = glm.Downstream.m10, "m11" = glm.Downstream.m11, "m12" = glm.Downstream.m12,
                                "m13" = glm.Downstream.m13, "m14" = glm.Downstream.m14)

AICctab(Select_model_Downstream, base= TRUE)


#AICc dAICc df
#m7  30.7  0.0  4 
#m6  31.1  0.5  5 
#m9  32.8  2.1  4 #Doesn't make sense; Fewer go downstream as [PZS] increases
#m8  33.1  2.5  5 
#m5  33.4  2.7  6 
#m1  33.6  3.0  3 
#m11 34.3  3.6  5 
#m10 34.3  3.7  5 
#m4  36.0  5.3  4 
#m2  36.0  5.3  4 
#m13 36.1  5.4  7 
#m3  38.4  7.8  5 
#m14 38.4  7.8  5 
#m12 41.0 10.3  6 

summary(glm.Downstream.m7)

# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)            -112.2019    66.0576  -1.699   0.0894 .
# TreatmentAnt_6.21E-12M    0.9162     1.2644   0.725   0.4687  
# GaugeHt                  78.3724    47.0522   1.666   0.0958 .


#########################################################
#PLOT

glm.Downstream.m7 <- glmer (Downstream~ Treatment + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

effects_Trt <- effect(term = c("Treatment"), mod = glm.Downstream.m7) #Using the top model 
summary(effects_Trt)


x_Trt <- as.data.frame(effects_Trt)
x_Trt

ggplot(data = x_Trt, aes(x= Treatment, y = fit ))+
  geom_point(data = x_Trt, aes(x = Treatment, y = fit), size = 3.5)+
  geom_errorbar(data = x_Trt, aes(x = Treatment, ymin = fit-se, ymax = fit+se),
                alpha = 0.7, width = 0.2 )+
  labs(x = "Treatment", y = "Downstream (%)")+
  ylim(min = 0, max = 1)+
  theme_classic()+
  theme(text=element_text(size= 15))


# Treatment        fit         se        lower     upper
# 1       Control 0.01561262 0.02442077 0.0007038246 0.2631612
# 2 Ant_6.21E-12M 0.03813362 0.04453133 0.0036575120 0.2998000

###########################################################




#####################
# Stay in cage glms 2019
# Separate analysis for each year
# Include JulianDate as random effect
#####################

unplugged_19$Treatment<-relevel(unplugged_19$Treatment, ref="Control")

glm.Cage.m1 <- glmer (Cage~ Treatment + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Cage.m2 <- glmer (Cage~ Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Cage.m3 <- glmer (Cage~ Treatment + Temp + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Cage.m4 <- glmer (Cage~ Treatment + Temp + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Cage.m5 <- glmer (Cage~ Treatment + Temp + tkPZS_ng_L + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Cage.m6 <- glmer (Cage~ Treatment + Temp + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Cage.m7 <- glmer (Cage~ Treatment + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Cage.m8 <- glmer (Cage~ Treatment + GaugeHt + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Cage.m9 <- glmer (Cage~ Treatment + PZS_ng_L + (1 | JulianDate) , family= binomial, data= unplugged_19, na.action=na.omit)

glm.Cage.m10 <- glmer (Cage~ Treatment + tkPZS_ng_L + PZS_ng_L + (1 | JulianDate) , family= binomial, data= unplugged_19, na.action=na.omit)

glm.Cage.m11 <- glmer (Cage~ Treatment + tkPZS_ng_L + DKPES_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Cage.m12 <- glmer (Cage~ Temp * Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Cage.m13 <- glmer (Cage~ GaugeHt + Temp * Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)

glm.Cage.m14 <- glmer (Cage~ Temp * Treatment  + (1 | JulianDate), family= binomial, data= unplugged_19, na.action=na.omit)


Select_model_Cage <- list("m1" = glm.Cage.m1, "m2" = glm.Cage.m2, "m3" =  glm.Cage.m3, "m4" = glm.Cage.m4, 
                          "m5" = glm.Cage.m5, "m6" = glm.Cage.m6, "m7" = glm.Cage.m7, "m8" = glm.Cage.m8, 
                          "m9" = glm.Cage.m9, "m10" = glm.Cage.m10, "m11" = glm.Cage.m11, "m12" = glm.Cage.m12,
                          "m13" = glm.Cage.m13, "m14" = glm.Cage.m14)

AICctab(Select_model_Cage, base= TRUE)

summary(glm.Cage.m1) #Results don't make sense
#ridiculously high SE for estimates and p = 0.990 for Treatment_6.21E-12M vs MeOH control; likely due to complete separation
#Needs to be ran as brglm instead


#####################
# Stay in cage brglms 2019 because high SE for estimates; likely due to complete separation
# Separate analysis for each year
# JulianDate as random effect removed because brglm only works for fixed effects
#####################
unplugged_19$Treatment<-relevel(unplugged_19$Treatment, ref="Control")

glm.Cage.m1 <- brglm (Cage~ Treatment, family= binomial, data= unplugged_19, na.action=na.omit)

glm.Cage.m2 <- brglm (Cage~ Treatment + tkPZS_ng_L, family= binomial, data= unplugged_19, na.action=na.omit)

glm.Cage.m3 <- brglm (Cage~ Treatment + Temp + tkPZS_ng_L, family= binomial, data= unplugged_19, na.action=na.omit)

glm.Cage.m4 <- brglm (Cage~ Treatment + Temp, family= binomial, data= unplugged_19, na.action=na.omit)

glm.Cage.m5 <- brglm (Cage~ Treatment + Temp + tkPZS_ng_L + GaugeHt, family= binomial, data= unplugged_19, na.action=na.omit)

glm.Cage.m6 <- brglm (Cage~ Treatment + Temp + GaugeHt, family= binomial, data= unplugged_19, na.action=na.omit)

glm.Cage.m7 <- brglm (Cage~ Treatment + GaugeHt, family= binomial, data= unplugged_19, na.action=na.omit)

glm.Cage.m8 <- brglm (Cage~ Treatment + GaugeHt + tkPZS_ng_L, family= binomial, data= unplugged_19, na.action=na.omit)

glm.Cage.m9 <- brglm (Cage~ Treatment + PZS_ng_L, family= binomial, data= unplugged_19, na.action=na.omit)

glm.Cage.m10 <- brglm (Cage~ Treatment + tkPZS_ng_L + PZS_ng_L, family= binomial, data= unplugged_19, na.action=na.omit)

glm.Cage.m11 <- brglm (Cage~ Treatment + tkPZS_ng_L + DKPES_ng_L, family= binomial, data= unplugged_19, na.action=na.omit)

glm.Cage.m12 <- brglm (Cage~ Temp * Treatment + tkPZS_ng_L, family= binomial, data= unplugged_19, na.action=na.omit)

glm.Cage.m13 <- brglm (Cage~ GaugeHt + Temp * Treatment + tkPZS_ng_L, family= binomial, data= unplugged_19, na.action=na.omit)

glm.Cage.m14 <- brglm (Cage~ Temp * Treatment, family= binomial, data= unplugged_19, na.action=na.omit)


Select_model_Cage <- list("m1" = glm.Cage.m1, "m2" = glm.Cage.m2, "m3" =  glm.Cage.m3, "m4" = glm.Cage.m4, 
                          "m5" = glm.Cage.m5, "m6" = glm.Cage.m6, "m7" = glm.Cage.m7, "m8" = glm.Cage.m8, 
                          "m9" = glm.Cage.m9, "m10" = glm.Cage.m10, "m11" = glm.Cage.m11, "m12" = glm.Cage.m12,
                          "m13" = glm.Cage.m13, "m14" = glm.Cage.m14)

AICctab(Select_model_Cage, base= TRUE)

# AICc dAICc df
# m1  34.3  0.0  2 
# m8  34.9  0.5  4 
# m7  35.5  1.1  3 
# m9  36.1  1.7  3 
# m2  36.4  2.0  3 
# m4  36.5  2.2  3 
# m5  36.8  2.5  5 
# m6  37.9  3.5  4 
# m10 38.5  4.1  4 
# m3  38.7  4.3  4 
# m11 39.0  4.7  4 
# m14 39.8  5.4  4 
# m13 40.4  6.0  6 
# m12 42.0  7.7  5 

summary(glm.Cage.m1)

# Coefficients:
#                           Estimate Std. Error z value Pr(>|z|)  
# (Intercept)              -3.761      1.464  -2.569   0.0102 *
#   TreatmentAnt_6.21E-12M    2.519      1.533   1.644   0.1002

##Suspect error bars on Antagonist treatment are large, hence not sig effect of treatment (p > 0.05).  
#If the error bar is not large, then need to re-visit this model.



#########################################################
#PLOT

glm.Cage.m1 <- brglm (Cage~ Treatment, family= binomial, data= unplugged_19, na.action=na.omit)

effects_Trt <- effect(term = c("Treatment"), mod = glm.Cage.m1) #Using the top model 
summary(effects_Trt)


x_Trt <- as.data.frame(effects_Trt)
x_Trt

ggplot(data = x_Trt, aes(x= Treatment, y = fit ))+
  geom_point(data = x_Trt, aes(x = Treatment, y = fit), size = 3.5)+
  geom_errorbar(data = x_Trt, aes(x = Treatment, ymin = fit-se, ymax = fit+se),
                alpha = 0.7, width = 0.2 )+
  labs(x = "Treatment", y = "Cage (%)")+
  ylim(min = 0, max = 1)+
  theme_classic()+
  theme(text=element_text(size= 15))


# Treatment        fit         se        lower     upper
#1       Control 0.02272727 0.03252160 0.001317061 0.2908281
#2 Ant_6.21E-12M 0.22413793 0.07880811 0.106221614 0.4125349

###########################################################


#####################
#Carp Lake Outlet 2019 change in abundance of sea lamprey
#Fig. 5F
# Vehicle (50% methanol) or 3sPZS and PZS (each at 6.2 x 10-12 M) were applied over the sea lamprey spawning grounds in 2019
#####################


setwd()

library(lme4) #for lmer & glmer models
library(lmerTest) # to generate p-values for your mixed models
library(sjPlot) #for plotting lmer and glmer mods
library(sjmisc) 
library(effects)
library(sjstats) #use for r2 functions
library(cowplot) #for manuscript ready figures
library(bbmle) #use to calculate AICc
library(MASS)
library(ggplot2)
library(brglm)

atlarge_19<-read.csv("At_Large_2019.csv")
summary(atlarge_19)

atlarge_19$Treatment<-factor(atlarge_19$Treatment)
atlarge_19$Year<-factor(atlarge_19$Year)
atlarge_19$JulianDate<-factor(atlarge_19$JulianDate)
summary(atlarge_19)

#####################
# At large total (males + females) total 2019  DIFFnumbermalesandfemales
# Separate analysis for each year
#####################


atlarge_19$Treatment<-relevel(atlarge_19$Treatment, ref="Control") #Change my reference factor for "Treatment" to the level "Control" so that when I look at the summary output of models "Control" is the reference.


lm.diffatlargetotal.m1 <- lm (DIFFnumbermalesandfemales ~ Treatment, data= atlarge_19, na.action=na.omit)

lm.diffatlargetotal.m2 <- lm (DIFFnumbermalesandfemales ~ Treatment + tkPZS_ng_L, data= atlarge_19, na.action=na.omit)

lm.diffatlargetotal.m3 <- lm (DIFFnumbermalesandfemales ~ Treatment + Temp + tkPZS_ng_L, data= atlarge_19, na.action=na.omit)

lm.diffatlargetotal.m4 <- lm (DIFFnumbermalesandfemales ~ Treatment + Temp, data= atlarge_19, na.action=na.omit)

lm.diffatlargetotal.m5 <- lm (DIFFnumbermalesandfemales ~ Treatment + Temp + tkPZS_ng_L + GaugeHt, data= atlarge_19, na.action=na.omit)

lm.diffatlargetotal.m6 <- lm (DIFFnumbermalesandfemales ~ Treatment + Temp + GaugeHt, data= atlarge_19, na.action=na.omit)

lm.diffatlargetotal.m7 <- lm (DIFFnumbermalesandfemales ~ Treatment + GaugeHt, data= atlarge_19, na.action=na.omit)

lm.diffatlargetotal.m8 <- lm (DIFFnumbermalesandfemales ~ Treatment + GaugeHt + tkPZS_ng_L, data= atlarge_19, na.action=na.omit)

lm.diffatlargetotal.m9 <- lm (DIFFnumbermalesandfemales ~ Treatment + PZS_ng_L, data= atlarge_19, na.action=na.omit)

lm.diffatlargetotal.m10 <- lm (DIFFnumbermalesandfemales ~ Treatment + tkPZS_ng_L + PZS_ng_L, data= atlarge_19, na.action=na.omit)

lm.diffatlargetotal.m11 <- lm (DIFFnumbermalesandfemales ~ Treatment + tkPZS_ng_L + DKPES_ng_L, data= atlarge_19, na.action=na.omit)

lm.diffatlargetotal.m12 <- lm (DIFFnumbermalesandfemales ~ Temp * Treatment + tkPZS_ng_L, data= atlarge_19, na.action=na.omit)

lm.diffatlargetotal.m13 <- lm (DIFFnumbermalesandfemales ~ GaugeHt + Temp * Treatment + tkPZS_ng_L, data= atlarge_19, na.action=na.omit)

lm.diffatlargetotal.m14 <- lm (DIFFnumbermalesandfemales ~ Temp * Treatment, data= atlarge_19, na.action=na.omit)


Select_model_mate <- list("m1" = lm.diffatlargetotal.m1, "m2" = lm.diffatlargetotal.m2, "m3" =  lm.diffatlargetotal.m3, "m4" = lm.diffatlargetotal.m4, 
                          "m5" = lm.diffatlargetotal.m5, 
                          "m6" = lm.diffatlargetotal.m6, "m7" = lm.diffatlargetotal.m7, "m8" = lm.diffatlargetotal.m8, 
                          "m9" = lm.diffatlargetotal.m9, "m10" = lm.diffatlargetotal.m10, "m11" = lm.diffatlargetotal.m11, "m12" = lm.diffatlargetotal.m12,
                          "m13" = lm.diffatlargetotal.m13, 
                          "m14" = lm.diffatlargetotal.m14)



AICctab(Select_model_mate, base = TRUE)

#AICc  dAICc df
#m1   72.2   0.0 3 
#m9   72.8   0.7 4 
#m7   76.5   4.3 4 
#m2   76.6   4.4 4 
#m4   76.9   4.7 4 
#m10  79.0   6.9 5 
#m14  81.3   9.1 5 
#m8   82.1  10.0 5 
#m6   82.6  10.5 5 
#m11  82.9  10.7 5 
#m3   82.9  10.7 5 
#m12  89.7  17.5 6 
#m5   90.8  18.6 6 
#m13 101.0  28.8 7 

summary(lm.diffatlargetotal.m1)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)               6.200      1.647   3.764   0.0037 **
#   TreatmentAnt_6.21E-12M   -5.057      2.157  -2.345   0.0410 * 

#########################################################
#PLOT

lm.diffatlargetotal.m1 <- lm (DIFFnumbermalesandfemales ~ Treatment, data= atlarge_19, na.action=na.omit)


effects_Trt <- effect(term = c("Treatment"), mod = lm.diffatlargetotal.m1) #Using the top model 
summary(effects_Trt)


x_Trt <- as.data.frame(effects_Trt)
x_Trt

ggplot(data = x_Trt, aes(x= Treatment, y = fit ))+
  geom_point(data = x_Trt, aes(x = Treatment, y = fit), size = 3.5)+
  geom_errorbar(data = x_Trt, aes(x = Treatment, ymin = fit-se, ymax = fit+se),
                alpha = 0.7, width = 0.2 )+
  labs(x = "Treatment", y = "Difference in # At large male + female")+
  ylim(min = -1, max = 10)+
  theme_classic()+
  theme(text=element_text(size= 15))


# Treatment      fit       se     lower    upper
# 1       Control 6.200000 1.647162  2.529894 9.870106
# 2 Ant_6.21E-12M 1.142857 1.392106 -1.958948 4.244663

###########################################################


#####################
#Carp Lake Outlet 2020 unplugged ovulated females 
#Fig. 5G-K
# Vehicle (50% methanol) or 3sPZS and PZS (each at 6.2 x 10-11 M) were applied over the sea lamprey spawning grounds in 2020
#####################

setwd()

library(lme4) #for lmer & glmer models
library(lmerTest) # to generate p-values for your mixed models
library(sjPlot) #for plotting lmer and glmer mods
library(sjmisc) 
library(effects)
library(sjstats) #use for r2 functions
library(cowplot) #for manuscript ready figures
library(bbmle) #use to calculate AICc
library(MASS)
library(ggplot2)
library(brglm)

unplugged_20<-read.csv("Females_unplugged_2020.csv")
summary(unplugged_20)

unplugged_20$Treatment<-factor(unplugged_20$Treatment)
unplugged_20$Year<-factor(unplugged_20$Year)
unplugged_20$JulianDate<-factor(unplugged_20$JulianDate)
unplugged_20$Cage<-factor(unplugged_20$Cage)
unplugged_20$Upstream<-factor(unplugged_20$Upstream)
unplugged_20$Downstream<-factor(unplugged_20$Downstream)
unplugged_20$Nest<-factor(unplugged_20$Nest)
unplugged_20$Mating<-factor(unplugged_20$Mating)
unplugged_20$FishID<-factor(unplugged_20$FishID)
summary(unplugged_20)

unplugged_20$Treatment<-relevel(unplugged_20$Treatment, ref="Control") #Change my reference factor for "Treatment" to the level "Control" so that when I look at the summary output of models "Control" is the reference.


#####################
# Mating glms 2020
# Separate analysis for each year
# Include JulianDate as random effect
#####################

glm.mate.m1 <- glmer (Mating~ Treatment + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.mate.m2 <- glmer (Mating~ Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.mate.m3 <- glmer (Mating~ Treatment + Temp + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.mate.m4 <- glmer (Mating~ Treatment + Temp + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.mate.m5 <- glmer (Mating~ Treatment + Temp + tkPZS_ng_L + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.mate.m6 <- glmer (Mating~ Treatment + Temp + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.mate.m7 <- glmer (Mating~ Treatment + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.mate.m8 <- glmer (Mating~ Treatment + GaugeHt + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.mate.m9 <- glmer (Mating~ Treatment + PZS_ng_L + (1 | JulianDate) , family= binomial, data= unplugged_20, na.action=na.omit)

glm.mate.m10 <- glmer (Mating~ Treatment + tkPZS_ng_L + PZS_ng_L + (1 | JulianDate) , family= binomial, data= unplugged_20, na.action=na.omit)

glm.mate.m11 <- glmer (Mating~ Treatment + tkPZS_ng_L + DKPES_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.mate.m12 <- glmer (Mating~ Temp * Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.mate.m13 <- glmer (Mating~ GaugeHt + Temp * Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.mate.m14 <- glmer (Mating~ Temp * Treatment  + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)




Select_model_mate <- list("m1" = glm.mate.m1, "m2" = glm.mate.m2, "m3" =  glm.mate.m3, "m4" = glm.mate.m4, 
                          #"m5" = glm.mate.m5, 
                          "m6" = glm.mate.m6, "m7" = glm.mate.m7, #"m8" = glm.mate.m8, 
                          "m9" = glm.mate.m9, "m10" = glm.mate.m10, "m11" = glm.mate.m11, "m12" = glm.mate.m12,
                          #"m13" = glm.mate.m13, 
                          "m14" = glm.mate.m14)



AICctab(Select_model_mate, base = TRUE)

# AICc dAICc df
# m8  44.9  0.0  5 #Remove; doesn't make sense that GaugeHt is strongly positively associated with increased spawning
# m3  46.0  1.1  5 
# m1  46.6  1.7  3 
# m2  47.0  2.1  4 
# m12 48.5  3.6  6 
# m11 48.8  3.9  5 
# m4  48.9  4.0  4 
# m7  48.9  4.0  4 
# m9  48.9  4.0  4 
# m10 49.2  4.3  5 
# m14 50.6  5.7  5 
# m6  50.7  5.8  5 


#Re-ran AICctab after removing m8 for reasons above

# AICc dAICc df
# m3  46.0  0.0  5 
# m1  46.6  0.6  3 
# m2  47.0  0.9  4 
# m12 48.5  2.5  6 
# m11 48.8  2.7  5 
# m4  48.9  2.9  4 
# m7  48.9  2.9  4 
# m9  48.9  2.9  4 
# m10 49.2  3.1  5 
# m14 50.6  4.6  5 
# m6  50.7  4.7  5 

summary(glm.mate.m3)

# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)              3.1344     4.5179   0.694   0.4878  
# TreatmentAnt_6.21E-11M  -4.1802     1.8125  -2.306   0.0211 *
# Temp                    -0.4847     0.2878  -1.684   0.0922 .
# tkPZS_ng_L               0.8565     0.4478   1.913   0.0558 .



#########################################################
#PLOT

glm.mate.m3 <- glmer (Mating~ Treatment + Temp + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

effects_Trt <- effect(term = c("Treatment"), mod = glm.mate.m3) #Using the top model 
summary(effects_Trt)


x_Trt <- as.data.frame(effects_Trt)
x_Trt

ggplot(data = x_Trt, aes(x= Treatment, y = fit ))+
  geom_point(data = x_Trt, aes(x = Treatment, y = fit), size = 3.5)+
  geom_errorbar(data = x_Trt, aes(x = Treatment, ymin = fit-se, ymax = fit+se),
                alpha = 0.7, width = 0.2 )+
  labs(x = "Treatment", y = "Spawn (%)")+
  ylim(min = 0, max = 1)+
  theme_classic()+
  theme(text=element_text(size= 15))


# Treatment       fit         se        lower     upper
# 1       Control 0.4911824 0.14368435 0.2382878981 0.7486718
# 2 Ant_6.21E-11M 0.0145498 0.02150894 0.0007797855 0.2183460


###########################################################


#####################
# Finds a nest glms 2020
# Each includes Year as fixed effect
# Include JulianDate as random effect
#####################

unplugged_20$Treatment<-relevel(unplugged_20$Treatment, ref="Control")

glm.Nest.m1 <- glmer (Nest~ Treatment + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Nest.m2 <- glmer (Nest~ Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Nest.m3 <- glmer (Nest~ Treatment + Temp + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Nest.m4 <- glmer (Nest~ Treatment + Temp + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Nest.m5 <- glmer (Nest~ Treatment + Temp + tkPZS_ng_L + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Nest.m6 <- glmer (Nest~ Treatment + Temp + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Nest.m7 <- glmer (Nest~ Treatment + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Nest.m8 <- glmer (Nest~ Treatment + GaugeHt + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Nest.m9 <- glmer (Nest~ Treatment + PZS_ng_L + (1 | JulianDate) , family= binomial, data= unplugged_20, na.action=na.omit)

glm.Nest.m10 <- glmer (Nest~ Treatment + tkPZS_ng_L + PZS_ng_L + (1 | JulianDate) , family= binomial, data= unplugged_20, na.action=na.omit)

glm.Nest.m11 <- glmer (Nest~ Treatment + tkPZS_ng_L + DKPES_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Nest.m12 <- glmer (Nest~ Temp * Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Nest.m13 <- glmer (Nest~ GaugeHt + Temp * Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Nest.m14 <- glmer (Nest~ Temp * Treatment  + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)


Select_model_Nest <- list("m1" = glm.Nest.m1, "m2" = glm.Nest.m2, "m3" =  glm.Nest.m3, "m4" = glm.Nest.m4, 
                          "m5" = glm.Nest.m5, "m6" = glm.Nest.m6, "m7" = glm.Nest.m7, "m8" = glm.Nest.m8, 
                          "m9" = glm.Nest.m9, "m10" = glm.Nest.m10, "m11" = glm.Nest.m11, "m12" = glm.Nest.m12,
                          "m13" = glm.Nest.m13, "m14" = glm.Nest.m14)

AICctab(Select_model_Nest, base= TRUE)

# AICc dAICc df
# m9  70.7  0.0  4 #Doesn't make sense; probability of finding a nest is positively associated with antagonist application (compared to MEOH)
#SE >> Parameter estimate for  TreatmentAnt_6.21E-11M  Estimate: 0.07878    SE: 0.74687
# m10 70.8  0.1  5 #Doesn't make sense; probability of finding a nest is positively associated with antagonist application (compared to MEOH)
#SE >> Parameter estimate for  TreatmentAnt_6.21E-11M  Estimate: 0.27231    SE: 0.78433
# m1  71.0  0.3  3 
# m3  71.8  1.0  5 
# m4  71.9  1.2  4 
# m8  72.3  1.5  5 
# m7  72.6  1.9  4 
# m2  73.2  2.5  4 
# m6  73.3  2.6  5 
# m12 73.6  2.9  6 
# m5  74.3  3.6  6 
# m14 74.4  3.6  5 
# m11 75.5  4.8  5 
# m13 76.1  5.3  7 

summary(glm.Nest.m9)
summary(glm.Nest.m10)
summary(glm.Nest.m1)


#Re-ran AICctab after removing m9 and m10 for reasons above
Select_model_Nest <- list("m1" = glm.Nest.m1, "m2" = glm.Nest.m2, "m3" =  glm.Nest.m3, "m4" = glm.Nest.m4, 
                          "m5" = glm.Nest.m5, "m6" = glm.Nest.m6, "m7" = glm.Nest.m7, "m8" = glm.Nest.m8, 
                          #"m9" = glm.Nest.m9, 
                          #"m10" = glm.Nest.m10, 
                          "m11" = glm.Nest.m11, "m12" = glm.Nest.m12,
                          "m13" = glm.Nest.m13, "m14" = glm.Nest.m14)

AICctab(Select_model_Nest, base= TRUE)

# AICc dAICc df
# m1  71.0  0.0  3 
# m3  71.8  0.8  5 
# m4  71.9  0.9  4 
# m8  72.3  1.2  5 
# m7  72.6  1.6  4 
# m2  73.2  2.2  4 
# m6  73.3  2.3  5 
# m12 73.6  2.6  6 
# m5  74.3  3.3  6 
# m14 74.4  3.3  5 
# m11 75.5  4.5  5 
# m13 76.1  5.0  7 

summary(glm.Nest.m1)

# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)             -0.3178     0.4848  -0.656    0.512
# TreatmentAnt_6.21E-11M  -0.6755     0.6414  -1.053    0.292



#########################################################
#PLOT

glm.Nest.m1 <- glmer (Nest~ Treatment + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

effects_Trt <- effect(term = c("Treatment"), mod = glm.Nest.m1) #Using the top model 
summary(effects_Trt)


x_Trt <- as.data.frame(effects_Trt)
x_Trt

ggplot(data = x_Trt, aes(x= Treatment, y = fit ))+
  geom_point(data = x_Trt, aes(x = Treatment, y = fit), size = 3.5)+
  geom_errorbar(data = x_Trt, aes(x = Treatment, ymin = fit-se, ymax = fit+se),
                alpha = 0.7, width = 0.2 )+
  labs(x = "Treatment", y = "Nest (%)")+
  ylim(min = 0, max = 1)+
  theme_classic()+
  theme(text=element_text(size= 15))


# Treatment       fit         se     lower     upper
# 1       Control 0.4212027 0.11818092 0.2196072 0.6530064
# 2 Ant_6.21E-11M 0.2702514 0.08260667 0.1401183 0.4570095

###########################################################


#####################
# Upstream glms 2020
# Separate analysis for each year
# Include JulianDate as random effect
#####################

unplugged_20$Treatment<-relevel(unplugged_20$Treatment, ref="Control")

glm.Upstream.m1 <- glmer (Upstream~ Treatment + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Upstream.m2 <- glmer (Upstream~ Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Upstream.m3 <- glmer (Upstream~ Treatment + Temp + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Upstream.m4 <- glmer (Upstream~ Treatment + Temp + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Upstream.m5 <- glmer (Upstream~ Treatment + Temp + tkPZS_ng_L + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Upstream.m6 <- glmer (Upstream~ Treatment + Temp + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Upstream.m7 <- glmer (Upstream~ Treatment + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Upstream.m8 <- glmer (Upstream~ Treatment + GaugeHt + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Upstream.m9 <- glmer (Upstream~ Treatment + PZS_ng_L + (1 | JulianDate) , family= binomial, data= unplugged_20, na.action=na.omit)

glm.Upstream.m10 <- glmer (Upstream~ Treatment + tkPZS_ng_L + PZS_ng_L + (1 | JulianDate) , family= binomial, data= unplugged_20, na.action=na.omit)

glm.Upstream.m11 <- glmer (Upstream~ Treatment + tkPZS_ng_L + DKPES_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Upstream.m12 <- glmer (Upstream~ Temp * Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Upstream.m13 <- glmer (Upstream~ GaugeHt + Temp * Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Upstream.m14 <- glmer (Upstream~ Temp * Treatment  + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)


Select_model_Upstream <- list("m1" = glm.Upstream.m1, "m2" = glm.Upstream.m2, "m3" =  glm.Upstream.m3, "m4" = glm.Upstream.m4, 
                              "m5" = glm.Upstream.m5, "m6" = glm.Upstream.m6, "m7" = glm.Upstream.m7, "m8" = glm.Upstream.m8, 
                              "m9" = glm.Upstream.m9, "m10" = glm.Upstream.m10, "m11" = glm.Upstream.m11, "m12" = glm.Upstream.m12,
                              "m13" = glm.Upstream.m13, "m14" = glm.Upstream.m14)

AICctab(Select_model_Upstream, base= TRUE)

# AICc dAICc df
# m1  77.2  0.0  3 
# m2  78.9  1.7  4 
# m9  79.1  1.9  4 
# m7  79.3  2.1  4 
# m4  79.5  2.3  4 
# m11 80.3  3.1  5 
# m10 81.3  4.0  5 
# m3  81.3  4.1  5 
# m8  81.3  4.1  5 
# m6  81.4  4.2  5 
# m14 81.8  4.6  5 
# m12 83.1  5.9  6 
# m5  83.8  6.6  6 
# m13 85.8  8.6  7 


summary(glm.Upstream.m1)

# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)              0.5390     0.4756   1.133    0.257
# TreatmentAnt_6.21E-11M  -0.4784     0.5895  -0.811    0.417


#########################################################
#PLOT

glm.Upstream.m1 <- glmer (Upstream~ Treatment + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

effects_Trt <- effect(term = c("Treatment"), mod = glm.Upstream.m1) #Using the top model 
summary(effects_Trt)


x_Trt <- as.data.frame(effects_Trt)
x_Trt

ggplot(data = x_Trt, aes(x= Treatment, y = fit ))+
  geom_point(data = x_Trt, aes(x = Treatment, y = fit), size = 3.5)+
  geom_errorbar(data = x_Trt, aes(x = Treatment, ymin = fit-se, ymax = fit+se),
                alpha = 0.7, width = 0.2 )+
  labs(x = "Treatment", y = "Upstream (%)")+
  ylim(min = 0, max = 1)+
  theme_classic()+
  theme(text=element_text(size= 15))


# Treatment       fit         se     lower     upper
# 1       Control 0.6315789 0.11066467 0.4029588 0.8132313
# 2 Ant_6.21E-11M 0.5151515 0.08699884 0.3493129 0.6777192

###########################################################


#####################
# Downstream glms 2020
# Separate analysis for each year
# Include JulianDate as random effect
#####################

unplugged_20$Treatment<-relevel(unplugged_20$Treatment, ref="Control")

glm.Downstream.m1 <- glmer (Downstream~ Treatment + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Downstream.m2 <- glmer (Downstream~ Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Downstream.m3 <- glmer (Downstream~ Treatment + Temp + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Downstream.m4 <- glmer (Downstream~ Treatment + Temp + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Downstream.m5 <- glmer (Downstream~ Treatment + Temp + tkPZS_ng_L + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Downstream.m6 <- glmer (Downstream~ Treatment + Temp + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Downstream.m7 <- glmer (Downstream~ Treatment + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Downstream.m8 <- glmer (Downstream~ Treatment + GaugeHt + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Downstream.m9 <- glmer (Downstream~ Treatment + PZS_ng_L + (1 | JulianDate) , family= binomial, data= unplugged_20, na.action=na.omit)

glm.Downstream.m10 <- glmer (Downstream~ Treatment + tkPZS_ng_L + PZS_ng_L + (1 | JulianDate) , family= binomial, data= unplugged_20, na.action=na.omit)

glm.Downstream.m11 <- glmer (Downstream~ Treatment + tkPZS_ng_L + DKPES_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Downstream.m12 <- glmer (Downstream~ Temp * Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Downstream.m13 <- glmer (Downstream~ GaugeHt + Temp * Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Downstream.m14 <- glmer (Downstream~ Temp * Treatment  + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)


Select_model_Downstream <- list("m1" = glm.Downstream.m1, "m2" = glm.Downstream.m2, "m3" =  glm.Downstream.m3, "m4" = glm.Downstream.m4, 
                                "m5" = glm.Downstream.m5, "m6" = glm.Downstream.m6, "m7" = glm.Downstream.m7, "m8" = glm.Downstream.m8, 
                                "m9" = glm.Downstream.m9, "m10" = glm.Downstream.m10, "m11" = glm.Downstream.m11, "m12" = glm.Downstream.m12,
                                "m13" = glm.Downstream.m13, "m14" = glm.Downstream.m14)

AICctab(Select_model_Downstream, base= TRUE)

# AICc dAICc df
# m1  56.5  0.0  3 
# m4  57.8  1.3  4 
# m9  57.9  1.4  4 
# m7  57.9  1.5  4 
# m2  58.8  2.3  4 
# m8  59.6  3.2  5 
# m3  59.9  3.4  5 
# m14 60.2  3.7  5 
# m10 60.2  3.7  5 
# m6  60.3  3.8  5 
# m11 61.1  4.7  5 
# m5  62.2  5.7  6 
# m12 62.4  5.9  6 
# m13 64.8  8.3  7 

summary(glm.Downstream.m1)

# Fixed effects:
#                           Estimate Std. Error z value Pr(>|z|)  
# (Intercept)             -1.0296     0.5210  -1.976   0.0481 *
#   TreatmentAnt_6.21E-11M  -0.6931     0.7121  -0.973   0.3304  

#########################################################
#PLOT

glm.Downstream.m1 <- glmer (Downstream~ Treatment + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

effects_Trt <- effect(term = c("Treatment"), mod = glm.Downstream.m1) #Using the top model 
summary(effects_Trt)


x_Trt <- as.data.frame(effects_Trt)
x_Trt

ggplot(data = x_Trt, aes(x= Treatment, y = fit ))+
  geom_point(data = x_Trt, aes(x = Treatment, y = fit), size = 3.5)+
  geom_errorbar(data = x_Trt, aes(x = Treatment, ymin = fit-se, ymax = fit+se),
                alpha = 0.7, width = 0.2 )+
  labs(x = "Treatment", y = "Downstream (%)")+
  ylim(min = 0, max = 1)+
  theme_classic()+
  theme(text=element_text(size= 15))


# Treatment       fit         se      lower     upper
# 1       Control 0.2631579 0.10102252 0.11397801 0.4978744
# 2 Ant_6.21E-11M 0.1515152 0.06241557 0.06450478 0.3162204

###########################################################



#####################
# Stay in cage glms 2020
# Separate analysis for each year
# Include JulianDate as random effect
#####################

unplugged_20$Treatment<-relevel(unplugged_20$Treatment, ref="Control")

glm.Cage.m1 <- glmer (Cage~ Treatment + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Cage.m2 <- glmer (Cage~ Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Cage.m3 <- glmer (Cage~ Treatment + Temp + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Cage.m4 <- glmer (Cage~ Treatment + Temp + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Cage.m5 <- glmer (Cage~ Treatment + Temp + tkPZS_ng_L + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Cage.m6 <- glmer (Cage~ Treatment + Temp + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Cage.m7 <- glmer (Cage~ Treatment + GaugeHt + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Cage.m8 <- glmer (Cage~ Treatment + GaugeHt + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Cage.m9 <- glmer (Cage~ Treatment + PZS_ng_L + (1 | JulianDate) , family= binomial, data= unplugged_20, na.action=na.omit)

glm.Cage.m10 <- glmer (Cage~ Treatment + tkPZS_ng_L + PZS_ng_L + (1 | JulianDate) , family= binomial, data= unplugged_20, na.action=na.omit)

glm.Cage.m11 <- glmer (Cage~ Treatment + tkPZS_ng_L + DKPES_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Cage.m12 <- glmer (Cage~ Temp * Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Cage.m13 <- glmer (Cage~ GaugeHt + Temp * Treatment + tkPZS_ng_L + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

glm.Cage.m14 <- glmer (Cage~ Temp * Treatment  + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)


Select_model_Cage <- list("m1" = glm.Cage.m1, "m2" = glm.Cage.m2, "m3" =  glm.Cage.m3, "m4" = glm.Cage.m4, 
                          "m5" = glm.Cage.m5, "m6" = glm.Cage.m6, "m7" = glm.Cage.m7, "m8" = glm.Cage.m8, 
                          "m9" = glm.Cage.m9, "m10" = glm.Cage.m10, "m11" = glm.Cage.m11, "m12" = glm.Cage.m12,
                          "m13" = glm.Cage.m13, "m14" = glm.Cage.m14)

AICctab(Select_model_Cage, base= TRUE)

# AICc dAICc df
# m1  68.1  0.0  3 
# m2  68.6  0.5  4 
# m7  69.5  1.4  4 
# m4  70.1  2.1  4 
# m10 70.3  2.2  5 
# m9  70.4  2.3  4 
# m6  70.5  2.4  5 
# m3  70.8  2.7  5 
# m12 70.9  2.9  6 
# m8  71.0  2.9  5 
# m11 71.0  3.0  5 
# m14 71.8  3.7  5 
# m5  72.8  4.8  6 
# m13 73.6  5.5  7 

summary(glm.Cage.m1)

# Fixed effects:
#                          Estimate  Std. Error z value Pr(>|z|)  
# (Intercept)             -1.3218     0.5627  -2.349   0.0188 *
#   TreatmentAnt_6.21E-11M   0.6286     0.6731   0.934   0.3503  


#########################################################
#PLOT

glm.Cage.m1 <- glmer (Cage~ Treatment + (1 | JulianDate), family= binomial, data= unplugged_20, na.action=na.omit)

effects_Trt <- effect(term = c("Treatment"), mod = glm.Cage.m1) #Using the top model 
summary(effects_Trt)


x_Trt <- as.data.frame(effects_Trt)
x_Trt

ggplot(data = x_Trt, aes(x= Treatment, y = fit ))+
  geom_point(data = x_Trt, aes(x = Treatment, y = fit), size = 3.5)+
  geom_errorbar(data = x_Trt, aes(x = Treatment, ymin = fit-se, ymax = fit+se),
                alpha = 0.7, width = 0.2 )+
  labs(x = "Treatment", y = "Cage (%)")+
  ylim(min = 0, max = 1)+
  theme_classic()+
  theme(text=element_text(size= 15))


# Treatment       fit        se      lower     upper
# 1       Control 0.2105263 0.0935288 0.08130927 0.4455117
# 2 Ant_6.21E-11M 0.3333333 0.0820610 0.19514616 0.5076538

###########################################################


#####################
#Carp Lake Outlet 2020 change in abundance of sea lamprey
#Fig. 5L
# Vehicle (50% methanol) or 3sPZS and PZS (each at 6.2 x 10-11 M) were applied over the sea lamprey spawning grounds in 2019
#####################
setwd()

atlarge_20_no0<-read.csv("At_Large_2020_no0atstart.csv")
summary(atlarge_20_no0)

atlarge_20_no0$Treatment<-factor(atlarge_20_no0$Treatment)
atlarge_20_no0$Year<-factor(atlarge_20_no0$Year)
atlarge_20_no0$JulianDate<-factor(atlarge_20_no0$JulianDate)
summary(atlarge_20_no0)

#####################
# At large total (males + females) total 2020  DIFFnumbermalesandfemales
# Separate analysis for each year
#####################

atlarge_20_no0$Treatment<-relevel(atlarge_20_no0$Treatment, ref="Control") #Change my reference factor for "Treatment" to the level "Control" so that when I look at the summary output of models "Control" is the reference.


lm.diffatlargetotal.m1 <- lm (DIFFnumbermalesandfemales ~ Treatment, data= atlarge_20_no0, na.action=na.omit)

lm.diffatlargetotal.m2 <- lm (DIFFnumbermalesandfemales ~ Treatment + tkPZS_ng_L, data= atlarge_20_no0, na.action=na.omit)

lm.diffatlargetotal.m3 <- lm (DIFFnumbermalesandfemales ~ Treatment + Temp + tkPZS_ng_L, data= atlarge_20_no0, na.action=na.omit)

lm.diffatlargetotal.m4 <- lm (DIFFnumbermalesandfemales ~ Treatment + Temp, data= atlarge_20_no0, na.action=na.omit)

lm.diffatlargetotal.m5 <- lm (DIFFnumbermalesandfemales ~ Treatment + Temp + tkPZS_ng_L + GaugeHt, data= atlarge_20_no0, na.action=na.omit)

lm.diffatlargetotal.m6 <- lm (DIFFnumbermalesandfemales ~ Treatment + Temp + GaugeHt, data= atlarge_20_no0, na.action=na.omit)

lm.diffatlargetotal.m7 <- lm (DIFFnumbermalesandfemales ~ Treatment + GaugeHt, data= atlarge_20_no0, na.action=na.omit)

lm.diffatlargetotal.m8 <- lm (DIFFnumbermalesandfemales ~ Treatment + GaugeHt + tkPZS_ng_L, data= atlarge_20_no0, na.action=na.omit)

lm.diffatlargetotal.m9 <- lm (DIFFnumbermalesandfemales ~ Treatment + PZS_ng_L, data= atlarge_20_no0, na.action=na.omit)

lm.diffatlargetotal.m10 <- lm (DIFFnumbermalesandfemales ~ Treatment + tkPZS_ng_L + PZS_ng_L, data= atlarge_20_no0, na.action=na.omit)

lm.diffatlargetotal.m11 <- lm (DIFFnumbermalesandfemales ~ Treatment + tkPZS_ng_L + DKPES_ng_L, data= atlarge_20_no0, na.action=na.omit)

lm.diffatlargetotal.m12 <- lm (DIFFnumbermalesandfemales ~ Temp * Treatment + tkPZS_ng_L, data= atlarge_20_no0, na.action=na.omit)

lm.diffatlargetotal.m13 <- lm (DIFFnumbermalesandfemales ~ GaugeHt + Temp * Treatment + tkPZS_ng_L, data= atlarge_20_no0, na.action=na.omit)

lm.diffatlargetotal.m14 <- lm (DIFFnumbermalesandfemales ~ Temp * Treatment, data= atlarge_20_no0, na.action=na.omit)


Select_model_mate <- list("m1" = lm.diffatlargetotal.m1, "m2" = lm.diffatlargetotal.m2, "m3" =  lm.diffatlargetotal.m3, "m4" = lm.diffatlargetotal.m4, 
                          "m5" = lm.diffatlargetotal.m5, 
                          "m6" = lm.diffatlargetotal.m6, "m7" = lm.diffatlargetotal.m7, "m8" = lm.diffatlargetotal.m8, 
                          "m9" = lm.diffatlargetotal.m9, "m10" = lm.diffatlargetotal.m10, "m11" = lm.diffatlargetotal.m11, "m12" = lm.diffatlargetotal.m12,
                          "m13" = lm.diffatlargetotal.m13, 
                          "m14" = lm.diffatlargetotal.m14)



AICctab(Select_model_mate, base = TRUE)

# AICc  dAICc df
# m2   52.3   0.0 4 
# m1   53.8   1.5 3 
# m9   57.9   5.7 4 
# m7   60.1   7.9 4 
# m4   60.5   8.2 4 
# m11  68.6  16.3 5 
# m10  68.7  16.4 5 
# m8   69.2  16.9 5 
# m3   70.7  18.4 5 
# m14  76.0  23.7 5 
# m6   78.8  26.5 5 
# m5  118.8  66.6 6 
# m12 125.6  73.3 6 
# m13   Inf   Inf 7 

summary(lm.diffatlargetotal.m2)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)             -8.1269     3.1721  -2.562  0.05052 . 
# TreatmentAnt_6.21E-11M  -7.1089     1.6103  -4.415  0.00693 **
#   tkPZS_ng_L               1.2597     0.3314   3.801  0.01261 * 

median(atlarge_20_no0$tkPZS_ng_L)


#########################################################
#PLOT


lm.diffatlargetotal.m2 <- lm (DIFFnumbermalesandfemales ~ Treatment + tkPZS_ng_L, data= atlarge_20_no0, na.action=na.omit)


effects_Trt <- effect(term = c("Treatment"), mod = lm.diffatlargetotal.m2) #Using the top model 
summary(effects_Trt)


x_Trt <- as.data.frame(effects_Trt)
x_Trt

ggplot(data = x_Trt, aes(x= Treatment, y = fit ))+
  geom_point(data = x_Trt, aes(x = Treatment, y = fit), size = 3.5)+
  geom_errorbar(data = x_Trt, aes(x = Treatment, ymin = fit-se, ymax = fit+se),
                alpha = 0.7, width = 0.2 )+
  labs(x = "Treatment", y = "Difference in # At large male + female")+
  ylim(min = -5, max = 10)+
  theme_classic()+
  theme(text=element_text(size= 15))


# Treatment       fit        se      lower      upper
# 1       Control  4.068082 1.2543305  0.8437227  7.2924408
# 2 Ant_6.21E-11M -3.040849 0.9618004 -5.5132356 -0.5684625

###########################################################



#####################
#Carp Lake Outlet 2019 unplugged vs naris plugged ovulated females 
#Fig. S3
#Only compared when Vehicle (50% methanol) was applied over the sea lamprey spawning grounds in 2019
#####################

install.packages("brglm")
library(lme4) #for lmer & glmer models
library(lmerTest) # to generate p-values for your mixed models
library(sjPlot) #for plotting lmer and glmer mods
library(sjmisc) 
library(effects)
library(sjstats) #use for r2 functions
library(cowplot) #for manuscript ready figures
library(bbmle) #use to calculate AICc
library(MASS)
library(brglm)

setwd()

MeOHtrials <-read.csv("Females_MeOHtrials_2019.csv")
summary(MeOHtrials)
View(MeOHtrials)

#Change some variables from continuous to factor.
MeOHtrials$Plugged<-factor(MeOHtrials$Plugged)
MeOHtrials$Treatment<-factor(MeOHtrials$Treatment)
MeOHtrials$Cage<-factor(MeOHtrials$Cage)
MeOHtrials$Upstream<-factor(MeOHtrials$Upstream)
MeOHtrials$Downstream<-factor(MeOHtrials$Downstream)
MeOHtrials$Nest<-factor(MeOHtrials$Nest)
MeOHtrials$Mating<-factor(MeOHtrials$Mating)
MeOHtrials$FishID<-factor(MeOHtrials$FishID)
summary(MeOHtrials)

#Response of interest: Spawned (mating)
#mating = 1 = Yes, spawned
#mating = 0 = No, did not spawn
#Main question of interest: Is the probability that a released female mates different if she is nose plugged or not plugged? 

#Response variables evaluated with generalized linear models with binomial responses (brglm)
#Response variable has complete separation for the binary-response model so the model  assigns unrealistically large parameter estimates & ridiculouslylarge Wald Standard Errors
#Read Ben Bolker's suggestions on website "http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#penalizationhandling-complete-separation"
#under topic "Penalization/handling complete separation"
#Summary of Suggestion: Use brglm packge to re-run models (https://cran.r-project.org/web/packages/brglm/brglm.pdf)
#brglm can only run fixed models

glm.mate <- brglm (Mating~ Plugged, family=binomial, data= MeOHtrials, na.action=na.omit)
anova(glm.mate,test="Chi")
#Results: Proportion of released females that mate is different if the nose is plugged or not (X2 = 7.3033, df= 1, p=  0.006883 **)
#No released females mated if the nose was plugged.


#Response of interest: Enter a male occupied nest
#nest = 1 = Yes, enterd a male occupied nest
#nest = 0 = No, did not a male occupied nest
##Response variables evaluated with generalized linear models with binomial responses (brglm)

glm.nest <- brglm (Nest~ Plugged, family=binomial, data= MeOHtrials, na.action=na.omit)
anova(glm.nest,test="Chi")
#Results: Proportion of released females that finds an occupied nest is lower if nose plugged compared to unplugged (X2 = 9.0465, df= 1, p= 0.002632 **)


#Response of interest: Upstream movement
#upstream = 1 = Yes, moved upstream
#upstream = 0 = No, did not move upstream
##Response variables evaluated with generalized linear models with binomial responses (brglm)

glm.up <- brglm (Upstream~ Plugged, family=binomial, data= MeOHtrials, na.action=na.omit)
anova(glm.up,test="Chi")
#Results: Proportion of released females that swim upstream is lower if the nose plugged compared to unplugged (X2 = 18.35, df= 1, p= 1.839e-05 **)


#Response of interest: Downstream movement
#downstream = 1 = Yes, moved downstream
#downstream = 0 = No, did not move downstream
##Response variables evaluated with generalized linear models with binomial responses (brglm) for same reasons explained above for mating

glm.down <- brglm (Downstream~ Plugged, family=binomial, data= MeOHtrials, na.action=na.omit)
anova(glm.down,test="Chi")
#Results: Proportion of released females that swim downstream is higher if nose plugged compared to unplugged (X2 = 6.0544, df= 1, p= 0.01387 **)

#Response of interest: Remaining in the release cage
#cage = 1 = Yes, remained in release cage
#cage = 0 = No, did not remain in release cage
##Response variables evaluated with generalized linear models with binomial responses (brglm) for same reasons explained above for mating

glm.cage <- brglm (Cage~ Plugged, family=binomial, data= MeOHtrials, na.action=na.omit)
anova(glm.cage,test="Chi")
#Results: Proportion of released females that remains in the release cage is higher if nose plugged compared to unplugged though not significant (X2 = 2.337, df= 1, p=  0.1263)
