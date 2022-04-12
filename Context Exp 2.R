rm(list=ls(all=TRUE))

library(nlme)    #Non-linear mixed effects package
library(car)   #Regression package
library(lme4)    #Linear mixed effects package
library(ggplot2)   #Advanced plotting package
library(languageR)  ##Packages containing useful functions for language research
library(lattice)  #need to download and install!
library(lmerTest)
library(effects)

CS = read.table("~/OneDrive - University of Iowa/TempHist_Context/Context Manuscript/Mixed Effects Analysis.csv", header = TRUE, sep = ",")
CS = na.omit(CS)

CS$RT_LN = log(CS$CorrSearchRT)

CS$Subject <- as.factor(CS$Subject) ##Subject as random factor
CS$ItemNumber <- as.factor(CS$ItemNumber) ##ItemNumber (category) as random factor
CS$Condition <- as.factor(CS$Condition) ##Condition as fixed factor
CS$Task <- as.factor(CS$Task) ##Task as fixed factor
CS$ArtNat <- as.factor(CS$ArtNat)  ##ArtNat as fixed factor

options(contrasts=c("contr.sum", "contr.poly"))  #Effect coding for two fixed factors
contrasts(CS$Condition)
contrasts(CS$ArtNat)
contrasts(CS$Task)

summary(CS)
names(CS)
attach(CS)

##Need Subject as a random effect
boxplot(CorrSearchRT ~ Subject, data = CS, xlab = "subject", ylab = "RT")

##Need ItemNumber (category) as a random effect
boxplot(CorrSearchRT ~ ItemNumber, data = CS, xlab = "category", ylab = "RT")



############################################
##Model1: S and I intercepts plus slopes for each of the FE and their interactions. Note that there is no slope for ArtNat by Item, since items appear in one or other of these conditions. That is, Items cannot differ in the magnitude of their Art/Nat effect, since each item is EITHER Art or Nat. Exactly the same goes for Task by Ss. Each subject is only in one task, so subs cannot differ on the variable task.
model1 <- lmer(CorrSearchRT ~ 1 + Task * Condition * ArtNat + (1 + Condition * ArtNat | Subject) + (1 + Task * Condition | ItemNumber), data = CS,
control=lmerControl(optimizer="bobyqa"))
summary(model1)

##Warning: Singular fit --> one or more variances are very close to 0

############################################
##Model2: Start by simplifying Item RE structure. Remove interaction.
model2 <- lmer(CorrSearchRT ~ 1 + Task * Condition * ArtNat + (1 + Condition * ArtNat | Subject) + (1 + Task + Condition | ItemNumber), data = CS,
control=lmerControl(optimizer="bobyqa"))
summary(model2)

##Warning: Singular fit 

anova(model2, model1, refit = FALSE)
## No Sig reduction in fit -- ELIMINATE Task * Condition slope for items


#############################################
##REDUCTION: model3 eliminates BOTH Task and Condition slopes for Items
##Model3
model3 <- lmer(CorrSearchRT ~ 1 + Task * Condition * ArtNat + (1 + Condition * ArtNat | Subject) + (1 | ItemNumber), data = CS,
control=lmerControl(optimizer="bobyqa"))
summary(model3)

##Warning: Singular fit 

anova(model3, model2, refit = FALSE)
## No Sig reduction in fit -- ELIMINATE Task and Condition slopes for items


#############################################
##REDUCTION: model4 eliminates Condition * ArtNat slope for Ss
##Model4
model4 <- lmer(CorrSearchRT ~ 1 + Task * Condition * ArtNat + (1 + Condition + ArtNat | Subject) + (1 | ItemNumber), data = CS,
control=lmerControl(optimizer="bobyqa"))
summary(model4)

##Warning: Singular fit 

anova(model4, model3, refit = FALSE)
## No Sig reduction in fit -- ELIMINATE Condition * ArtNat slope for Ss


#############################################
##REDUCTION: model5 eliminates ArtNat slope for Ss
##Model5
model5 <- lmer(CorrSearchRT ~ 1 + Task * Condition * ArtNat + (1 + Condition | Subject) + (1 | ItemNumber), data = CS,
control=lmerControl(optimizer="bobyqa"))
summary(model5)

##Successful convergence -- perhaps suggests that ArtNat slope was responsible for convergence failure

anova(model5, model4, refit = FALSE)
## Sig reduction in fit -- Retain ArtNat slope for the moment


#############################################
##REDUCTION: model6 eliminates Condition slope for Ss
##Model6
model6 <- lmer(CorrSearchRT ~ 1 + Task * Condition * ArtNat + (1 + ArtNat | Subject) + (1 | ItemNumber), data = CS,
control=lmerControl(optimizer="bobyqa"))
summary(model6)

##Warning: Singular fit. Likely culprit is ArtNat slope. Note also that there is a value of 1.0 for ArtNat Correlation. Again indicates convergence error.

anova(model6, model4, refit = FALSE)
## No Sig reduction in fit -- ELIMINATE Condiiton slope for S



#############################################
##REDUCTION: model7 eliminates ArtNaT slope to eliminate convergence issue
##Model7
model7 <- lmer(CorrSearchRT ~ 1 + Task * Condition * ArtNat  + (1 | Subject) + (1 | ItemNumber), data = CS,
control=lmerControl(optimizer="bobyqa"))
summary(model7)

##Successful convergence

anova(model7, model6, refit = FALSE)
##Sig Reduction in fit, BUT, model with ArtNat slope fails to converge, so even with reduction, drop ArtNat slope


###########
##Final Model is model7 with S and I intercepts.
summary(model7)
anova(model7)

#############################################
##Follow up analysis: simple effects of task
sub_rating <- subset(CS, CS$Task == "Ranking")
sub_classification <- subset(CS, CS$Task == "Classification")

model8 <- lmer(CorrSearchRT ~ 1 + Condition * ArtNat  + (1 | Subject) + (1 | ItemNumber), data = sub_rating,
control=lmerControl(optimizer="bobyqa"))
summary(model8)
anova(model8)

model9 <- lmer(CorrSearchRT ~ 1 + Condition * ArtNat  + (1 | Subject) + (1 | ItemNumber), data = sub_classification,
control=lmerControl(optimizer="bobyqa"))
summary(model9)
anova(model9)



