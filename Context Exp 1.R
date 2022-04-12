rm(list=ls(all=TRUE))

library(nlme)    #Non-linear mixed effects package
library(car)   #Regression package
library(lme4)    #Linear mixed effects package
library(ggplot2)   #Advanced plotting package
library(languageR)  ##Packages containing useful functions for language research
library(lattice)  #need to download and install!
library(lmerTest)
library(effects)

CS = read.table("~/OneDrive - University of Iowa/TempHist_Context/Experiments/Exp 5/Mixed Effects Analysis Exp 1.csv", header = TRUE, sep = ",")
CS = na.omit(CS)

CS$RT_LN = log(CS$CorrSearchRT)

CS$Subject <- as.factor(CS$Subject) ##Subject as random factor
CS$ItemNumber <- as.factor(CS$ItemNumber) ##ItemNumber (category) as random factor
CS$Condition <- as.factor(CS$Condition) ##Condition as fixed factor
CS$ArtNat <- as.factor(CS$ArtNat)  ##ArtNat as fixed factor

options(contrasts=c("contr.sum", "contr.poly"))  #Effect coding for two fixed factors
contrasts(CS$Condition)
contrasts(CS$ArtNat)

summary(CS)
names(CS)
attach(CS)

##Need Subject as a random effect
boxplot(CorrSearchRT ~ Subject, data = CS, xlab = "subject", ylab = "RT")

##Need ItemNumber (category) as a random effect
boxplot(CorrSearchRT ~ ItemNumber, data = CS, xlab = "category", ylab = "RT")


############################################
##Model1: S and I intercepts plus slopes for each of the FE and their interactions. Note that there is no slope for ArtNat by Item, since items appear in one or other of these conditions. That is, Items cannot differ in the magnitude of their Art/Nat effect, since each item is EITHER Art or Nat.
model1 <- lmer(CorrSearchRT ~ 1 + Condition * ArtNat + (1 + Condition * ArtNat | Subject) + (1 + Condition | ItemNumber), data = CS,
control=lmerControl(optimizer="bobyqa"))
summary(model1)

##Warning: Singular fit --> one or more variances are very close to 0


#############################################
##REDUCTION: model2 eliminates Condition slopes for Items
##Model2
model2 <- lmer(CorrSearchRT ~ 1 + Condition * ArtNat + (1 + Condition * ArtNat | Subject) + (1 | ItemNumber), data = CS,
control=lmerControl(optimizer="bobyqa"))
summary(model2)

## Not a singular fit -- likely culprit was Condition for items

anova(model2, model1, refit = FALSE)
## No Sig reduction in fit -- ELIMINATE Condition slope for items


#############################################
##REDUCTION: model3 eliminates Condition * ArtNat slope for Ss
##Model3
model3 <- lmer(CorrSearchRT ~ 1 + Condition * ArtNat + (1 + Condition + ArtNat | Subject) + (1 | ItemNumber), data = CS,
control=lmerControl(optimizer="bobyqa"))
summary(model3)

anova(model3, model2, refit = FALSE)
## No Sig reduction in fit -- ELIMINATE Condition * ArtNat slope for Ss


#############################################
##REDUCTION: model5 eliminates ArtNat slope for Ss
##Model4
model4 <- lmer(CorrSearchRT ~ 1 + Condition * ArtNat + (1 + Condition | Subject) + (1 | ItemNumber), data = CS,
control=lmerControl(optimizer="bobyqa"))
summary(model4)

anova(model4, model3, refit = FALSE)
## Sig reduction in fit -- Retain ArtNat slope for the moment


#############################################
##REDUCTION: model5 eliminates Condition slope for Ss
##Model5
model5 <- lmer(CorrSearchRT ~ 1 + Condition * ArtNat + (1 + ArtNat | Subject) + (1 | ItemNumber), data = CS,
control=lmerControl(optimizer="bobyqa"))
summary(model5)

anova(model5, model3, refit = FALSE)
## No Sig reduction in fit -- ELIMINATE Condition slope for S


#############################################
##REDUCTION: model6 eliminates ArtNaT slope for S to eliminate convergence issue and condition slope for S
##Model6
model6 <- lmer(CorrSearchRT ~ 1 + Condition * ArtNat  + (1 | Subject) + (1 | ItemNumber), data = CS,
control=lmerControl(optimizer="bobyqa"))
summary(model6)

anova(model6, model5, refit = FALSE)
##Sig Reduction in fit, RETAIN ArtNat slope


###########
##Final Model is model5 with S and I intercepts and slope for ArtNat by S.
summary(model5)
anova(model5)



