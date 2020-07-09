library(R.matlab)
library(tidyr)
library(car)
library(lmerTest)
library(ggplot2)
library(ggsignif)
library(dplyr)

setwd("C:/Users/franc/Aalborg Universitet/Sofia Dahl - ASIAMS-drumstrokes/singleStrokes")

##### Procedure (from Camara et al.) #####

# Remove outliers, i.e. values more than three times the interquartile
# range away from the median of each condition, participant, and audio descriptor 
# separately and excluded

# Check for normality: within-subjects standardized distributions (average of all strokes across conditions
# for a given participant subtracted from each individual condition value for the same participant; see
# Fischer and Milfont, 2010), manually screened via histograms and Q-Q plots for each dependent variable separately. 

# [We use all strokes or means per series]
# Normal standardized distributions: one-way repeated-measures analyses of variances (RMANOVAs)
# For distributions showing departure from normality: corresponding non-parametric tests of differences of medians
# (Friedman) 
# [At the moment, I'm doing one-way ANOVAs and linear mixed models everywhere.]

# All pairwise post hoc comparison tests (paired t-test or Wilcoxon) Bonferroni-corrected for multiple comparisons 
# (three pairs). Violations of sphericity (Mauchly's test) corrected using Greenhouse-Geisser.
# [We don't need this, since we only have two levels.]

# [To do]
# Correlation tests for all instrument strokes across participants and style conditions 
# between the sound descriptors. For descriptor pairs with normal standardized distributions,
# Pearson's correlations tests; for pairs with at least one variable showing departure from normality, Spearman's
# correlations.

##### Summary ######

# totDur
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# significant difference in stroke condition according to linear mixed model (random effect of subject into account)

# totDurMeans
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# significant difference in stroke condition according to linear mixed model (random effect of subject into account)

# attDur
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# significant difference in stroke condition according to linear mixed model (random effect of subject into account)

# attDurMeans
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# significant difference between participants according to one-way RMANOVA

# decDur
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# significant difference in stroke condition according to linear mixed model (random effect of subject into account)

# decDurMeans
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# significant difference in stroke condition according to linear mixed model (random effect of subject into account)

# transDur
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# no significant difference

# transDurMeans
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# no significant difference

# LAT
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# significant difference in stroke condition according to linear mixed model (random effect of subject into account)

# LATMeans
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# no significant difference

# totSPL
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# no significant difference

# totSPLMeans
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# no significant difference

# attSPL
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# no significant difference

# attSPLMeans
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# no significant difference

# decSPL
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# no significant difference

# decSPLMeans
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# no significant difference

# transSPL
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# significant difference in stroke condition according to linear mixed model (random effect of subject into account)

# transSPLMeans
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# no significant difference

# totSC
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# significant difference in stroke condition according to linear mixed model (random effect of subject into account)

# totSCMeans
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# significant difference in stroke condition according to linear mixed model (random effect of subject into account)

# attSC
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# significant difference in stroke condition according to linear mixed model (random effect of subject into account)

# attSCMeans
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# significant difference in stroke condition according to linear mixed model (random effect of subject into account)

# decSC
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# no significant difference

# decSCMeans
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# no significant difference

# transSC
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# significant difference in stroke condition according to linear mixed model (random effect of subject into account)

# transSCMeans
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# significant difference in stroke condition according to linear mixed model (random effect of subject into account)

# TC
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# no significant difference

# TCMeans
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# no significant difference

# totAmpFlat
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# no significant difference

# totAmpFlatMeans
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# significant difference between participants according to one-way RMANOVA

# attAmpFlat
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# no significant difference

# attAmpFlatMeans
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.), but much higher p-value
# no significant difference

# decAmpFlat
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# significant difference in stroke condition according to linear mixed model (random effect of subject into account)

# decAmpFlatMeans
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.), but much higher p-value
# no significant difference

# transAmpFlat
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# no significant difference

# transAmpFlatMeans
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# no significant difference

# totSpecFlat
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# no significant difference

# totSpecFlatMeans
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# significant difference between participants according to one-way RMANOVA

# attSpecFlat
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# no significant difference

# attSpecFlatMeans
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.), but much higher p-value
# no significant difference

# decSpecFlat
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# no significant difference

# decSpecFlatMeans
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.), but much higher p-value
# no significant difference

# transSpecFlat
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# no significant difference

# transSpecFlatMeans
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# no significant difference

# crest
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# significant difference in stroke condition according to linear mixed model (random effect of subject into account)

# crestMeans
# Shapiro-Wilk rejects null hypothesis (H0: samples come from normal dist.)
# significant difference in stroke condition according to linear mixed model (random effect of subject into account)

##### Implementation #####

# totDur
totDur <- read.csv("totDur.csv")
totDur$Subject <- factor(totDur$Subject)
totDur$Series <- factor(totDur$Series)
totDur$Condition  <- factor(totDur$Condition)
totDur$Arm  <- factor(totDur$Arm)
str(totDur)
summary(totDur)
qqnorm(totDur$Value, pch = 1, frame = FALSE)
qqline(totDur$Value, col = "steelblue", lwd = 2)
hist(totDur$Value)
shapiro.test(totDur$Value)
replications(Value ~ Condition + Error(Condition), totDur)
totDurAov <- aov(Value ~ Condition + Error(Subject/Condition), data=totDur)
summary(totDurAov)
totDurLmer <- lmer(Value ~ Condition + (1|Subject), data=totDur)
anova(totDurLmer)

# totDurMeans
totDurMeans <- read.csv("totDurMeans.csv")
totDurMeans$mSubject <- factor(totDurMeans$mSubject)
totDurMeans$mCondition  <- factor(totDurMeans$mCondition)
totDurMeans$mArm  <- factor(totDurMeans$mArm)
str(totDurMeans)
summary(totDurMeans)
qqnorm(totDurMeans$mValue, pch = 1, frame = FALSE)
qqline(totDurMeans$mValue, col = "steelblue", lwd = 2)
hist(totDurMeans$mValue)
shapiro.test(totDurMeans$mValue)
replications(mValue ~ mCondition + Error(mSubject/mCondition), totDurMeans)
totDurMeansAov <- aov(mValue ~ mCondition + Error(mSubject/mCondition), data=totDurMeans)
summary(totDurMeansAov)
totDurMeansLmer <- lmer(mValue ~ mCondition + (1|mSubject), data=totDurMeans)
anova(totDurMeansLmer)
ggplot(totDurMeans, aes(x = mCondition, y = mValue)) +
  geom_boxplot(fill = "grey80", colour = "black") +
  scale_x_discrete() + xlab("Condition") +
  ylab("standardized total duration [ms]") +
  geom_signif(comparisons = list(c("C", "N")), 
              map_signif_level=FALSE)

# attDur
attDur <- read.csv("attDur.csv")
attDur$Subject <- factor(attDur$Subject)
attDur$Series <- factor(attDur$Series)
attDur$Condition  <- factor(attDur$Condition)
attDur$Arm  <- factor(attDur$Arm)
str(attDur)
summary(attDur)
qqnorm(attDur$Value, pch = 1, frame = FALSE)
qqline(attDur$Value, col = "steelblue", lwd = 2)
hist(attDur$Value)
shapiro.test(attDur$Value)
replications(Value ~ Condition + Error(Condition), attDur)
attDurAov <- aov(Value ~ Condition + Error(Subject/Condition), data=attDur)
summary(attDurAov)
attDurLmer <- lmer(Value ~ Condition + (1|Subject), data=attDur)
anova(attDurLmer)

# attDurMeans
attDurMeans <- read.csv("attDurMeans.csv")
attDurMeans$mSubject <- factor(attDurMeans$mSubject)
attDurMeans$mCondition  <- factor(attDurMeans$mCondition)
attDurMeans$mArm  <- factor(attDurMeans$mArm)
str(attDurMeans)
summary(attDurMeans)
qqnorm(attDurMeans$mValue, pch = 1, frame = FALSE)
qqline(attDurMeans$mValue, col = "steelblue", lwd = 2)
hist(attDurMeans$mValue)
shapiro.test(attDurMeans$mValue)
replications(mValue ~ mCondition + Error(mSubject/mCondition), attDurMeans)
attDurMeansAov <- aov(mValue ~ mCondition + Error(mSubject/mCondition), data=attDurMeans)
summary(attDurMeansAov)
attDurMeansLmer <- lmer(mValue ~ mCondition + (1|mSubject), data=attDurMeans)
anova(attDurMeansLmer)

# decDur
decDur <- read.csv("decDur.csv")
decDur$Subject <- factor(decDur$Subject)
decDur$Series <- factor(decDur$Series)
decDur$Condition  <- factor(decDur$Condition)
decDur$Arm  <- factor(decDur$Arm)
str(decDur)
summary(decDur)
qqnorm(decDur$Value, pch = 1, frame = FALSE)
qqline(decDur$Value, col = "steelblue", lwd = 2)
hist(decDur$Value)
shapiro.test(decDur$Value)
replications(Value ~ Condition + Error(Condition), decDur)
decDurAov <- aov(Value ~ Condition + Error(Subject/Condition), data=decDur)
summary(decDurAov)
decDurLmer <- lmer(Value ~ Condition + (1|Subject), data=decDur)
anova(decDurLmer)

# decDurMeans
decDurMeans <- read.csv("decDurMeans.csv")
decDurMeans$mSubject <- factor(decDurMeans$mSubject)
decDurMeans$mCondition  <- factor(decDurMeans$mCondition)
decDurMeans$mArm  <- factor(decDurMeans$mArm)
str(decDurMeans)
summary(decDurMeans)
qqnorm(decDurMeans$mValue, pch = 1, frame = FALSE)
qqline(decDurMeans$mValue, col = "steelblue", lwd = 2)
hist(decDurMeans$mValue)
shapiro.test(decDurMeans$mValue)
replications(mValue ~ mCondition + Error(mSubject/mCondition), decDurMeans)
decDurMeansAov <- aov(mValue ~ mCondition + Error(mSubject/mCondition), data=decDurMeans)
summary(decDurMeansAov)
decDurMeansLmer <- lmer(mValue ~ mCondition + (1|mSubject), data=decDurMeans)
anova(decDurMeansLmer)
ggplot(decDurMeans, aes(x = mCondition, y = mValue)) +
  geom_boxplot(fill = "grey80", colour = "black") +
  scale_x_discrete() + xlab("Condition") +
  ylab("standardized decay duration [ms]") +
  geom_signif(comparisons = list(c("C", "N")), 
              map_signif_level=FALSE)

# transDur
transDur <- read.csv("transDur.csv")
transDur$Subject <- factor(transDur$Subject)
transDur$Series <- factor(transDur$Series)
transDur$Condition  <- factor(transDur$Condition)
transDur$Arm  <- factor(transDur$Arm)
str(transDur)
summary(transDur)
qqnorm(transDur$Value, pch = 1, frame = FALSE)
qqline(transDur$Value, col = "steelblue", lwd = 2)
hist(transDur$Value)
shapiro.test(transDur$Value)
replications(Value ~ Condition + Error(Condition), transDur)
transDurAov <- aov(Value ~ Condition + Error(Subject/Condition), data=transDur)
summary(transDurAov)
transDurLmer <- lmer(Value ~ Condition + (1|Subject), data=transDur)
anova(transDurLmer)

# transDurMeans
transDurMeans <- read.csv("transDurMeans.csv")
transDurMeans$mSubject <- factor(transDurMeans$mSubject)
transDurMeans$mCondition  <- factor(transDurMeans$mCondition)
transDurMeans$mArm  <- factor(transDurMeans$mArm)
str(transDurMeans)
summary(transDurMeans)
qqnorm(transDurMeans$mValue, pch = 1, frame = FALSE)
qqline(transDurMeans$mValue, col = "steelblue", lwd = 2)
hist(transDurMeans$mValue)
shapiro.test(transDurMeans$mValue)
replications(mValue ~ mCondition + Error(mSubject/mCondition), transDurMeans)
transDurMeansAov <- aov(mValue ~ mCondition + Error(mSubject/mCondition), data=transDurMeans)
summary(transDurMeansAov)
transDurMeansLmer <- lmer(mValue ~ mCondition + (1|mSubject), data=transDurMeans)
anova(transDurMeansLmer)

# LAT
LAT <- read.csv("LAT.csv")
LAT$Subject <- factor(LAT$Subject)
LAT$Series <- factor(LAT$Series)
LAT$Condition  <- factor(LAT$Condition)
LAT$Arm  <- factor(LAT$Arm)
str(LAT)
summary(LAT)
qqnorm(LAT$Value, pch = 1, frame = FALSE)
qqline(LAT$Value, col = "steelblue", lwd = 2)
hist(LAT$Value)
shapiro.test(LAT$Value)
replications(Value ~ Condition + Error(Condition), LAT)
LATAov <- aov(Value ~ Condition + Error(Subject/Condition), data=LAT)
summary(LATAov)
LATLmer <- lmer(Value ~ Condition + (1|Subject), data=LAT)
anova(LATLmer)

# LATMeans
LATMeans <- read.csv("LATMeans.csv")
LATMeans$mSubject <- factor(LATMeans$mSubject)
LATMeans$mCondition  <- factor(LATMeans$mCondition)
LATMeans$mArm  <- factor(LATMeans$mArm)
str(LATMeans)
summary(LATMeans)
qqnorm(LATMeans$mValue, pch = 1, frame = FALSE)
qqline(LATMeans$mValue, col = "steelblue", lwd = 2)
hist(LATMeans$mValue)
shapiro.test(LATMeans$mValue)
replications(mValue ~ mCondition + Error(mSubject/mCondition), LATMeans)
LATMeansAov <- aov(mValue ~ mCondition + Error(mSubject/mCondition), data=LATMeans)
summary(LATMeansAov)
LATMeansLmer <- lmer(mValue ~ mCondition + (1|mSubject), data=LATMeans)
anova(LATMeansLmer)
ggplot(LATMeans, aes(x = mCondition, y = mValue)) +
  geom_boxplot(fill = "grey80", colour = "black") +
  scale_x_discrete() + xlab("Condition") +
  ylab("standardized log attack time [log(s)]") +
  geom_signif(comparisons = list(c("C", "N")), 
              map_signif_level=FALSE)

# totSPL
totSPL <- read.csv("totSPL.csv")
totSPL$Subject <- factor(totSPL$Subject)
totSPL$Series <- factor(totSPL$Series)
totSPL$Condition  <- factor(totSPL$Condition)
totSPL$Arm  <- factor(totSPL$Arm)
str(totSPL)
summary(totSPL)
qqnorm(totSPL$Value, pch = 1, frame = FALSE)
qqline(totSPL$Value, col = "steelblue", lwd = 2)
hist(totSPL$Value)
shapiro.test(totSPL$Value)
replications(Value ~ Condition + Error(Condition), totSPL)
totSPLAov <- aov(Value ~ Condition + Error(Subject/Condition), data=totSPL)
summary(totSPLAov)
totSPLLmer <- lmer(Value ~ Condition + (1|Subject), data=totSPL)
anova(totSPLLmer)

# totSPLMeans
totSPLMeans <- read.csv("totSPLMeans.csv")
totSPLMeans$mSubject <- factor(totSPLMeans$mSubject)
totSPLMeans$mCondition  <- factor(totSPLMeans$mCondition)
totSPLMeans$mArm  <- factor(totSPLMeans$mArm)
str(totSPLMeans)
summary(totSPLMeans)
qqnorm(totSPLMeans$mValue, pch = 1, frame = FALSE)
qqline(totSPLMeans$mValue, col = "steelblue", lwd = 2)
hist(totSPLMeans$mValue)
shapiro.test(totSPLMeans$mValue)
replications(mValue ~ mCondition + Error(mSubject/mCondition), totSPLMeans)
totSPLMeansAov <- aov(mValue ~ mCondition + Error(mSubject/mCondition), data=totSPLMeans)
summary(totSPLMeansAov)
totSPLMeansLmer <- lmer(mValue ~ mCondition + (1|mSubject), data=totSPLMeans)
anova(totSPLMeansLmer)

# attSPL
attSPL <- read.csv("attSPL.csv")
attSPL$Subject <- factor(attSPL$Subject)
attSPL$Series <- factor(attSPL$Series)
attSPL$Condition  <- factor(attSPL$Condition)
attSPL$Arm  <- factor(attSPL$Arm)
str(attSPL)
summary(attSPL)
qqnorm(attSPL$Value, pch = 1, frame = FALSE)
qqline(attSPL$Value, col = "steelblue", lwd = 2)
hist(attSPL$Value)
shapiro.test(attSPL$Value)
replications(Value ~ Condition + Error(Condition), attSPL)
attSPLAov <- aov(Value ~ Condition + Error(Subject/Condition), data=attSPL)
summary(attSPLAov)
attSPLLmer <- lmer(Value ~ Condition + (1|Subject), data=attSPL)
anova(attSPLLmer)

# attSPLMeans
attSPLMeans <- read.csv("attSPLMeans.csv")
attSPLMeans$mSubject <- factor(attSPLMeans$mSubject)
attSPLMeans$mCondition  <- factor(attSPLMeans$mCondition)
attSPLMeans$mArm  <- factor(attSPLMeans$mArm)
str(attSPLMeans)
summary(attSPLMeans)
qqnorm(attSPLMeans$mValue, pch = 1, frame = FALSE)
qqline(attSPLMeans$mValue, col = "steelblue", lwd = 2)
hist(attSPLMeans$mValue)
shapiro.test(attSPLMeans$mValue)
replications(mValue ~ mCondition + Error(mSubject/mCondition), attSPLMeans)
attSPLMeansAov <- aov(mValue ~ mCondition + Error(mSubject/mCondition), data=attSPLMeans)
summary(attSPLMeansAov)
attSPLMeansLmer <- lmer(mValue ~ mCondition + (1|mSubject), data=attSPLMeans)
anova(attSPLMeansLmer)

# decSPL
decSPL <- read.csv("decSPL.csv")
decSPL$Subject <- factor(decSPL$Subject)
decSPL$Series <- factor(decSPL$Series)
decSPL$Condition  <- factor(decSPL$Condition)
decSPL$Arm  <- factor(decSPL$Arm)
str(decSPL)
summary(decSPL)
qqnorm(decSPL$Value, pch = 1, frame = FALSE)
qqline(decSPL$Value, col = "steelblue", lwd = 2)
hist(decSPL$Value)
shapiro.test(decSPL$Value)
replications(Value ~ Condition + Error(Condition), decSPL)
decSPLAov <- aov(Value ~ Condition + Error(Subject/Condition), data=decSPL)
summary(decSPLAov)
decSPLLmer <- lmer(Value ~ Condition + (1|Subject), data=decSPL)
anova(decSPLLmer)

# decSPLMeans
decSPLMeans <- read.csv("decSPLMeans.csv")
decSPLMeans$mSubject <- factor(decSPLMeans$mSubject)
decSPLMeans$mCondition  <- factor(decSPLMeans$mCondition)
decSPLMeans$mArm  <- factor(decSPLMeans$mArm)
str(decSPLMeans)
summary(decSPLMeans)
qqnorm(decSPLMeans$mValue, pch = 1, frame = FALSE)
qqline(decSPLMeans$mValue, col = "steelblue", lwd = 2)
hist(decSPLMeans$mValue)
shapiro.test(decSPLMeans$mValue)
replications(mValue ~ mCondition + Error(mSubject/mCondition), decSPLMeans)
decSPLMeansAov <- aov(mValue ~ mCondition + Error(mSubject/mCondition), data=decSPLMeans)
summary(decSPLMeansAov)
decSPLMeansLmer <- lmer(mValue ~ mCondition + (1|mSubject), data=decSPLMeans)
anova(decSPLMeansLmer)

# transSPL
transSPL <- read.csv("transSPL.csv")
transSPL$Subject <- factor(transSPL$Subject)
transSPL$Series <- factor(transSPL$Series)
transSPL$Condition  <- factor(transSPL$Condition)
transSPL$Arm  <- factor(transSPL$Arm)
str(transSPL)
summary(transSPL)
qqnorm(transSPL$Value, pch = 1, frame = FALSE)
qqline(transSPL$Value, col = "steelblue", lwd = 2)
hist(transSPL$Value)
shapiro.test(transSPL$Value)
replications(Value ~ Condition + Error(Condition), transSPL)
transSPLAov <- aov(Value ~ Condition + Error(Subject/Condition), data=transSPL)
summary(transSPLAov)
transSPLLmer <- lmer(Value ~ Condition + (1|Subject), data=transSPL)
anova(transSPLLmer)

# transSPLMeans
transSPLMeans <- read.csv("transSPLMeans.csv")
transSPLMeans$mSubject <- factor(transSPLMeans$mSubject)
transSPLMeans$mCondition  <- factor(transSPLMeans$mCondition)
transSPLMeans$mArm  <- factor(transSPLMeans$mArm)
str(transSPLMeans)
summary(transSPLMeans)
qqnorm(transSPLMeans$mValue, pch = 1, frame = FALSE)
qqline(transSPLMeans$mValue, col = "steelblue", lwd = 2)
hist(transSPLMeans$mValue)
shapiro.test(transSPLMeans$mValue)
replications(mValue ~ mCondition + Error(mSubject/mCondition), transSPLMeans)
transSPLMeansAov <- aov(mValue ~ mCondition + Error(mSubject/mCondition), data=transSPLMeans)
summary(transSPLMeansAov)
transSPLMeansLmer <- lmer(mValue ~ mCondition + (1|mSubject), data=transSPLMeans)
anova(transSPLMeansLmer)

# totSC
totSC <- read.csv("totSC.csv")
totSC$Subject <- factor(totSC$Subject)
totSC$Series <- factor(totSC$Series)
totSC$Condition  <- factor(totSC$Condition)
totSC$Arm  <- factor(totSC$Arm)
str(totSC)
summary(totSC)
qqnorm(totSC$Value, pch = 1, frame = FALSE)
qqline(totSC$Value, col = "steelblue", lwd = 2)
hist(totSC$Value)
shapiro.test(totSC$Value)
replications(Value ~ Condition + Error(Condition), totSC)
totSCAov <- aov(Value ~ Condition + Error(Subject/Condition), data=totSC)
summary(totSCAov)
totSCLmer <- lmer(Value ~ Condition + (1|Subject), data=totSC)
anova(totSCLmer)

# totSCMeans
totSCMeans <- read.csv("totSCMeans.csv")
totSCMeans$mSubject <- factor(totSCMeans$mSubject)
totSCMeans$mCondition  <- factor(totSCMeans$mCondition)
totSCMeans$mArm  <- factor(totSCMeans$mArm)
str(totSCMeans)
summary(totSCMeans)
qqnorm(totSCMeans$mValue, pch = 1, frame = FALSE)
qqline(totSCMeans$mValue, col = "steelblue", lwd = 2)
hist(totSCMeans$mValue)
shapiro.test(totSCMeans$mValue)
replications(mValue ~ mCondition + Error(mSubject/mCondition), totSCMeans)
totSCMeansAov <- aov(mValue ~ mCondition + Error(mSubject/mCondition), data=totSCMeans)
summary(totSCMeansAov)
totSCMeansLmer <- lmer(mValue ~ mCondition + (1|mSubject), data=totSCMeans)
anova(totSCMeansLmer)
ggplot(totSCMeans, aes(x = mCondition, y = mValue)) +
  geom_boxplot(fill = "grey80", colour = "black") +
  scale_x_discrete() + xlab("Condition") +
  ylab("standardized total spectral centroid [Hz]") +
  geom_signif(comparisons = list(c("C", "N")), 
              map_signif_level=FALSE)

# attSC
attSC <- read.csv("attSC.csv")
attSC$Subject <- factor(attSC$Subject)
attSC$Series <- factor(attSC$Series)
attSC$Condition  <- factor(attSC$Condition)
attSC$Arm  <- factor(attSC$Arm)
str(attSC)
summary(attSC)
qqnorm(attSC$Value, pch = 1, frame = FALSE)
qqline(attSC$Value, col = "steelblue", lwd = 2)
hist(attSC$Value)
shapiro.test(attSC$Value)
replications(Value ~ Condition + Error(Condition), attSC)
attSCAov <- aov(Value ~ Condition + Error(Subject/Condition), data=attSC)
summary(attSCAov)
attSCLmer <- lmer(Value ~ Condition + (1|Subject), data=attSC)
anova(attSCLmer)

# attSCMeans
attSCMeans <- read.csv("attSCMeans.csv")
attSCMeans$mSubject <- factor(attSCMeans$mSubject)
attSCMeans$mCondition  <- factor(attSCMeans$mCondition)
attSCMeans$mArm  <- factor(attSCMeans$mArm)
str(attSCMeans)
summary(attSCMeans)
qqnorm(attSCMeans$mValue, pch = 1, frame = FALSE)
qqline(attSCMeans$mValue, col = "steelblue", lwd = 2)
hist(attSCMeans$mValue)
shapiro.test(attSCMeans$mValue)
replications(mValue ~ mCondition + Error(mSubject/mCondition), attSCMeans)
attSCMeansAov <- aov(mValue ~ mCondition + Error(mSubject/mCondition), data=attSCMeans)
summary(attSCMeansAov)
attSCMeansLmer <- lmer(mValue ~ mCondition + (1|mSubject), data=attSCMeans)
anova(attSCMeansLmer)
ggplot(totSCMeans, aes(x = mCondition, y = mValue)) +
  geom_boxplot(fill = "grey80", colour = "black") +
  scale_x_discrete() + xlab("Condition") +
  ylab("standardized attack spectral centroid [Hz]") +
  geom_signif(comparisons = list(c("C", "N")), 
              map_signif_level=FALSE)

# decSC
decSC <- read.csv("decSC.csv")
decSC$Subject <- factor(decSC$Subject)
decSC$Series <- factor(decSC$Series)
decSC$Condition  <- factor(decSC$Condition)
decSC$Arm  <- factor(decSC$Arm)
str(decSC)
summary(decSC)
qqnorm(decSC$Value, pch = 1, frame = FALSE)
qqline(decSC$Value, col = "steelblue", lwd = 2)
hist(decSC$Value)
shapiro.test(decSC$Value)
replications(Value ~ Condition + Error(Condition), decSC)
decSCAov <- aov(Value ~ Condition + Error(Subject/Condition), data=decSC)
summary(decSCAov)
decSCLmer <- lmer(Value ~ Condition + (1|Subject), data=decSC)
anova(decSCLmer)

# decSCMeans
decSCMeans <- read.csv("decSCMeans.csv")
decSCMeans$mSubject <- factor(decSCMeans$mSubject)
decSCMeans$mCondition  <- factor(decSCMeans$mCondition)
decSCMeans$mArm  <- factor(decSCMeans$mArm)
str(decSCMeans)
summary(decSCMeans)
qqnorm(decSCMeans$mValue, pch = 1, frame = FALSE)
qqline(decSCMeans$mValue, col = "steelblue", lwd = 2)
hist(decSCMeans$mValue)
shapiro.test(decSCMeans$mValue)
replications(mValue ~ mCondition + Error(mSubject/mCondition), decSCMeans)
decSCMeansAov <- aov(mValue ~ mCondition + Error(mSubject/mCondition), data=decSCMeans)
summary(decSCMeansAov)
decSCMeansLmer <- lmer(mValue ~ mCondition + (1|mSubject), data=decSCMeans)
anova(decSCMeansLmer)

# transSC
transSC <- read.csv("transSC.csv")
transSC$Subject <- factor(transSC$Subject)
transSC$Series <- factor(transSC$Series)
transSC$Condition  <- factor(transSC$Condition)
transSC$Arm  <- factor(transSC$Arm)
str(transSC)
summary(transSC)
qqnorm(transSC$Value, pch = 1, frame = FALSE)
qqline(transSC$Value, col = "steelblue", lwd = 2)
hist(transSC$Value)
shapiro.test(transSC$Value)
replications(Value ~ Condition + Error(Condition), transSC)
transSCAov <- aov(Value ~ Condition + Error(Subject/Condition), data=transSC)
summary(transSCAov)
transSCLmer <- lmer(Value ~ Condition + (1|Subject), data=transSC)
anova(transSCLmer)

# transSCMeans
transSCMeans <- read.csv("transSCMeans.csv")
transSCMeans$mSubject <- factor(transSCMeans$mSubject)
transSCMeans$mCondition  <- factor(transSCMeans$mCondition)
transSCMeans$mArm  <- factor(transSCMeans$mArm)
str(transSCMeans)
summary(transSCMeans)
qqnorm(transSCMeans$mValue, pch = 1, frame = FALSE)
qqline(transSCMeans$mValue, col = "steelblue", lwd = 2)
hist(transSCMeans$mValue)
shapiro.test(transSCMeans$mValue)
replications(mValue ~ mCondition + Error(mSubject/mCondition), transSCMeans)
transSCMeansAov <- aov(mValue ~ mCondition + Error(mSubject/mCondition), data=transSCMeans)
summary(transSCMeansAov)
transSCMeansLmer <- lmer(mValue ~ mCondition + (1|mSubject), data=transSCMeans)
anova(transSCMeansLmer)
ggplot(transSCMeans, aes(x = mCondition, y = mValue)) +
  geom_boxplot(fill = "grey80", colour = "black") +
  scale_x_discrete() + xlab("Condition") +
  ylab("standardized transient spectral centroid [Hz]") +
  geom_signif(comparisons = list(c("C", "N")), 
              map_signif_level=FALSE)

# TC
TC <- read.csv("TC.csv")
TC$Subject <- factor(TC$Subject)
TC$Series <- factor(TC$Series)
TC$Condition  <- factor(TC$Condition)
TC$Arm  <- factor(TC$Arm)
str(TC)
summary(TC)
qqnorm(TC$Value, pch = 1, frame = FALSE)
qqline(TC$Value, col = "steelblue", lwd = 2)
hist(TC$Value)
shapiro.test(TC$Value)
replications(Value ~ Condition + Error(Condition), TC)
TCAov <- aov(Value ~ Condition + Error(Subject/Condition), data=TC)
summary(TCAov)
TCLmer <- lmer(Value ~ Condition + (1|Subject), data=TC)
anova(TCLmer)

# TCMeans
TCMeans <- read.csv("TCMeans.csv")
TCMeans$mSubject <- factor(TCMeans$mSubject)
TCMeans$mCondition  <- factor(TCMeans$mCondition)
TCMeans$mArm  <- factor(TCMeans$mArm)
str(TCMeans)
summary(TCMeans)
qqnorm(TCMeans$mValue, pch = 1, frame = FALSE)
qqline(TCMeans$mValue, col = "steelblue", lwd = 2)
hist(TCMeans$mValue)
shapiro.test(TCMeans$mValue)
replications(mValue ~ mCondition + Error(mSubject/mCondition), TCMeans)
TCMeansAov <- aov(mValue ~ mCondition + Error(mSubject/mCondition), data=TCMeans)
summary(TCMeansAov)
TCMeansLmer <- lmer(mValue ~ mCondition + (1|mSubject), data=TCMeans)
anova(TCMeansLmer)

# totAmpFlat
totAmpFlat <- read.csv("totAmpFlat.csv")
totAmpFlat$Subject <- factor(totAmpFlat$Subject)
totAmpFlat$Series <- factor(totAmpFlat$Series)
totAmpFlat$Condition  <- factor(totAmpFlat$Condition)
totAmpFlat$Arm  <- factor(totAmpFlat$Arm)
str(totAmpFlat)
summary(totAmpFlat)
qqnorm(totAmpFlat$Value, pch = 1, frame = FALSE)
qqline(totAmpFlat$Value, col = "steelblue", lwd = 2)
hist(totAmpFlat$Value)
shapiro.test(totAmpFlat$Value)
replications(Value ~ Condition + Error(Condition), totAmpFlat)
totAmpFlatAov <- aov(Value ~ Condition + Error(Subject/Condition), data=totAmpFlat)
summary(totAmpFlatAov)
totAmpFlatLmer <- lmer(Value ~ Condition + (1|Subject), data=totAmpFlat)
anova(totAmpFlatLmer)

# totAmpFlatMeans
totAmpFlatMeans <- read.csv("totAmpFlatMeans.csv")
totAmpFlatMeans$mSubject <- factor(totAmpFlatMeans$mSubject)
totAmpFlatMeans$mCondition  <- factor(totAmpFlatMeans$mCondition)
totAmpFlatMeans$mArm  <- factor(totAmpFlatMeans$mArm)
str(totAmpFlatMeans)
summary(totAmpFlatMeans)
qqnorm(totAmpFlatMeans$mValue, pch = 1, frame = FALSE)
qqline(totAmpFlatMeans$mValue, col = "steelblue", lwd = 2)
hist(totAmpFlatMeans$mValue)
shapiro.test(totAmpFlatMeans$mValue)
replications(mValue ~ mCondition + Error(mSubject/mCondition), totAmpFlatMeans)
totAmpFlatMeansAov <- aov(mValue ~ mCondition + Error(mSubject/mCondition), data=totAmpFlatMeans)
summary(totAmpFlatMeansAov)
totAmpFlatMeansLmer <- lmer(mValue ~ mCondition + (1|mSubject), data=totAmpFlatMeans)
anova(totAmpFlatMeansLmer)
ggplot(totAmpFlatMeans, aes(x = mCondition, y = mValue)) +
  geom_boxplot(fill = "grey80", colour = "black") +
  scale_x_discrete() + xlab("Condition") +
  ylab("standardized total amplitude flatness [-]") +
  geom_signif(comparisons = list(c("C", "N")), 
              map_signif_level=FALSE)

# attAmpFlat
attAmpFlat <- read.csv("attAmpFlat.csv")
attAmpFlat$Subject <- factor(attAmpFlat$Subject)
attAmpFlat$Series <- factor(attAmpFlat$Series)
attAmpFlat$Condition  <- factor(attAmpFlat$Condition)
attAmpFlat$Arm  <- factor(attAmpFlat$Arm)
str(attAmpFlat)
summary(attAmpFlat)
qqnorm(attAmpFlat$Value, pch = 1, frame = FALSE)
qqline(attAmpFlat$Value, col = "steelblue", lwd = 2)
hist(attAmpFlat$Value)
shapiro.test(attAmpFlat$Value)
replications(Value ~ Condition + Error(Condition), attAmpFlat)
attAmpFlatAov <- aov(Value ~ Condition + Error(Subject/Condition), data=attAmpFlat)
summary(attAmpFlatAov)
attAmpFlatLmer <- lmer(Value ~ Condition + (1|Subject), data=attAmpFlat)
anova(attAmpFlatLmer)

# attAmpFlatMeans
attAmpFlatMeans <- read.csv("attAmpFlatMeans.csv")
attAmpFlatMeans$mSubject <- factor(attAmpFlatMeans$mSubject)
attAmpFlatMeans$mCondition  <- factor(attAmpFlatMeans$mCondition)
attAmpFlatMeans$mArm  <- factor(attAmpFlatMeans$mArm)
str(attAmpFlatMeans)
summary(attAmpFlatMeans)
qqnorm(attAmpFlatMeans$mValue, pch = 1, frame = FALSE)
qqline(attAmpFlatMeans$mValue, col = "steelblue", lwd = 2)
hist(attAmpFlatMeans$mValue)
shapiro.test(attAmpFlatMeans$mValue)
replications(mValue ~ mCondition + Error(mSubject/mCondition), attAmpFlatMeans)
attAmpFlatMeansAov <- aov(mValue ~ mCondition + Error(mSubject/mCondition), data=attAmpFlatMeans)
summary(attAmpFlatMeansAov)
attAmpFlatMeansLmer <- lmer(mValue ~ mCondition + (1|mSubject), data=attAmpFlatMeans)
anova(attAmpFlatMeansLmer)

# decAmpFlat
decAmpFlat <- read.csv("decAmpFlat.csv")
decAmpFlat$Subject <- factor(decAmpFlat$Subject)
decAmpFlat$Series <- factor(decAmpFlat$Series)
decAmpFlat$Condition  <- factor(decAmpFlat$Condition)
decAmpFlat$Arm  <- factor(decAmpFlat$Arm)
str(decAmpFlat)
summary(decAmpFlat)
qqnorm(decAmpFlat$Value, pch = 1, frame = FALSE)
qqline(decAmpFlat$Value, col = "steelblue", lwd = 2)
hist(decAmpFlat$Value)
shapiro.test(decAmpFlat$Value)
replications(Value ~ Condition + Error(Condition), decAmpFlat)
decAmpFlatAov <- aov(Value ~ Condition + Error(Subject/Condition), data=decAmpFlat)
summary(decAmpFlatAov)
decAmpFlatLmer <- lmer(Value ~ Condition + (1|Subject), data=decAmpFlat)
anova(decAmpFlatLmer)

# decAmpFlatMeans
decAmpFlatMeans <- read.csv("decAmpFlatMeans.csv")
decAmpFlatMeans$mSubject <- factor(decAmpFlatMeans$mSubject)
decAmpFlatMeans$mCondition  <- factor(decAmpFlatMeans$mCondition)
decAmpFlatMeans$mArm  <- factor(decAmpFlatMeans$mArm)
str(decAmpFlatMeans)
summary(decAmpFlatMeans)
qqnorm(decAmpFlatMeans$mValue, pch = 1, frame = FALSE)
qqline(decAmpFlatMeans$mValue, col = "steelblue", lwd = 2)
hist(decAmpFlatMeans$mValue)
shapiro.test(decAmpFlatMeans$mValue)
replications(mValue ~ mCondition + Error(mSubject/mCondition), decAmpFlatMeans)
decAmpFlatMeansAov <- aov(mValue ~ mCondition + Error(mSubject/mCondition), data=decAmpFlatMeans)
summary(decAmpFlatMeansAov)
decAmpFlatMeansLmer <- lmer(mValue ~ mCondition + (1|mSubject), data=decAmpFlatMeans)
anova(decAmpFlatMeansLmer)
ggplot(decAmpFlatMeans, aes(x = mCondition, y = mValue)) +
  geom_boxplot(fill = "grey80", colour = "black") +
  scale_x_discrete() + xlab("Condition") +
  ylab("standardized decay amplitude flatness [-]") +
  geom_signif(comparisons = list(c("C", "N")), 
              map_signif_level=FALSE)

# transAmpFlat
transAmpFlat <- read.csv("transAmpFlat.csv")
transAmpFlat$Subject <- factor(transAmpFlat$Subject)
transAmpFlat$Series <- factor(transAmpFlat$Series)
transAmpFlat$Condition  <- factor(transAmpFlat$Condition)
transAmpFlat$Arm  <- factor(transAmpFlat$Arm)
str(transAmpFlat)
summary(transAmpFlat)
qqnorm(transAmpFlat$Value, pch = 1, frame = FALSE)
qqline(transAmpFlat$Value, col = "steelblue", lwd = 2)
hist(transAmpFlat$Value)
shapiro.test(transAmpFlat$Value)
replications(Value ~ Condition + Error(Condition), transAmpFlat)
transAmpFlatAov <- aov(Value ~ Condition + Error(Subject/Condition), data=transAmpFlat)
summary(transAmpFlatAov)
transAmpFlatLmer <- lmer(Value ~ Condition + (1|Subject), data=transAmpFlat)
anova(transAmpFlatLmer)

# transAmpFlatMeans
transAmpFlatMeans <- read.csv("transAmpFlatMeans.csv")
transAmpFlatMeans$mSubject <- factor(transAmpFlatMeans$mSubject)
transAmpFlatMeans$mCondition  <- factor(transAmpFlatMeans$mCondition)
transAmpFlatMeans$mArm  <- factor(transAmpFlatMeans$mArm)
str(transAmpFlatMeans)
summary(transAmpFlatMeans)
qqnorm(transAmpFlatMeans$mValue, pch = 1, frame = FALSE)
qqline(transAmpFlatMeans$mValue, col = "steelblue", lwd = 2)
hist(transAmpFlatMeans$mValue)
shapiro.test(transAmpFlatMeans$mValue)
replications(mValue ~ mCondition + Error(mSubject/mCondition), transAmpFlatMeans)
transAmpFlatMeansAov <- aov(mValue ~ mCondition + Error(mSubject/mCondition), data=transAmpFlatMeans)
summary(transAmpFlatMeansAov)
transAmpFlatMeansLmer <- lmer(mValue ~ mCondition + (1|mSubject), data=transAmpFlatMeans)
anova(transAmpFlatMeansLmer)
ggplot(transAmpFlatMeans, aes(x = mCondition, y = mValue)) +
  geom_boxplot(fill = "grey80", colour = "black") +
  scale_x_discrete() + xlab("Condition") +
  ylab("standardized transient amplitude flatness [-]") +
  geom_signif(comparisons = list(c("C", "N")), 
              map_signif_level=FALSE)

# totSpecFlat
totSpecFlat <- read.csv("totSpecFlat.csv")
totSpecFlat$Subject <- factor(totSpecFlat$Subject)
totSpecFlat$Series <- factor(totSpecFlat$Series)
totSpecFlat$Condition  <- factor(totSpecFlat$Condition)
totSpecFlat$Arm  <- factor(totSpecFlat$Arm)
str(totSpecFlat)
summary(totSpecFlat)
qqnorm(totSpecFlat$Value, pch = 1, frame = FALSE)
qqline(totSpecFlat$Value, col = "steelblue", lwd = 2)
hist(totSpecFlat$Value)
shapiro.test(totSpecFlat$Value)
replications(Value ~ Condition + Error(Condition), totSpecFlat)
totSpecFlatAov <- aov(Value ~ Condition + Error(Subject/Condition), data=totSpecFlat)
summary(totSpecFlatAov)
totSpecFlatLmer <- lmer(Value ~ Condition + (1|Subject), data=totSpecFlat)
anova(totSpecFlatLmer)

# totSpecFlatMeans
totSpecFlatMeans <- read.csv("totSpecFlatMeans.csv")
totSpecFlatMeans$mSubject <- factor(totSpecFlatMeans$mSubject)
totSpecFlatMeans$mCondition  <- factor(totSpecFlatMeans$mCondition)
totSpecFlatMeans$mArm  <- factor(totSpecFlatMeans$mArm)
str(totSpecFlatMeans)
summary(totSpecFlatMeans)
qqnorm(totSpecFlatMeans$mValue, pch = 1, frame = FALSE)
qqline(totSpecFlatMeans$mValue, col = "steelblue", lwd = 2)
hist(totSpecFlatMeans$mValue)
shapiro.test(totSpecFlatMeans$mValue)
replications(mValue ~ mCondition + Error(mSubject/mCondition), totSpecFlatMeans)
totSpecFlatMeansAov <- aov(mValue ~ mCondition + Error(mSubject/mCondition), data=totSpecFlatMeans)
summary(totSpecFlatMeansAov)
totSpecFlatMeansLmer <- lmer(mValue ~ mCondition + (1|mSubject), data=totSpecFlatMeans)
anova(totSpecFlatMeansLmer)
ggplot(totSpecFlatMeans, aes(x = mCondition, y = mValue)) +
  geom_boxplot(fill = "grey80", colour = "black") +
  scale_x_discrete() + xlab("Condition") +
  ylab("standardized total spectral flatness [-]") +
  geom_signif(comparisons = list(c("C", "N")), 
              map_signif_level=FALSE)

# attSpecFlat
attSpecFlat <- read.csv("attSpecFlat.csv")
attSpecFlat$Subject <- factor(attSpecFlat$Subject)
attSpecFlat$Series <- factor(attSpecFlat$Series)
attSpecFlat$Condition  <- factor(attSpecFlat$Condition)
attSpecFlat$Arm  <- factor(attSpecFlat$Arm)
str(attSpecFlat)
summary(attSpecFlat)
qqnorm(attSpecFlat$Value, pch = 1, frame = FALSE)
qqline(attSpecFlat$Value, col = "steelblue", lwd = 2)
hist(attSpecFlat$Value)
shapiro.test(attSpecFlat$Value)
replications(Value ~ Condition + Error(Condition), attSpecFlat)
attSpecFlatAov <- aov(Value ~ Condition + Error(Subject/Condition), data=attSpecFlat)
summary(attSpecFlatAov)
attSpecFlatLmer <- lmer(Value ~ Condition + (1|Subject), data=attSpecFlat)
anova(attSpecFlatLmer)

# attSpecFlatMeans
attSpecFlatMeans <- read.csv("attSpecFlatMeans.csv")
attSpecFlatMeans$mSubject <- factor(attSpecFlatMeans$mSubject)
attSpecFlatMeans$mCondition  <- factor(attSpecFlatMeans$mCondition)
attSpecFlatMeans$mArm  <- factor(attSpecFlatMeans$mArm)
str(attSpecFlatMeans)
summary(attSpecFlatMeans)
qqnorm(attSpecFlatMeans$mValue, pch = 1, frame = FALSE)
qqline(attSpecFlatMeans$mValue, col = "steelblue", lwd = 2)
hist(attSpecFlatMeans$mValue)
shapiro.test(attSpecFlatMeans$mValue)
replications(mValue ~ mCondition + Error(mSubject/mCondition), attSpecFlatMeans)
attSpecFlatMeansAov <- aov(mValue ~ mCondition + Error(mSubject/mCondition), data=attSpecFlatMeans)
summary(attSpecFlatMeansAov)
attSpecFlatMeansLmer <- lmer(mValue ~ mCondition + (1|mSubject), data=attSpecFlatMeans)
anova(attSpecFlatMeansLmer)

# decSpecFlat
decSpecFlat <- read.csv("decSpecFlat.csv")
decSpecFlat$Subject <- factor(decSpecFlat$Subject)
decSpecFlat$Series <- factor(decSpecFlat$Series)
decSpecFlat$Condition  <- factor(decSpecFlat$Condition)
decSpecFlat$Arm  <- factor(decSpecFlat$Arm)
str(decSpecFlat)
summary(decSpecFlat)
qqnorm(decSpecFlat$Value, pch = 1, frame = FALSE)
qqline(decSpecFlat$Value, col = "steelblue", lwd = 2)
hist(decSpecFlat$Value)
shapiro.test(decSpecFlat$Value)
replications(Value ~ Condition + Error(Condition), decSpecFlat)
decSpecFlatAov <- aov(Value ~ Condition + Error(Subject/Condition), data=decSpecFlat)
summary(decSpecFlatAov)
decSpecFlatLmer <- lmer(Value ~ Condition + (1|Subject), data=decSpecFlat)
anova(decSpecFlatLmer)

# decSpecFlatMeans
decSpecFlatMeans <- read.csv("decSpecFlatMeans.csv")
decSpecFlatMeans$mSubject <- factor(decSpecFlatMeans$mSubject)
decSpecFlatMeans$mCondition  <- factor(decSpecFlatMeans$mCondition)
decSpecFlatMeans$mArm  <- factor(decSpecFlatMeans$mArm)
str(decSpecFlatMeans)
summary(decSpecFlatMeans)
qqnorm(decSpecFlatMeans$mValue, pch = 1, frame = FALSE)
qqline(decSpecFlatMeans$mValue, col = "steelblue", lwd = 2)
hist(decSpecFlatMeans$mValue)
shapiro.test(decSpecFlatMeans$mValue)
replications(mValue ~ mCondition + Error(mSubject/mCondition), decSpecFlatMeans)
decSpecFlatMeansAov <- aov(mValue ~ mCondition + Error(mSubject/mCondition), data=decSpecFlatMeans)
summary(decSpecFlatMeansAov)
decSpecFlatMeansLmer <- lmer(mValue ~ mCondition + (1|mSubject), data=decSpecFlatMeans)
anova(decSpecFlatMeansLmer)

# transSpecFlat
transSpecFlat <- read.csv("transSpecFlat.csv")
transSpecFlat$Subject <- factor(transSpecFlat$Subject)
transSpecFlat$Series <- factor(transSpecFlat$Series)
transSpecFlat$Condition  <- factor(transSpecFlat$Condition)
transSpecFlat$Arm  <- factor(transSpecFlat$Arm)
str(transSpecFlat)
summary(transSpecFlat)
qqnorm(transSpecFlat$Value, pch = 1, frame = FALSE)
qqline(transSpecFlat$Value, col = "steelblue", lwd = 2)
hist(transSpecFlat$Value)
shapiro.test(transSpecFlat$Value)
replications(Value ~ Condition + Error(Condition), transSpecFlat)
transSpecFlatAov <- aov(Value ~ Condition + Error(Subject/Condition), data=transSpecFlat)
summary(transSpecFlatAov)
transSpecFlatLmer <- lmer(Value ~ Condition + (1|Subject), data=transSpecFlat)
anova(transSpecFlatLmer)

# transSpecFlatMeans
transSpecFlatMeans <- read.csv("transSpecFlatMeans.csv")
transSpecFlatMeans$mSubject <- factor(transSpecFlatMeans$mSubject)
transSpecFlatMeans$mCondition  <- factor(transSpecFlatMeans$mCondition)
transSpecFlatMeans$mArm  <- factor(transSpecFlatMeans$mArm)
str(transSpecFlatMeans)
summary(transSpecFlatMeans)
qqnorm(transSpecFlatMeans$mValue, pch = 1, frame = FALSE)
qqline(transSpecFlatMeans$mValue, col = "steelblue", lwd = 2)
hist(transSpecFlatMeans$mValue)
shapiro.test(transSpecFlatMeans$mValue)
replications(mValue ~ mCondition + Error(mSubject/mCondition), transSpecFlatMeans)
transSpecFlatMeansAov <- aov(mValue ~ mCondition + Error(mSubject/mCondition), data=transSpecFlatMeans)
summary(transSpecFlatMeansAov)
transSpecFlatMeansLmer <- lmer(mValue ~ mCondition + (1|mSubject), data=transSpecFlatMeans)
anova(transSpecFlatMeansLmer)
ggplot(transSpecFlatMeans, aes(x = mCondition, y = mValue)) +
  geom_boxplot(fill = "grey80", colour = "black") +
  scale_x_discrete() + xlab("Condition") +
  ylab("standardized transient spectral flatness [-]") +
  geom_signif(comparisons = list(c("C", "N")), 
              map_signif_level=FALSE)

# crest
crest <- read.csv("crest.csv")
crest$Subject <- factor(crest$Subject)
crest$Series <- factor(crest$Series)
crest$Condition  <- factor(crest$Condition)
crest$Arm  <- factor(crest$Arm)
str(crest)
summary(crest)
qqnorm(crest$Value, pch = 1, frame = FALSE)
qqline(crest$Value, col = "steelblue", lwd = 2)
hist(crest$Value)
shapiro.test(crest$Value)
replications(Value ~ Condition + Error(Condition), crest)
crestAov <- aov(Value ~ Condition + Error(Subject/Condition), data=crest)
summary(crestAov)
crestLmer <- lmer(Value ~ Condition + (1|Subject), data=crest)
anova(crestLmer)

# crestMeans
crestMeans <- read.csv("crestMeans.csv")
crestMeans$mSubject <- factor(crestMeans$mSubject)
crestMeans$mCondition  <- factor(crestMeans$mCondition)
crestMeans$mArm  <- factor(crestMeans$mArm)
str(crestMeans)
summary(crestMeans)
qqnorm(crestMeans$mValue, pch = 1, frame = FALSE)
qqline(crestMeans$mValue, col = "steelblue", lwd = 2)
hist(crestMeans$mValue)
shapiro.test(crestMeans$mValue)
replications(mValue ~ mCondition + Error(mSubject/mCondition), crestMeans)
crestMeansAov <- aov(mValue ~ mCondition + Error(mSubject/mCondition), data=crestMeans)
summary(crestMeansAov)
crestMeansLmer <- lmer(mValue ~ mCondition + (1|mSubject), data=crestMeans)
anova(crestMeansLmer)
ggplot(crestMeans, aes(x = mCondition, y = mValue)) +
  geom_boxplot(fill = "grey80", colour = "black") +
  scale_x_discrete() + xlab("Condition") +
  ylab("standardized crest factor [-]") +
  geom_signif(comparisons = list(c("C", "N")), 
              map_signif_level=FALSE)
