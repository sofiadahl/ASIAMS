library(tidyverse)
library(ggpubr)
library(rstatix)
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

# All pairwise post hoc comparison tests (paired t-test or Wilcoxon) Bonferroni-corrected for multiple comparisons 
# (three pairs). Violations of sphericity (Mauchly's test) corrected using Greenhouse-Geisser.
# [We don't need this, since we only have two levels.]

# [To do]
# Correlation tests for all instrument strokes across participants and style conditions 
# between the sound descriptors. For descriptor pairs with normal standardized distributions,
# Pearson's correlations tests; for pairs with at least one variable showing departure from normality, Spearman's
# correlations.

##### Summary ######

# I tried to use mean condition per series/subject, but the models won't fit as we would have replicated measures.

# totDur
# Deviates from normality
# no significant difference with ANOVA or Friedman's

# transDur
# Deviates from normality
# no significant difference with ANOVA or Friedman's (smaller p-values)

# decDur
# Deviates from normality
# no significant difference with ANOVA or Friedman's

# transDur
# Deviates from normality
# significant difference between conditions according to Friedman test, Wilcoxon signed-rank test fails

# LAT
# Deviates from normality
# no significant difference with ANOVA or Friedman's

# totSPL
# Deviates from normality
# no significant difference with ANOVA or Friedman's

# attSPL
# Deviates from normality
# no significant difference with ANOVA or Friedman's

# decSPL
# Deviates from normality
# no significant difference with ANOVA or Friedman's

# transSPL
# Deviates from normality
# no significant difference with ANOVA or Friedman's

# totSC
# Deviates from normality
# no significant difference with ANOVA or Friedman's (lower p-values)

# attSC
# Deviates from normality only slightly
# no significant difference with ANOVA or Friedman's (lower p-value for ANOVA)

# decSC
# Deviates from normality
# no significant difference with ANOVA or Friedman's

# transSC
# Deviates from normality
# no significant difference with ANOVA or Friedman's (lower p-values)

# TC
# Deviates from normality
# significant difference between conditions according to Friedman test, Wilcoxon signed-rank test fails slightly

# totAmpFlat
# Deviates from normality
# no significant difference with ANOVA or Friedman's

# attAmpFlat
# Deviates from normality only slightly
# no significant difference with ANOVA or Friedman's

# decAmpFlat
# Deviates from normality
# no significant difference with ANOVA or Friedman's

# transAmpFlat
# Deviates from normality
# no significant difference with ANOVA or Friedman's

# totSpecFlat
# Deviates from normality
# no significant difference with ANOVA or Friedman's

# attSpecFlat
# Deviates from normality only slightly
# no significant difference with ANOVA or Friedman's

# decSpecFlat
# Deviates from normality
# no significant difference with ANOVA or Friedman's

# transSpecFlat
# Deviates from normality
# no significant difference with ANOVA or Friedman's

# crest
# Deviates from normality
# no significant difference with ANOVA or Friedman's (lower p-values)

##### Implementation #####

##### totDur #####

# Setup data frame
totDur <- read.csv("totDur.csv")
totDur <- totDur %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(totDur, 3)

# Get summary stats
totDur %>%
  group_by(Condition, Subject) %>%
  get_summary_stats(Value, type = "common")

# Create box plot
bxp <- ggboxplot(totDur, x = "Subject", y = "Value", color = "Condition", palette = "jco")
bxp

# Find extreme outliers
outliers <- totDur %>%
  group_by(Condition) %>%
  identify_outliers(Value)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
totDur <- totDur[!totDur$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
totDur %>%
  group_by(Condition) %>%
  shapiro_test(Value)

# Inspect QQ plot
ggqqplot(totDur, "Value", facet.by = "Condition")

# Aggregate by mean condition per subject
totDurMeans <- as_tibble(totDur %>%
  #group_by(Subject,Series,Condition) %>%
  group_by(Subject,Condition) %>%
  summarise_at(.vars = names(.)[6:7], .funs = c(median="median")))

# Remove unused columns
totDurMeans[,ncol(totDurMeans)] <- NULL

resultsAov <- anova_test(data = totDurMeans, dv = Value_median, wid = Subject, within = c(Condition))
get_anova_table(resultsAov)

resultsFriedman <- totDurMeans %>% friedman_test(Value_median ~ Condition | Subject)
resultsFriedman
totDurMeans %>% friedman_effsize(Value_median ~ Condition | Subject)

##### attDur #####

# Setup data frame
attDur <- read.csv("attDur.csv")
attDur <- attDur %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(attDur, 3)

# Get summary stats
attDur %>%
  group_by(Condition) %>%
  get_summary_stats(Value, type = "common")

# Create box plot
bxp <- ggboxplot(attDur, x = "Condition", y = "Value", add = "jitter")
bxp

# Find extreme outliers
outliers <- attDur %>%
  group_by(Condition) %>%
  identify_outliers(Value)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
attDur <- attDur[!attDur$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
attDur %>%
  group_by(Condition) %>%
  shapiro_test(Value)

# Inspect QQ plot
ggqqplot(attDur, "Value", facet.by = "Condition")

# Aggregate by mean condition per subject
attDurMeans <- as_tibble(attDur %>%
                           #group_by(Subject,Series,Condition) %>%
                           group_by(Subject,Condition) %>%
                           summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Remove unused columns
attDurMeans[,ncol(attDurMeans)] <- NULL

resultsAov <- anova_test(data = attDurMeans, dv = Value_mean, wid = Subject, within = c(Condition))
get_anova_table(resultsAov)

resultsFriedman <- attDurMeans %>% friedman_test(Value_mean ~ Condition | Subject)
resultsFriedman
attDurMeans %>% friedman_effsize(Value_mean ~ Condition | Subject)

##### decDur #####

# Setup data frame
decDur <- read.csv("decDur.csv")
decDur <- decDur %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(decDur, 3)

# Get summary stats
decDur %>%
  group_by(Condition) %>%
  get_summary_stats(Value, type = "common")

# Create box plot
bxp <- ggboxplot(decDur, x = "Condition", y = "Value", add = "jitter")
bxp

# Find extreme outliers
outliers <- decDur %>%
  group_by(Condition) %>%
  identify_outliers(Value)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
decDur <- decDur[!decDur$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
decDur %>%
  group_by(Condition) %>%
  shapiro_test(Value)

# Inspect QQ plot
ggqqplot(decDur, "Value", facet.by = "Condition")

# Aggregate by mean condition per subject
decDurMeans <- as_tibble(decDur %>%
                           #group_by(Subject,Series,Condition) %>%
                           group_by(Subject,Condition) %>%
                           summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Remove unused columns
decDurMeans[,ncol(decDurMeans)] <- NULL

resultsAov <- anova_test(data = decDurMeans, dv = Value_mean, wid = Subject, within = c(Condition))
get_anova_table(resultsAov)

resultsFriedman <- decDurMeans %>% friedman_test(Value_mean ~ Condition | Subject)
resultsFriedman
decDurMeans %>% friedman_effsize(Value_mean ~ Condition | Subject)

##### transDur #####

# Setup data frame
transDur <- read.csv("transDur.csv")
transDur <- transDur %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(transDur, 3)

# Get summary stats
transDur %>%
  group_by(Condition) %>%
  get_summary_stats(Value, type = "common")

# Create box plot
bxp <- ggboxplot(transDur, x = "Condition", y = "Value", add = "jitter")
bxp

# Find extreme outliers
outliers <- transDur %>%
  group_by(Condition) %>%
  identify_outliers(Value)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
transDur <- transDur[!transDur$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
transDur %>%
  group_by(Condition) %>%
  shapiro_test(Value)

# Inspect QQ plot
ggqqplot(transDur, "Value", facet.by = "Condition")

# Aggregate by mean condition per subject
transDurMeans <- as_tibble(transDur %>%
                           #group_by(Subject,Series,Condition) %>%
                           group_by(Subject,Condition) %>%
                           summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Remove unused columns
transDurMeans[,ncol(transDurMeans)] <- NULL

resultsAov <- anova_test(data = transDurMeans, dv = Value_mean, wid = Subject, within = c(Condition))
get_anova_table(resultsAov)

resultsFriedman <- transDurMeans %>% friedman_test(Value_mean ~ Condition | Subject)
resultsFriedman
transDurMeans %>% friedman_effsize(Value_mean ~ Condition | Subject)

# Pairwise comparison
pwc <- transDurMeans %>%
  wilcox_test(Value_mean ~ Condition, paired = TRUE, p.adjust.method = "bonferroni")
pwc

# Report
pwc <- pwc %>% add_xy_position(x = "Condition")
ggboxplot(transDurMeans, x = "Condition", y = "Value_mean", add = "point") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(resultsFriedman,  detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

##### LAT #####

# Setup data frame
LAT <- read.csv("LAT.csv")
LAT <- LAT %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(LAT, 3)

# Get summary stats
LAT %>%
  group_by(Condition) %>%
  get_summary_stats(Value, type = "common")

# Create box plot
bxp <- ggboxplot(LAT, x = "Condition", y = "Value", add = "jitter")
bxp

# Find extreme outliers
outliers <- LAT %>%
  group_by(Condition) %>%
  identify_outliers(Value)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
LAT <- LAT[!LAT$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
LAT %>%
  group_by(Condition) %>%
  shapiro_test(Value)

# Inspect QQ plot
ggqqplot(LAT, "Value", facet.by = "Condition")

# Aggregate by mean condition per subject
LATMeans <- as_tibble(LAT %>%
                             #group_by(Subject,Series,Condition) %>%
                             group_by(Subject,Condition) %>%
                             summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Remove unused columns
LATMeans[,ncol(LATMeans)] <- NULL

resultsAov <- anova_test(data = LATMeans, dv = Value_mean, wid = Subject, within = c(Condition))
get_anova_table(resultsAov)

resultsFriedman <- LATMeans %>% friedman_test(Value_mean ~ Condition | Subject)
resultsFriedman
LATMeans %>% friedman_effsize(Value_mean ~ Condition | Subject)

##### totSPL #####

# Setup data frame
totSPL <- read.csv("totSPL.csv")
totSPL <- totSPL %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(totSPL, 3)

# Get summary stats
totSPL %>%
  group_by(Condition) %>%
  get_summary_stats(Value, type = "common")

# Create box plot
bxp <- ggboxplot(totSPL, x = "Condition", y = "Value", add = "jitter")
bxp

# Find extreme outliers
outliers <- totSPL %>%
  group_by(Condition) %>%
  identify_outliers(Value)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
totSPL <- totSPL[!totSPL$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
totSPL %>%
  group_by(Condition) %>%
  shapiro_test(Value)

# Inspect QQ plot
ggqqplot(totSPL, "Value", facet.by = "Condition")

# Aggregate by mean condition per subject
totSPLMeans <- as_tibble(totSPL %>%
                        #group_by(Subject,Series,Condition) %>%
                        group_by(Subject,Condition) %>%
                        summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Remove unused columns
totSPLMeans[,ncol(totSPLMeans)] <- NULL

resultsAov <- anova_test(data = totSPLMeans, dv = Value_mean, wid = Subject, within = c(Condition))
get_anova_table(resultsAov)

resultsFriedman <- totSPLMeans %>% friedman_test(Value_mean ~ Condition | Subject)
resultsFriedman
totSPLMeans %>% friedman_effsize(Value_mean ~ Condition | Subject)

##### attSPL #####

# Setup data frame
attSPL <- read.csv("attSPL.csv")
attSPL <- attSPL %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(attSPL, 3)

# Get summary stats
attSPL %>%
  group_by(Condition) %>%
  get_summary_stats(Value, type = "common")

# Create box plot
bxp <- ggboxplot(attSPL, x = "Condition", y = "Value", add = "jitter")
bxp

# Find extreme outliers
outliers <- attSPL %>%
  group_by(Condition) %>%
  identify_outliers(Value)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
attSPL <- attSPL[!attSPL$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
attSPL %>%
  group_by(Condition) %>%
  shapiro_test(Value)

# Inspect QQ plot
ggqqplot(attSPL, "Value", facet.by = "Condition")

# Aggregate by mean condition per subject
attSPLMeans <- as_tibble(attSPL %>%
                           #group_by(Subject,Series,Condition) %>%
                           group_by(Subject,Condition) %>%
                           summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Remove unused columns
attSPLMeans[,ncol(attSPLMeans)] <- NULL

resultsAov <- anova_test(data = attSPLMeans, dv = Value_mean, wid = Subject, within = c(Condition))
get_anova_table(resultsAov)

resultsFriedman <- attSPLMeans %>% friedman_test(Value_mean ~ Condition | Subject)
resultsFriedman
attSPLMeans %>% friedman_effsize(Value_mean ~ Condition | Subject)

##### decSPL #####

# Setup data frame
decSPL <- read.csv("decSPL.csv")
decSPL <- decSPL %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(decSPL, 3)

# Get summary stats
decSPL %>%
  group_by(Condition) %>%
  get_summary_stats(Value, type = "common")

# Create box plot
bxp <- ggboxplot(decSPL, x = "Condition", y = "Value", add = "jitter")
bxp

# Find extreme outliers
outliers <- decSPL %>%
  group_by(Condition) %>%
  identify_outliers(Value)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
decSPL <- decSPL[!decSPL$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
decSPL %>%
  group_by(Condition) %>%
  shapiro_test(Value)

# Inspect QQ plot
ggqqplot(decSPL, "Value", facet.by = "Condition")

# Aggregate by mean condition per subject
decSPLMeans <- as_tibble(decSPL %>%
                           #group_by(Subject,Series,Condition) %>%
                           group_by(Subject,Condition) %>%
                           summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Remove unused columns
decSPLMeans[,ncol(decSPLMeans)] <- NULL

resultsAov <- anova_test(data = decSPLMeans, dv = Value_mean, wid = Subject, within = c(Condition))
get_anova_table(resultsAov)

resultsFriedman <- decSPLMeans %>% friedman_test(Value_mean ~ Condition | Subject)
resultsFriedman
decSPLMeans %>% friedman_effsize(Value_mean ~ Condition | Subject)

##### transSPL #####

# Setup data frame
transSPL <- read.csv("transSPL.csv")
transSPL <- transSPL %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(transSPL, 3)

# Get summary stats
transSPL %>%
  group_by(Condition) %>%
  get_summary_stats(Value, type = "common")

# Create box plot
bxp <- ggboxplot(transSPL, x = "Condition", y = "Value", add = "jitter")
bxp

# Find extreme outliers
outliers <- transSPL %>%
  group_by(Condition) %>%
  identify_outliers(Value)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
transSPL <- transSPL[!transSPL$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
transSPL %>%
  group_by(Condition) %>%
  shapiro_test(Value)

# Inspect QQ plot
ggqqplot(transSPL, "Value", facet.by = "Condition")

# Aggregate by mean condition per subject
transSPLMeans <- as_tibble(transSPL %>%
                           #group_by(Subject,Series,Condition) %>%
                           group_by(Subject,Condition) %>%
                           summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Remove unused columns
transSPLMeans[,ncol(transSPLMeans)] <- NULL

resultsAov <- anova_test(data = transSPLMeans, dv = Value_mean, wid = Subject, within = c(Condition))
get_anova_table(resultsAov)

resultsFriedman <- transSPLMeans %>% friedman_test(Value_mean ~ Condition | Subject)
resultsFriedman
transSPLMeans %>% friedman_effsize(Value_mean ~ Condition | Subject)

##### totSC #####

# Setup data frame
totSC <- read.csv("totSC.csv")
totSC <- totSC %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(totSC, 3)

# Get summary stats
totSC %>%
  group_by(Condition) %>%
  get_summary_stats(Value, type = "common")

# Create box plot
bxp <- ggboxplot(totSC, x = "Condition", y = "Value", add = "jitter")
bxp

# Find extreme outliers
outliers <- totSC %>%
  group_by(Condition) %>%
  identify_outliers(Value)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
totSC <- totSC[!totSC$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
totSC %>%
  group_by(Condition) %>%
  shapiro_test(Value)

# Inspect QQ plot
ggqqplot(totSC, "Value", facet.by = "Condition")

# Aggregate by mean condition per subject
totSCMeans <- as_tibble(totSC %>%
                             #group_by(Subject,Series,Condition) %>%
                             group_by(Subject,Condition) %>%
                             summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Remove unused columns
totSCMeans[,ncol(totSCMeans)] <- NULL

resultsAov <- anova_test(data = totSCMeans, dv = Value_mean, wid = Subject, within = c(Condition))
get_anova_table(resultsAov)

resultsFriedman <- totSCMeans %>% friedman_test(Value_mean ~ Condition | Subject)
resultsFriedman
totSCMeans %>% friedman_effsize(Value_mean ~ Condition | Subject)

##### attSC #####

# Setup data frame
attSC <- read.csv("attSC.csv")
attSC <- attSC %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(attSC, 3)

# Get summary stats
attSC %>%
  group_by(Condition) %>%
  get_summary_stats(Value, type = "common")

# Create box plot
bxp <- ggboxplot(attSC, x = "Condition", y = "Value", add = "jitter")
bxp

# Find extreme outliers
outliers <- attSC %>%
  group_by(Condition) %>%
  identify_outliers(Value)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
attSC <- attSC[!attSC$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
attSC %>%
  group_by(Condition) %>%
  shapiro_test(Value)

# Inspect QQ plot
ggqqplot(attSC, "Value", facet.by = "Condition")

# Aggregate by mean condition per subject
attSCMeans <- as_tibble(attSC %>%
                          #group_by(Subject,Series,Condition) %>%
                          group_by(Subject,Condition) %>%
                          summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Remove unused columns
attSCMeans[,ncol(attSCMeans)] <- NULL

resultsAov <- anova_test(data = attSCMeans, dv = Value_mean, wid = Subject, within = c(Condition))
get_anova_table(resultsAov)

resultsFriedman <- attSCMeans %>% friedman_test(Value_mean ~ Condition | Subject)
resultsFriedman
attSCMeans %>% friedman_effsize(Value_mean ~ Condition | Subject)

##### decSC #####

# Setup data frame
decSC <- read.csv("decSC.csv")
decSC <- decSC %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(decSC, 3)

# Get summary stats
decSC %>%
  group_by(Condition) %>%
  get_summary_stats(Value, type = "common")

# Create box plot
bxp <- ggboxplot(decSC, x = "Condition", y = "Value", add = "jitter")
bxp

# Find extreme outliers
outliers <- decSC %>%
  group_by(Condition) %>%
  identify_outliers(Value)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
decSC <- decSC[!decSC$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
decSC %>%
  group_by(Condition) %>%
  shapiro_test(Value)

# Inspect QQ plot
ggqqplot(decSC, "Value", facet.by = "Condition")

# Aggregate by mean condition per subject
decSCMeans <- as_tibble(decSC %>%
                          #group_by(Subject,Series,Condition) %>%
                          group_by(Subject,Condition) %>%
                          summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Remove unused columns
decSCMeans[,ncol(decSCMeans)] <- NULL

resultsAov <- anova_test(data = decSCMeans, dv = Value_mean, wid = Subject, within = c(Condition))
get_anova_table(resultsAov)

resultsFriedman <- decSCMeans %>% friedman_test(Value_mean ~ Condition | Subject)
resultsFriedman
decSCMeans %>% friedman_effsize(Value_mean ~ Condition | Subject)

##### transSC #####

# Setup data frame
transSC <- read.csv("transSC.csv")
transSC <- transSC %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(transSC, 3)

# Get summary stats
transSC %>%
  group_by(Condition) %>%
  get_summary_stats(Value, type = "common")

# Create box plot
bxp <- ggboxplot(transSC, x = "Condition", y = "Value", add = "jitter")
bxp

# Find extreme outliers
outliers <- transSC %>%
  group_by(Condition) %>%
  identify_outliers(Value)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
transSC <- transSC[!transSC$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
transSC %>%
  group_by(Condition) %>%
  shapiro_test(Value)

# Inspect QQ plot
ggqqplot(transSC, "Value", facet.by = "Condition")

# Aggregate by mean condition per subject
transSCMeans <- as_tibble(transSC %>%
                          #group_by(Subject,Series,Condition) %>%
                          group_by(Subject,Condition) %>%
                          summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Remove unused columns
transSCMeans[,ncol(transSCMeans)] <- NULL

resultsAov <- anova_test(data = transSCMeans, dv = Value_mean, wid = Subject, within = c(Condition))
get_anova_table(resultsAov)

resultsFriedman <- transSCMeans %>% friedman_test(Value_mean ~ Condition | Subject)
resultsFriedman
transSCMeans %>% friedman_effsize(Value_mean ~ Condition | Subject)

##### TC #####

# Setup data frame
TC <- read.csv("TC.csv")
TC <- TC %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(TC, 3)

# Get summary stats
TC %>%
  group_by(Condition) %>%
  get_summary_stats(Value, type = "common")

# Create box plot
bxp <- ggboxplot(TC, x = "Condition", y = "Value", add = "jitter")
bxp

# Find extreme outliers
outliers <- TC %>%
  group_by(Condition) %>%
  identify_outliers(Value)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
TC <- TC[!TC$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
TC %>%
  group_by(Condition) %>%
  shapiro_test(Value)

# Inspect QQ plot
ggqqplot(TC, "Value", facet.by = "Condition")

# Aggregate by mean condition per subject
TCMeans <- as_tibble(TC %>%
                            #group_by(Subject,Series,Condition) %>%
                            group_by(Subject,Condition) %>%
                            summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Remove unused columns
TCMeans[,ncol(TCMeans)] <- NULL

resultsAov <- anova_test(data = TCMeans, dv = Value_mean, wid = Subject, within = c(Condition))
get_anova_table(resultsAov)

resultsFriedman <- TCMeans %>% friedman_test(Value_mean ~ Condition | Subject)
resultsFriedman
TCMeans %>% friedman_effsize(Value_mean ~ Condition | Subject)

# Pairwise comparison
pwc <- TCMeans %>%
  wilcox_test(Value_mean ~ Condition, paired = TRUE, p.adjust.method = "bonferroni")
pwc

# Report
pwc <- pwc %>% add_xy_position(x = "Condition")
ggboxplot(TCMeans, x = "Condition", y = "Value_mean", add = "point") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(resultsFriedman,  detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

##### totAmpFlat #####

# Setup data frame
totAmpFlat <- read.csv("totAmpFlat.csv")
totAmpFlat <- totAmpFlat %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(totAmpFlat, 3)

# Get summary stats
totAmpFlat %>%
  group_by(Condition) %>%
  get_summary_stats(Value, type = "common")

# Create box plot
bxp <- ggboxplot(totAmpFlat, x = "Condition", y = "Value", add = "jitter")
bxp

# Find extreme outliers
outliers <- totAmpFlat %>%
  group_by(Condition) %>%
  identify_outliers(Value)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
totAmpFlat <- totAmpFlat[!totAmpFlat$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
totAmpFlat %>%
  group_by(Condition) %>%
  shapiro_test(Value)

# Inspect QQ plot
ggqqplot(totAmpFlat, "Value", facet.by = "Condition")

# Aggregate by mean condition per subject
totAmpFlatMeans <- as_tibble(totAmpFlat %>%
                       #group_by(Subject,Series,Condition) %>%
                       group_by(Subject,Condition) %>%
                       summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Remove unused columns
totAmpFlatMeans[,ncol(totAmpFlatMeans)] <- NULL

resultsAov <- anova_test(data = totAmpFlatMeans, dv = Value_mean, wid = Subject, within = c(Condition))
get_anova_table(resultsAov)

resultsFriedman <- totAmpFlatMeans %>% friedman_test(Value_mean ~ Condition | Subject)
resultsFriedman
totAmpFlatMeans %>% friedman_effsize(Value_mean ~ Condition | Subject)

##### attAmpFlat #####

# Setup data frame
attAmpFlat <- read.csv("attAmpFlat.csv")
attAmpFlat <- attAmpFlat %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(attAmpFlat, 3)

# Get summary stats
attAmpFlat %>%
  group_by(Condition) %>%
  get_summary_stats(Value, type = "common")

# Create box plot
bxp <- ggboxplot(attAmpFlat, x = "Condition", y = "Value", add = "jitter")
bxp

# Find extreme outliers
outliers <- attAmpFlat %>%
  group_by(Condition) %>%
  identify_outliers(Value)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
attAmpFlat <- attAmpFlat[!attAmpFlat$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
attAmpFlat %>%
  group_by(Condition) %>%
  shapiro_test(Value)

# Inspect QQ plot
ggqqplot(attAmpFlat, "Value", facet.by = "Condition")

# Aggregate by mean condition per subject
attAmpFlatMeans <- as_tibble(attAmpFlat %>%
                               #group_by(Subject,Series,Condition) %>%
                               group_by(Subject,Condition) %>%
                               summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Remove unused columns
attAmpFlatMeans[,ncol(attAmpFlatMeans)] <- NULL

resultsAov <- anova_test(data = attAmpFlatMeans, dv = Value_mean, wid = Subject, within = c(Condition))
get_anova_table(resultsAov)

resultsFriedman <- attAmpFlatMeans %>% friedman_test(Value_mean ~ Condition | Subject)
resultsFriedman
attAmpFlatMeans %>% friedman_effsize(Value_mean ~ Condition | Subject)

##### decAmpFlat #####

# Setup data frame
decAmpFlat <- read.csv("decAmpFlat.csv")
decAmpFlat <- decAmpFlat %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(decAmpFlat, 3)

# Get summary stats
decAmpFlat %>%
  group_by(Condition) %>%
  get_summary_stats(Value, type = "common")

# Create box plot
bxp <- ggboxplot(decAmpFlat, x = "Condition", y = "Value", add = "jitter")
bxp

# Find extreme outliers
outliers <- decAmpFlat %>%
  group_by(Condition) %>%
  identify_outliers(Value)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
decAmpFlat <- decAmpFlat[!decAmpFlat$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
decAmpFlat %>%
  group_by(Condition) %>%
  shapiro_test(Value)

# Inspect QQ plot
ggqqplot(decAmpFlat, "Value", facet.by = "Condition")

# Aggregate by mean condition per subject
decAmpFlatMeans <- as_tibble(decAmpFlat %>%
                               #group_by(Subject,Series,Condition) %>%
                               group_by(Subject,Condition) %>%
                               summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Remove unused columns
decAmpFlatMeans[,ncol(decAmpFlatMeans)] <- NULL

resultsAov <- anova_test(data = decAmpFlatMeans, dv = Value_mean, wid = Subject, within = c(Condition))
get_anova_table(resultsAov)

resultsFriedman <- decAmpFlatMeans %>% friedman_test(Value_mean ~ Condition | Subject)
resultsFriedman
decAmpFlatMeans %>% friedman_effsize(Value_mean ~ Condition | Subject)

##### transAmpFlat #####

# Setup data frame
transAmpFlat <- read.csv("transAmpFlat.csv")
transAmpFlat <- transAmpFlat %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(transAmpFlat, 3)

# Get summary stats
transAmpFlat %>%
  group_by(Condition) %>%
  get_summary_stats(Value, type = "common")

# Create box plot
bxp <- ggboxplot(transAmpFlat, x = "Condition", y = "Value", add = "jitter")
bxp

# Find extreme outliers
outliers <- transAmpFlat %>%
  group_by(Condition) %>%
  identify_outliers(Value)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
transAmpFlat <- transAmpFlat[!transAmpFlat$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
transAmpFlat %>%
  group_by(Condition) %>%
  shapiro_test(Value)

# Inspect QQ plot
ggqqplot(transAmpFlat, "Value", facet.by = "Condition")

# Aggregate by mean condition per subject
transAmpFlatMeans <- as_tibble(transAmpFlat %>%
                               #group_by(Subject,Series,Condition) %>%
                               group_by(Subject,Condition) %>%
                               summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Remove unused columns
transAmpFlatMeans[,ncol(transAmpFlatMeans)] <- NULL

resultsAov <- anova_test(data = transAmpFlatMeans, dv = Value_mean, wid = Subject, within = c(Condition))
get_anova_table(resultsAov)

resultsFriedman <- transAmpFlatMeans %>% friedman_test(Value_mean ~ Condition | Subject)
resultsFriedman
transAmpFlatMeans %>% friedman_effsize(Value_mean ~ Condition | Subject)

##### totSpecFlat #####

# Setup data frame
totSpecFlat <- read.csv("totSpecFlat.csv")
totSpecFlat <- totSpecFlat %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(totSpecFlat, 3)

# Get summary stats
totSpecFlat %>%
  group_by(Condition) %>%
  get_summary_stats(Value, type = "common")

# Create box plot
bxp <- ggboxplot(totSpecFlat, x = "Condition", y = "Value", add = "jitter")
bxp

# Find extreme outliers
outliers <- totSpecFlat %>%
  group_by(Condition) %>%
  identify_outliers(Value)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
totSpecFlat <- totSpecFlat[!totSpecFlat$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sSpecle size)
totSpecFlat %>%
  group_by(Condition) %>%
  shapiro_test(Value)

# Inspect QQ plot
ggqqplot(totSpecFlat, "Value", facet.by = "Condition")

# Aggregate by mean condition per subject
totSpecFlatMeans <- as_tibble(totSpecFlat %>%
                               #group_by(Subject,Series,Condition) %>%
                               group_by(Subject,Condition) %>%
                               summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Remove unused columns
totSpecFlatMeans[,ncol(totSpecFlatMeans)] <- NULL

resultsAov <- anova_test(data = totSpecFlatMeans, dv = Value_mean, wid = Subject, within = c(Condition))
get_anova_table(resultsAov)

resultsFriedman <- totSpecFlatMeans %>% friedman_test(Value_mean ~ Condition | Subject)
resultsFriedman
totSpecFlatMeans %>% friedman_effsize(Value_mean ~ Condition | Subject)

##### attSpecFlat #####

# Setup data frame
attSpecFlat <- read.csv("attSpecFlat.csv")
attSpecFlat <- attSpecFlat %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(attSpecFlat, 3)

# Get summary stats
attSpecFlat %>%
  group_by(Condition) %>%
  get_summary_stats(Value, type = "common")

# Create box plot
bxp <- ggboxplot(attSpecFlat, x = "Condition", y = "Value", add = "jitter")
bxp

# Find extreme outliers
outliers <- attSpecFlat %>%
  group_by(Condition) %>%
  identify_outliers(Value)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
attSpecFlat <- attSpecFlat[!attSpecFlat$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sSpecle size)
attSpecFlat %>%
  group_by(Condition) %>%
  shapiro_test(Value)

# Inspect QQ plot
ggqqplot(attSpecFlat, "Value", facet.by = "Condition")

# Aggregate by mean condition per subject
attSpecFlatMeans <- as_tibble(attSpecFlat %>%
                               #group_by(Subject,Series,Condition) %>%
                               group_by(Subject,Condition) %>%
                               summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Remove unused columns
attSpecFlatMeans[,ncol(attSpecFlatMeans)] <- NULL

resultsAov <- anova_test(data = attSpecFlatMeans, dv = Value_mean, wid = Subject, within = c(Condition))
get_anova_table(resultsAov)

resultsFriedman <- attSpecFlatMeans %>% friedman_test(Value_mean ~ Condition | Subject)
resultsFriedman
attSpecFlatMeans %>% friedman_effsize(Value_mean ~ Condition | Subject)

##### decSpecFlat #####

# Setup data frame
decSpecFlat <- read.csv("decSpecFlat.csv")
decSpecFlat <- decSpecFlat %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(decSpecFlat, 3)

# Get summary stats
decSpecFlat %>%
  group_by(Condition) %>%
  get_summary_stats(Value, type = "common")

# Create box plot
bxp <- ggboxplot(decSpecFlat, x = "Condition", y = "Value", add = "jitter")
bxp

# Find extreme outliers
outliers <- decSpecFlat %>%
  group_by(Condition) %>%
  identify_outliers(Value)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
decSpecFlat <- decSpecFlat[!decSpecFlat$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sSpecle size)
decSpecFlat %>%
  group_by(Condition) %>%
  shapiro_test(Value)

# Inspect QQ plot
ggqqplot(decSpecFlat, "Value", facet.by = "Condition")

# Aggregate by mean condition per subject
decSpecFlatMeans <- as_tibble(decSpecFlat %>%
                               #group_by(Subject,Series,Condition) %>%
                               group_by(Subject,Condition) %>%
                               summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Remove unused columns
decSpecFlatMeans[,ncol(decSpecFlatMeans)] <- NULL

resultsAov <- anova_test(data = decSpecFlatMeans, dv = Value_mean, wid = Subject, within = c(Condition))
get_anova_table(resultsAov)

resultsFriedman <- decSpecFlatMeans %>% friedman_test(Value_mean ~ Condition | Subject)
resultsFriedman
decSpecFlatMeans %>% friedman_effsize(Value_mean ~ Condition | Subject)

##### transSpecFlat #####

# Setup data frame
transSpecFlat <- read.csv("transSpecFlat.csv")
transSpecFlat <- transSpecFlat %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(transSpecFlat, 3)

# Get summary stats
transSpecFlat %>%
  group_by(Condition) %>%
  get_summary_stats(Value, type = "common")

# Create box plot
bxp <- ggboxplot(transSpecFlat, x = "Condition", y = "Value", add = "jitter")
bxp

# Find extreme outliers
outliers <- transSpecFlat %>%
  group_by(Condition) %>%
  identify_outliers(Value)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
transSpecFlat <- transSpecFlat[!transSpecFlat$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sSpecle size)
transSpecFlat %>%
  group_by(Condition) %>%
  shapiro_test(Value)

# Inspect QQ plot
ggqqplot(transSpecFlat, "Value", facet.by = "Condition")

# Aggregate by mean condition per subject
transSpecFlatMeans <- as_tibble(transSpecFlat %>%
                                 #group_by(Subject,Series,Condition) %>%
                                 group_by(Subject,Condition) %>%
                                 summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Remove unused columns
transSpecFlatMeans[,ncol(transSpecFlatMeans)] <- NULL

resultsAov <- anova_test(data = transSpecFlatMeans, dv = Value_mean, wid = Subject, within = c(Condition))
get_anova_table(resultsAov)

resultsFriedman <- transSpecFlatMeans %>% friedman_test(Value_mean ~ Condition | Subject)
resultsFriedman
transSpecFlatMeans %>% friedman_effsize(Value_mean ~ Condition | Subject)

##### crest #####

# Setup data frame
crest <- read.csv("crest.csv")
crest <- crest %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(crest, 3)

# Get summary stats
crest %>%
  group_by(Condition) %>%
  get_summary_stats(Value, type = "common")

# Create box plot
bxp <- ggboxplot(crest, x = "Condition", y = "Value", add = "jitter")
bxp

# Find extreme outliers
outliers <- crest %>%
  group_by(Condition) %>%
  identify_outliers(Value)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
crest <- crest[!crest$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sSpecle size)
crest %>%
  group_by(Condition) %>%
  shapiro_test(Value)

# Inspect QQ plot
ggqqplot(crest, "Value", facet.by = "Condition")

# Aggregate by mean condition per subject
crestMeans <- as_tibble(crest %>%
                                  #group_by(Subject,Series,Condition) %>%
                                  group_by(Subject,Condition) %>%
                                  summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Remove unused columns
crestMeans[,ncol(crestMeans)] <- NULL

resultsAov <- anova_test(data = crestMeans, dv = Value_mean, wid = Subject, within = c(Condition))
get_anova_table(resultsAov)

resultsFriedman <- crestMeans %>% friedman_test(Value_mean ~ Condition | Subject)
resultsFriedman
crestMeans %>% friedman_effsize(Value_mean ~ Condition | Subject)

