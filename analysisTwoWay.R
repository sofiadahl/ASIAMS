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

# totDur
# Significant effect of series

# transDur
# No significant effect

# decDur
# Significant effect of series

# transDur
# Significant effect of series

# LAT
# No significant effect

# totSPL
# Significant effect of series

# attSPL
# No significant effect

# decSPL
# Significant effect of series

# transSPL
# Significant effect of series

# totSC
# No significant effect

# attSC
# Significant effect of series

# decSC
# No significant effect

# transSC
# No significant effect

# TC
# Significant effect of series

# totAmpFlat
# Significant effect of series

# attAmpFlat
# No significant effect

# decAmpFlat
# No significant effect

# transAmpFlat
# No significant effect

# totSpecFlat
# Significant effect of series

# attSpecFlat
# Significant effect of series

# decSpecFlat
# No significant effect

# crest
# No significant effect

# crest
# Significant effect of series

##### Implementation #####

##### totDur #####

# Setup data frame
totDur <- read.csv("totDur.csv")
totDur <- totDur[-c(1:283),] %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(totDur, 3)

# Aggregate by mean condition per subject and series
totDurMeans <- as_tibble(totDur %>%
                           group_by(Subject,Series,Condition) %>%
                           summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Get summary stats
totDurMeans %>%
  group_by(Condition, Series) %>%
  get_summary_stats(Value_mean, type = "common")

# Create box plot
bxp <- ggboxplot(totDurMeans, x = "Series", y = "Value_mean", color = "Condition", palette = "jco")
bxp

# Find extreme outliers
outliers <- totDurMeans %>%
  group_by(Condition, Series) %>%
  identify_outliers(Value_mean)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
totDur <- totDur[!totDur$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
totDurMeans %>%
  group_by(Condition,Series) %>%
  shapiro_test(Value_mean)

# Inspect QQ plots
ggqqplot(totDurMeans, "Value_mean", ggtheme = theme_bw()) +
  facet_grid(Series ~ Condition, labeller = "label_both")

# Remove unused columns
totDurMeans[,ncol(totDurMeans)] <- NULL

resultsAov <- anova_test(data = totDurMeans, dv = Value_mean, wid = Subject, within = c(Series, Condition))
get_anova_table(resultsAov)

##### attDur #####

# Setup data frame
attDur <- read.csv("attDur.csv")
attDur <- attDur[-c(1:283),] %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(attDur, 3)

# Aggregate by mean condition per subject and series
attDurMeans <- as_tibble(attDur %>%
                           group_by(Subject,Series,Condition) %>%
                           summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Get summary stats
attDurMeans %>%
  group_by(Condition, Series) %>%
  get_summary_stats(Value_mean, type = "common")

# Create box plot
bxp <- ggboxplot(attDurMeans, x = "Series", y = "Value_mean", color = "Condition", palette = "jco")
bxp

# Find extreme outliers
outliers <- attDurMeans %>%
  group_by(Condition, Series) %>%
  identify_outliers(Value_mean)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
attDur <- attDur[!attDur$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
attDurMeans %>%
  group_by(Condition,Series) %>%
  shapiro_test(Value_mean)

# Inspect QQ plots
ggqqplot(attDurMeans, "Value_mean", ggtheme = theme_bw()) +
  facet_grid(Series ~ Condition, labeller = "label_both")

# Remove unused columns
attDurMeans[,ncol(attDurMeans)] <- NULL

resultsAov <- anova_test(data = attDurMeans, dv = Value_mean, wid = Subject, within = c(Series, Condition))
get_anova_table(resultsAov)

##### decDur #####

# Setup data frame
decDur <- read.csv("decDur.csv")
decDur <- decDur[-c(1:283),] %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(decDur, 3)

# Aggregate by mean condition per subject and series
decDurMeans <- as_tibble(decDur %>%
                           group_by(Subject,Series,Condition) %>%
                           summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Get summary stats
decDurMeans %>%
  group_by(Condition, Series) %>%
  get_summary_stats(Value_mean, type = "common")

# Create box plot
bxp <- ggboxplot(decDurMeans, x = "Series", y = "Value_mean", color = "Condition", palette = "jco")
bxp

# Find extreme outliers
outliers <- decDurMeans %>%
  group_by(Condition, Series) %>%
  identify_outliers(Value_mean)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
decDur <- decDur[!decDur$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
decDurMeans %>%
  group_by(Condition,Series) %>%
  shapiro_test(Value_mean)

# Inspect QQ plots
ggqqplot(decDurMeans, "Value_mean", ggtheme = theme_bw()) +
  facet_grid(Series ~ Condition, labeller = "label_both")

# Remove unused columns
decDurMeans[,ncol(decDurMeans)] <- NULL

resultsAov <- anova_test(data = decDurMeans, dv = Value_mean, wid = Subject, within = c(Series, Condition))
get_anova_table(resultsAov)

##### transDur #####

# Setup data frame
transDur <- read.csv("transDur.csv")
transDur <- transDur[-c(1:283),] %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(transDur, 3)

# Aggregate by mean condition per subject and series
transDurMeans <- as_tibble(transDur %>%
                           group_by(Subject,Series,Condition) %>%
                           summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Get summary stats
transDurMeans %>%
  group_by(Condition, Series) %>%
  get_summary_stats(Value_mean, type = "common")

# Create box plot
bxp <- ggboxplot(transDurMeans, x = "Series", y = "Value_mean", color = "Condition", palette = "jco")
bxp

# Find extreme outliers
outliers <- transDurMeans %>%
  group_by(Condition, Series) %>%
  identify_outliers(Value_mean)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
transDur <- transDur[!transDur$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
transDurMeans %>%
  group_by(Condition,Series) %>%
  shapiro_test(Value_mean)

# Inspect QQ plots
ggqqplot(transDurMeans, "Value_mean", ggtheme = theme_bw()) +
  facet_grid(Series ~ Condition, labeller = "label_both")

# Remove unused columns
transDurMeans[,ncol(transDurMeans)] <- NULL

resultsAov <- anova_test(data = transDurMeans, dv = Value_mean, wid = Subject, within = c(Series, Condition))
get_anova_table(resultsAov)

##### LAT #####

# Setup data frame
LAT <- read.csv("LAT.csv")
LAT <- LAT[-c(1:283),] %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(LAT, 3)

# Aggregate by mean condition per subject and series
LATMeans <- as_tibble(LAT %>%
                             group_by(Subject,Series,Condition) %>%
                             summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Get summary stats
LATMeans %>%
  group_by(Condition, Series) %>%
  get_summary_stats(Value_mean, type = "common")

# Create box plot
bxp <- ggboxplot(LATMeans, x = "Series", y = "Value_mean", color = "Condition", palette = "jco")
bxp

# Find extreme outliers
outliers <- LATMeans %>%
  group_by(Condition, Series) %>%
  identify_outliers(Value_mean)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
LAT <- LAT[!LAT$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
LATMeans %>%
  group_by(Condition,Series) %>%
  shapiro_test(Value_mean)

# Inspect QQ plots
ggqqplot(LATMeans, "Value_mean", ggtheme = theme_bw()) +
  facet_grid(Series ~ Condition, labeller = "label_both")

# Remove unused columns
LATMeans[,ncol(LATMeans)] <- NULL

resultsAov <- anova_test(data = LATMeans, dv = Value_mean, wid = Subject, within = c(Series, Condition))
get_anova_table(resultsAov)

##### totSPL #####

# Setup data frame
totSPL <- read.csv("totSPL.csv")
totSPL <- totSPL[-c(1:283),] %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(totSPL, 3)

# Aggregate by mean condition per subject and series
totSPLMeans <- as_tibble(totSPL %>%
                           group_by(Subject,Series,Condition) %>%
                           summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Get summary stats
totSPLMeans %>%
  group_by(Condition, Series) %>%
  get_summary_stats(Value_mean, type = "common")

# Create box plot
bxp <- ggboxplot(totSPLMeans, x = "Series", y = "Value_mean", color = "Condition", palette = "jco")
bxp

# Find extreme outliers
outliers <- totSPLMeans %>%
  group_by(Condition, Series) %>%
  identify_outliers(Value_mean)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
totSPL <- totSPL[!totSPL$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
totSPLMeans %>%
  group_by(Condition,Series) %>%
  shapiro_test(Value_mean)

# Inspect QQ plots
ggqqplot(totSPLMeans, "Value_mean", ggtheme = theme_bw()) +
  facet_grid(Series ~ Condition, labeller = "label_both")

# Remove unused columns
totSPLMeans[,ncol(totSPLMeans)] <- NULL

resultsAov <- anova_test(data = totSPLMeans, dv = Value_mean, wid = Subject, within = c(Series, Condition))
get_anova_table(resultsAov)

##### attSPL #####

# Setup data frame
attSPL <- read.csv("attSPL.csv")
attSPL <- attSPL[-c(1:283),] %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(attSPL, 3)

# Aggregate by mean condition per subject and series
attSPLMeans <- as_tibble(attSPL %>%
                           group_by(Subject,Series,Condition) %>%
                           summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Get summary stats
attSPLMeans %>%
  group_by(Condition, Series) %>%
  get_summary_stats(Value_mean, type = "common")

# Create box plot
bxp <- ggboxplot(attSPLMeans, x = "Series", y = "Value_mean", color = "Condition", palette = "jco")
bxp

# Find extreme outliers
outliers <- attSPLMeans %>%
  group_by(Condition, Series) %>%
  identify_outliers(Value_mean)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
attSPL <- attSPL[!attSPL$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
attSPLMeans %>%
  group_by(Condition,Series) %>%
  shapiro_test(Value_mean)

# Inspect QQ plots
ggqqplot(attSPLMeans, "Value_mean", ggtheme = theme_bw()) +
  facet_grid(Series ~ Condition, labeller = "label_both")

# Remove unused columns
attSPLMeans[,ncol(attSPLMeans)] <- NULL

resultsAov <- anova_test(data = attSPLMeans, dv = Value_mean, wid = Subject, within = c(Series, Condition))
get_anova_table(resultsAov)

##### decSPL #####

# Setup data frame
decSPL <- read.csv("decSPL.csv")
decSPL <- decSPL[-c(1:283),] %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(decSPL, 3)

# Aggregate by mean condition per subject and series
decSPLMeans <- as_tibble(decSPL %>%
                           group_by(Subject,Series,Condition) %>%
                           summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Get summary stats
decSPLMeans %>%
  group_by(Condition, Series) %>%
  get_summary_stats(Value_mean, type = "common")

# Create box plot
bxp <- ggboxplot(decSPLMeans, x = "Series", y = "Value_mean", color = "Condition", palette = "jco")
bxp

# Find extreme outliers
outliers <- decSPLMeans %>%
  group_by(Condition, Series) %>%
  identify_outliers(Value_mean)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
decSPL <- decSPL[!decSPL$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
decSPLMeans %>%
  group_by(Condition,Series) %>%
  shapiro_test(Value_mean)

# Inspect QQ plots
ggqqplot(decSPLMeans, "Value_mean", ggtheme = theme_bw()) +
  facet_grid(Series ~ Condition, labeller = "label_both")

# Remove unused columns
decSPLMeans[,ncol(decSPLMeans)] <- NULL

resultsAov <- anova_test(data = decSPLMeans, dv = Value_mean, wid = Subject, within = c(Series, Condition))
get_anova_table(resultsAov)

##### transSPL #####

# Setup data frame
transSPL <- read.csv("transSPL.csv")
transSPL <- transSPL[-c(1:283),] %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(transSPL, 3)

# Aggregate by mean condition per subject and series
transSPLMeans <- as_tibble(transSPL %>%
                             group_by(Subject,Series,Condition) %>%
                             summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Get summary stats
transSPLMeans %>%
  group_by(Condition, Series) %>%
  get_summary_stats(Value_mean, type = "common")

# Create box plot
bxp <- ggboxplot(transSPLMeans, x = "Series", y = "Value_mean", color = "Condition", palette = "jco")
bxp

# Find extreme outliers
outliers <- transSPLMeans %>%
  group_by(Condition, Series) %>%
  identify_outliers(Value_mean)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
transSPL <- transSPL[!transSPL$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
transSPLMeans %>%
  group_by(Condition,Series) %>%
  shapiro_test(Value_mean)

# Inspect QQ plots
ggqqplot(transSPLMeans, "Value_mean", ggtheme = theme_bw()) +
  facet_grid(Series ~ Condition, labeller = "label_both")

# Remove unused columns
transSPLMeans[,ncol(transSPLMeans)] <- NULL

resultsAov <- anova_test(data = transSPLMeans, dv = Value_mean, wid = Subject, within = c(Series, Condition))
get_anova_table(resultsAov)

##### totSC #####

# Setup data frame
totSC <- read.csv("totSC.csv")
totSC <- totSC[-c(1:283),] %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(totSC, 3)

# Aggregate by mean condition per subject and series
totSCMeans <- as_tibble(totSC %>%
                           group_by(Subject,Series,Condition) %>%
                           summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Get summary stats
totSCMeans %>%
  group_by(Condition, Series) %>%
  get_summary_stats(Value_mean, type = "common")

# Create box plot
bxp <- ggboxplot(totSCMeans, x = "Series", y = "Value_mean", color = "Condition", palette = "jco")
bxp

# Find extreme outliers
outliers <- totSCMeans %>%
  group_by(Condition, Series) %>%
  identify_outliers(Value_mean)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
totSC <- totSC[!totSC$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
totSCMeans %>%
  group_by(Condition,Series) %>%
  shapiro_test(Value_mean)

# Inspect QQ plots
ggqqplot(totSCMeans, "Value_mean", ggtheme = theme_bw()) +
  facet_grid(Series ~ Condition, labeller = "label_both")

# Remove unused columns
totSCMeans[,ncol(totSCMeans)] <- NULL

resultsAov <- anova_test(data = totSCMeans, dv = Value_mean, wid = Subject, within = c(Series, Condition))
get_anova_table(resultsAov)

##### attSC #####

# Setup data frame
attSC <- read.csv("attSC.csv")
attSC <- attSC[-c(1:283),] %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(attSC, 3)

# Aggregate by mean condition per subject and series
attSCMeans <- as_tibble(attSC %>%
                           group_by(Subject,Series,Condition) %>%
                           summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Get summary stats
attSCMeans %>%
  group_by(Condition, Series) %>%
  get_summary_stats(Value_mean, type = "common")

# Create box plot
bxp <- ggboxplot(attSCMeans, x = "Series", y = "Value_mean", color = "Condition", palette = "jco")
bxp

# Find extreme outliers
outliers <- attSCMeans %>%
  group_by(Condition, Series) %>%
  identify_outliers(Value_mean)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
attSC <- attSC[!attSC$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
attSCMeans %>%
  group_by(Condition,Series) %>%
  shapiro_test(Value_mean)

# Inspect QQ plots
ggqqplot(attSCMeans, "Value_mean", ggtheme = theme_bw()) +
  facet_grid(Series ~ Condition, labeller = "label_both")

# Remove unused columns
attSCMeans[,ncol(attSCMeans)] <- NULL

resultsAov <- anova_test(data = attSCMeans, dv = Value_mean, wid = Subject, within = c(Series, Condition))
get_anova_table(resultsAov)

##### decSC #####

# Setup data frame
decSC <- read.csv("decSC.csv")
decSC <- decSC[-c(1:283),] %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(decSC, 3)

# Aggregate by mean condition per subject and series
decSCMeans <- as_tibble(decSC %>%
                           group_by(Subject,Series,Condition) %>%
                           summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Get summary stats
decSCMeans %>%
  group_by(Condition, Series) %>%
  get_summary_stats(Value_mean, type = "common")

# Create box plot
bxp <- ggboxplot(decSCMeans, x = "Series", y = "Value_mean", color = "Condition", palette = "jco")
bxp

# Find extreme outliers
outliers <- decSCMeans %>%
  group_by(Condition, Series) %>%
  identify_outliers(Value_mean)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
decSC <- decSC[!decSC$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
decSCMeans %>%
  group_by(Condition,Series) %>%
  shapiro_test(Value_mean)

# Inspect QQ plots
ggqqplot(decSCMeans, "Value_mean", ggtheme = theme_bw()) +
  facet_grid(Series ~ Condition, labeller = "label_both")

# Remove unused columns
decSCMeans[,ncol(decSCMeans)] <- NULL

resultsAov <- anova_test(data = decSCMeans, dv = Value_mean, wid = Subject, within = c(Series, Condition))
get_anova_table(resultsAov)

##### transSC #####

# Setup data frame
transSC <- read.csv("transSC.csv")
transSC <- transSC[-c(1:283),] %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(transSC, 3)

# Aggregate by mean condition per subject and series
transSCMeans <- as_tibble(transSC %>%
                             group_by(Subject,Series,Condition) %>%
                             summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Get summary stats
transSCMeans %>%
  group_by(Condition, Series) %>%
  get_summary_stats(Value_mean, type = "common")

# Create box plot
bxp <- ggboxplot(transSCMeans, x = "Series", y = "Value_mean", color = "Condition", palette = "jco")
bxp

# Find extreme outliers
outliers <- transSCMeans %>%
  group_by(Condition, Series) %>%
  identify_outliers(Value_mean)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
transSC <- transSC[!transSC$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
transSCMeans %>%
  group_by(Condition,Series) %>%
  shapiro_test(Value_mean)

# Inspect QQ plots
ggqqplot(transSCMeans, "Value_mean", ggtheme = theme_bw()) +
  facet_grid(Series ~ Condition, labeller = "label_both")

# Remove unused columns
transSCMeans[,ncol(transSCMeans)] <- NULL

resultsAov <- anova_test(data = transSCMeans, dv = Value_mean, wid = Subject, within = c(Series, Condition))
get_anova_table(resultsAov)

##### TC #####

# Setup data frame
TC <- read.csv("TC.csv")
TC <- TC[-c(1:283),] %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(TC, 3)

# Aggregate by mean condition per subject and series
TCMeans <- as_tibble(TC %>%
                            group_by(Subject,Series,Condition) %>%
                            summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Get summary stats
TCMeans %>%
  group_by(Condition, Series) %>%
  get_summary_stats(Value_mean, type = "common")

# Create box plot
bxp <- ggboxplot(TCMeans, x = "Series", y = "Value_mean", color = "Condition", palette = "jco")
bxp

# Find extreme outliers
outliers <- TCMeans %>%
  group_by(Condition, Series) %>%
  identify_outliers(Value_mean)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
TC <- TC[!TC$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
TCMeans %>%
  group_by(Condition,Series) %>%
  shapiro_test(Value_mean)

# Inspect QQ plots
ggqqplot(TCMeans, "Value_mean", ggtheme = theme_bw()) +
  facet_grid(Series ~ Condition, labeller = "label_both")

# Remove unused columns
TCMeans[,ncol(TCMeans)] <- NULL

resultsAov <- anova_test(data = TCMeans, dv = Value_mean, wid = Subject, within = c(Series, Condition))
get_anova_table(resultsAov)

##### totAmpFlat #####

# Setup data frame
totAmpFlat <- read.csv("totAmpFlat.csv")
totAmpFlat <- totAmpFlat[-c(1:283),] %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(totAmpFlat, 3)

# Aggregate by mean condition per subject and series
totAmpFlatMeans <- as_tibble(totAmpFlat %>%
                          group_by(Subject,Series,Condition) %>%
                          summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Get summary stats
totAmpFlatMeans %>%
  group_by(Condition, Series) %>%
  get_summary_stats(Value_mean, type = "common")

# Create box plot
bxp <- ggboxplot(totAmpFlatMeans, x = "Series", y = "Value_mean", color = "Condition", palette = "jco")
bxp

# Find extreme outliers
outliers <- totAmpFlatMeans %>%
  group_by(Condition, Series) %>%
  identify_outliers(Value_mean)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
totAmpFlat <- totAmpFlat[!totAmpFlat$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
totAmpFlatMeans %>%
  group_by(Condition,Series) %>%
  shapiro_test(Value_mean)

# Inspect QQ plots
ggqqplot(totAmpFlatMeans, "Value_mean", ggtheme = theme_bw()) +
  facet_grid(Series ~ Condition, labeller = "label_both")

# Remove unused columns
totAmpFlatMeans[,ncol(totAmpFlatMeans)] <- NULL

resultsAov <- anova_test(data = totAmpFlatMeans, dv = Value_mean, wid = Subject, within = c(Series, Condition))
get_anova_table(resultsAov)

##### attAmpFlat #####

# Setup data frame
attAmpFlat <- read.csv("attAmpFlat.csv")
attAmpFlat <- attAmpFlat[-c(1:283),] %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(attAmpFlat, 3)

# Aggregate by mean condition per subject and series
attAmpFlatMeans <- as_tibble(attAmpFlat %>%
                          group_by(Subject,Series,Condition) %>%
                          summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Get summary stats
attAmpFlatMeans %>%
  group_by(Condition, Series) %>%
  get_summary_stats(Value_mean, type = "common")

# Create box plot
bxp <- ggboxplot(attAmpFlatMeans, x = "Series", y = "Value_mean", color = "Condition", palette = "jco")
bxp

# Find extreme outliers
outliers <- attAmpFlatMeans %>%
  group_by(Condition, Series) %>%
  identify_outliers(Value_mean)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
attAmpFlat <- attAmpFlat[!attAmpFlat$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
attAmpFlatMeans %>%
  group_by(Condition,Series) %>%
  shapiro_test(Value_mean)

# Inspect QQ plots
ggqqplot(attAmpFlatMeans, "Value_mean", ggtheme = theme_bw()) +
  facet_grid(Series ~ Condition, labeller = "label_both")

# Remove unused columns
attAmpFlatMeans[,ncol(attAmpFlatMeans)] <- NULL

resultsAov <- anova_test(data = attAmpFlatMeans, dv = Value_mean, wid = Subject, within = c(Series, Condition))
get_anova_table(resultsAov)

##### decAmpFlat #####

# Setup data frame
decAmpFlat <- read.csv("decAmpFlat.csv")
decAmpFlat <- decAmpFlat[-c(1:283),] %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(decAmpFlat, 3)

# Aggregate by mean condition per subject and series
decAmpFlatMeans <- as_tibble(decAmpFlat %>%
                          group_by(Subject,Series,Condition) %>%
                          summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Get summary stats
decAmpFlatMeans %>%
  group_by(Condition, Series) %>%
  get_summary_stats(Value_mean, type = "common")

# Create box plot
bxp <- ggboxplot(decAmpFlatMeans, x = "Series", y = "Value_mean", color = "Condition", palette = "jco")
bxp

# Find extreme outliers
outliers <- decAmpFlatMeans %>%
  group_by(Condition, Series) %>%
  identify_outliers(Value_mean)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
decAmpFlat <- decAmpFlat[!decAmpFlat$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
decAmpFlatMeans %>%
  group_by(Condition,Series) %>%
  shapiro_test(Value_mean)

# Inspect QQ plots
ggqqplot(decAmpFlatMeans, "Value_mean", ggtheme = theme_bw()) +
  facet_grid(Series ~ Condition, labeller = "label_both")

# Remove unused columns
decAmpFlatMeans[,ncol(decAmpFlatMeans)] <- NULL

resultsAov <- anova_test(data = decAmpFlatMeans, dv = Value_mean, wid = Subject, within = c(Series, Condition))
get_anova_table(resultsAov)

##### transAmpFlat #####

# Setup data frame
transAmpFlat <- read.csv("transAmpFlat.csv")
transAmpFlat <- transAmpFlat[-c(1:283),] %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(transAmpFlat, 3)

# Aggregate by mean condition per subject and series
transAmpFlatMeans <- as_tibble(transAmpFlat %>%
                            group_by(Subject,Series,Condition) %>%
                            summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Get summary stats
transAmpFlatMeans %>%
  group_by(Condition, Series) %>%
  get_summary_stats(Value_mean, type = "common")

# Create box plot
bxp <- ggboxplot(transAmpFlatMeans, x = "Series", y = "Value_mean", color = "Condition", palette = "jco")
bxp

# Find extreme outliers
outliers <- transAmpFlatMeans %>%
  group_by(Condition, Series) %>%
  identify_outliers(Value_mean)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
transAmpFlat <- transAmpFlat[!transAmpFlat$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
transAmpFlatMeans %>%
  group_by(Condition,Series) %>%
  shapiro_test(Value_mean)

# Inspect QQ plots
ggqqplot(transAmpFlatMeans, "Value_mean", ggtheme = theme_bw()) +
  facet_grid(Series ~ Condition, labeller = "label_both")

# Remove unused columns
transAmpFlatMeans[,ncol(transAmpFlatMeans)] <- NULL

resultsAov <- anova_test(data = transAmpFlatMeans, dv = Value_mean, wid = Subject, within = c(Series, Condition))
get_anova_table(resultsAov)

##### totSpecFlat #####

# Setup data frame
totSpecFlat <- read.csv("totSpecFlat.csv")
totSpecFlat <- totSpecFlat[-c(1:283),] %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(totSpecFlat, 3)

# Aggregate by mean condition per subject and series
totSpecFlatMeans <- as_tibble(totSpecFlat %>%
                          group_by(Subject,Series,Condition) %>%
                          summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Get summary stats
totSpecFlatMeans %>%
  group_by(Condition, Series) %>%
  get_summary_stats(Value_mean, type = "common")

# Create box plot
bxp <- ggboxplot(totSpecFlatMeans, x = "Series", y = "Value_mean", color = "Condition", palette = "jco")
bxp

# Find extreme outliers
outliers <- totSpecFlatMeans %>%
  group_by(Condition, Series) %>%
  identify_outliers(Value_mean)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
totSpecFlat <- totSpecFlat[!totSpecFlat$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
totSpecFlatMeans %>%
  group_by(Condition,Series) %>%
  shapiro_test(Value_mean)

# Inspect QQ plots
ggqqplot(totSpecFlatMeans, "Value_mean", ggtheme = theme_bw()) +
  facet_grid(Series ~ Condition, labeller = "label_both")

# Remove unused columns
totSpecFlatMeans[,ncol(totSpecFlatMeans)] <- NULL

resultsAov <- anova_test(data = totSpecFlatMeans, dv = Value_mean, wid = Subject, within = c(Series, Condition))
get_anova_table(resultsAov)

##### attSpecFlat #####

# Setup data frame
attSpecFlat <- read.csv("attSpecFlat.csv")
attSpecFlat <- attSpecFlat[-c(1:283),] %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(attSpecFlat, 3)

# Aggregate by mean condition per subject and series
attSpecFlatMeans <- as_tibble(attSpecFlat %>%
                          group_by(Subject,Series,Condition) %>%
                          summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Get summary stats
attSpecFlatMeans %>%
  group_by(Condition, Series) %>%
  get_summary_stats(Value_mean, type = "common")

# Create box plot
bxp <- ggboxplot(attSpecFlatMeans, x = "Series", y = "Value_mean", color = "Condition", palette = "jco")
bxp

# Find extreme outliers
outliers <- attSpecFlatMeans %>%
  group_by(Condition, Series) %>%
  identify_outliers(Value_mean)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
attSpecFlat <- attSpecFlat[!attSpecFlat$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
attSpecFlatMeans %>%
  group_by(Condition,Series) %>%
  shapiro_test(Value_mean)

# Inspect QQ plots
ggqqplot(attSpecFlatMeans, "Value_mean", ggtheme = theme_bw()) +
  facet_grid(Series ~ Condition, labeller = "label_both")

# Remove unused columns
attSpecFlatMeans[,ncol(attSpecFlatMeans)] <- NULL

resultsAov <- anova_test(data = attSpecFlatMeans, dv = Value_mean, wid = Subject, within = c(Series, Condition))
get_anova_table(resultsAov)

##### decSpecFlat #####

# Setup data frame
decSpecFlat <- read.csv("decSpecFlat.csv")
decSpecFlat <- decSpecFlat[-c(1:283),] %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(decSpecFlat, 3)

# Aggregate by mean condition per subject and series
decSpecFlatMeans <- as_tibble(decSpecFlat %>%
                          group_by(Subject,Series,Condition) %>%
                          summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Get summary stats
decSpecFlatMeans %>%
  group_by(Condition, Series) %>%
  get_summary_stats(Value_mean, type = "common")

# Create box plot
bxp <- ggboxplot(decSpecFlatMeans, x = "Series", y = "Value_mean", color = "Condition", palette = "jco")
bxp

# Find extreme outliers
outliers <- decSpecFlatMeans %>%
  group_by(Condition, Series) %>%
  identify_outliers(Value_mean)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
decSpecFlat <- decSpecFlat[!decSpecFlat$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
decSpecFlatMeans %>%
  group_by(Condition,Series) %>%
  shapiro_test(Value_mean)

# Inspect QQ plots
ggqqplot(decSpecFlatMeans, "Value_mean", ggtheme = theme_bw()) +
  facet_grid(Series ~ Condition, labeller = "label_both")

# Remove unused columns
decSpecFlatMeans[,ncol(decSpecFlatMeans)] <- NULL

resultsAov <- anova_test(data = decSpecFlatMeans, dv = Value_mean, wid = Subject, within = c(Series, Condition))
get_anova_table(resultsAov)

##### transSpecFlat #####

# Setup data frame
transSpecFlat <- read.csv("transSpecFlat.csv")
transSpecFlat <- transSpecFlat[-c(1:283),] %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(transSpecFlat, 3)

# Aggregate by mean condition per subject and series
transSpecFlatMeans <- as_tibble(transSpecFlat %>%
                            group_by(Subject,Series,Condition) %>%
                            summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Get summary stats
transSpecFlatMeans %>%
  group_by(Condition, Series) %>%
  get_summary_stats(Value_mean, type = "common")

# Create box plot
bxp <- ggboxplot(transSpecFlatMeans, x = "Series", y = "Value_mean", color = "Condition", palette = "jco")
bxp

# Find extreme outliers
outliers <- transSpecFlatMeans %>%
  group_by(Condition, Series) %>%
  identify_outliers(Value_mean)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
transSpecFlat <- transSpecFlat[!transSpecFlat$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
transSpecFlatMeans %>%
  group_by(Condition,Series) %>%
  shapiro_test(Value_mean)

# Inspect QQ plots
ggqqplot(transSpecFlatMeans, "Value_mean", ggtheme = theme_bw()) +
  facet_grid(Series ~ Condition, labeller = "label_both")

# Remove unused columns
transSpecFlatMeans[,ncol(transSpecFlatMeans)] <- NULL

resultsAov <- anova_test(data = transSpecFlatMeans, dv = Value_mean, wid = Subject, within = c(Series, Condition))
get_anova_table(resultsAov)

##### crest #####

# Setup data frame
crest <- read.csv("crest.csv")
crest <- crest[-c(1:283),] %>%
  convert_as_factor(Subject, Arm, Series, Condition)
head(crest, 3)

# Aggregate by mean condition per subject and series
crestMeans <- as_tibble(crest %>%
                                  group_by(Subject,Series,Condition) %>%
                                  summarise_at(.vars = names(.)[6:7], .funs = c(mean="mean")))

# Get summary stats
crestMeans %>%
  group_by(Condition, Series) %>%
  get_summary_stats(Value_mean, type = "common")

# Create box plot
bxp <- ggboxplot(crestMeans, x = "Series", y = "Value_mean", color = "Condition", palette = "jco")
bxp

# Find extreme outliers
outliers <- crestMeans %>%
  group_by(Condition, Series) %>%
  identify_outliers(Value_mean)
sprintf("Found %d extreme outliers", length(outliers$Stroke[outliers$is.extreme == TRUE]))

# Remove extreme outliers
crest <- crest[!crest$Stroke %in% c(outliers$Stroke[outliers$is.extreme == TRUE]),]

# Check normality assumption
# Shapiro-Wilk test (won't work with this sample size)
crestMeans %>%
  group_by(Condition,Series) %>%
  shapiro_test(Value_mean)

# Inspect QQ plots
ggqqplot(crestMeans, "Value_mean", ggtheme = theme_bw()) +
  facet_grid(Series ~ Condition, labeller = "label_both")

# Remove unused columns
crestMeans[,ncol(crestMeans)] <- NULL

resultsAov <- anova_test(data = crestMeans, dv = Value_mean, wid = Subject, within = c(Series, Condition))
get_anova_table(resultsAov)
