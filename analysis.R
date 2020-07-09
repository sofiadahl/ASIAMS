library(tidyverse)
library(ggpubr)
library(rstatix)
library(dplyr)

setwd("C:/Users/franc/Aalborg Universitet/Sofia Dahl - ASIAMS-drumstrokes/singleStrokes")

##### Implementation #####

# Setup data frame
allFeatures <- read.csv("features.csv")

allFeatures <- allFeatures %>%
  convert_as_factor(subj, arm, series, cond)
head(allFeatures, 3)

##### Inspect QQ plots #####

# totDur: 3/32 non-normal series
# attDur: 4/32 non-normal series
# decDur: 4/32 non-normal series
# transDur: 4/32 non-normal series
# LAT: 3/32 non-normal series
# totSPL: 8/32 non-normal series
# attSPL: 11/32 non-normal series
# decSPL: 11/32 non-normal series
# transSPL: 9/32 non-normal series
# totSC: 6/32 non-normal series
# attSC: 4/32 non-normal series
# decSC: 6/32 non-normal series
# transSC: 5/32 non-normal series
# TC: 3/32 non-normal series
# totFlat: 3/32 non-normal series
# attFlat: 30/32 non-normal series
# decFlat: 4/32 non-normal series
# transFlat: 3/32 non-normal series
# totSpecFlat: 3/32 non-normal series
# attSpecFlat: 4/32 non-normal series
# decSpecFlat: 2/32 non-normal series
# transSpecFlat: 5/32 non-normal series
# crest: 3/32 non-normal series

ggqqplot(allFeatures, "totDur", facet.by = c("cond","series"))
ggqqplot(allFeatures, "attDur", facet.by = c("cond","series"))
ggqqplot(allFeatures, "decDur", facet.by = c("cond","series"))
ggqqplot(allFeatures, "transDur", facet.by = c("cond","series"))
ggqqplot(allFeatures, "LAT", facet.by = c("cond","series"))
ggqqplot(allFeatures, "totSPL", facet.by = c("cond","series"))
ggqqplot(allFeatures, "attSPL", facet.by = c("cond","series"))
ggqqplot(allFeatures, "decSPL", facet.by = c("cond","series"))
ggqqplot(allFeatures, "transSPL", facet.by = c("cond","series"))
ggqqplot(allFeatures, "totSC", facet.by = c("cond","series"))
ggqqplot(allFeatures, "attSC", facet.by = c("cond","series"))
ggqqplot(allFeatures, "decSC", facet.by = c("cond","series"))
ggqqplot(allFeatures, "transSC", facet.by = c("cond","series"))
ggqqplot(allFeatures, "TC", facet.by = c("cond","series"))
ggqqplot(allFeatures, "totFlat", facet.by = c("cond","series"))
ggqqplot(allFeatures, "attFlat", facet.by = c("cond","series"))
ggqqplot(allFeatures, "decFlat", facet.by = c("cond","series"))
ggqqplot(allFeatures, "transFlat", facet.by = c("cond","series"))
ggqqplot(allFeatures, "totSpecFlat", facet.by = c("cond","series"))
ggqqplot(allFeatures, "attSpecFlat", facet.by = c("cond","series"))
ggqqplot(allFeatures, "decSpecFlat", facet.by = c("cond","series"))
ggqqplot(allFeatures, "transSpecFlat", facet.by = c("cond","series"))
ggqqplot(allFeatures, "crest", facet.by = c("cond","series"))

##### Remove outliers #####

# totDur: 6
# attDur: 1
# decDur: 6
# transDur: 2
# LAT: 3
# totSPL: 0
# attSPL: 0
# decSPL: 0
# transSPL: 3
# totSC: 0
# attSC: 6
# decSC: 0
# transSC: 0
# TC: 8
# totFlat: 11
# attFlat: 24
# decFlat: 4
# transFlat: 0
# totSpecFlat: 4
# attSpecFlat: 8
# decSpecFlat: 4
# transSpecFlat: 0
# crest: 0

leftSide <- c(1:6)

# totDur
# Find extreme outliers
outliers <- allFeatures %>%
  group_by(cond, subj) %>%
  identify_outliers(totDur)
sprintf("Found %d extreme outliers", nrow(outliers[outliers$is.extreme == TRUE,]))

# Remove extreme outliers
totDur <- allFeatures[!allFeatures$filename %in% outliers$filename[outliers$is.extreme == TRUE],c(leftSide, c(7))]

# attDur
# Find extreme outliers
outliers <- allFeatures %>%
  group_by(cond, subj) %>%
  identify_outliers(attDur)
sprintf("Found %d extreme outliers", nrow(outliers[outliers$is.extreme == TRUE,]))

# Remove extreme outliers
attDur <- allFeatures[!allFeatures$filename %in% outliers$filename[outliers$is.extreme == TRUE],c(leftSide, c(8))]

# decDur
# Find extreme outliers
outliers <- allFeatures %>%
  group_by(cond, subj) %>%
  identify_outliers(decDur)
sprintf("Found %d extreme outliers", nrow(outliers[outliers$is.extreme == TRUE,]))

# Remove extreme outliers
decDur <- allFeatures[!allFeatures$filename %in% outliers$filename[outliers$is.extreme == TRUE],c(leftSide, c(9))]

# transDur
# Find extreme outliers
outliers <- allFeatures %>%
  group_by(cond, subj) %>%
  identify_outliers(transDur)
sprintf("Found %d extreme outliers", nrow(outliers[outliers$is.extreme == TRUE,]))

# Remove extreme outliers
transDur <- allFeatures[!allFeatures$filename %in% outliers$filename[outliers$is.extreme == TRUE],c(leftSide, c(10))]

# LAT
# Find extreme outliers
outliers <- allFeatures %>%
  group_by(cond, subj) %>%
  identify_outliers(LAT)
sprintf("Found %d extreme outliers", nrow(outliers[outliers$is.extreme == TRUE,]))

# Remove extreme outliers
LAT <- allFeatures[!allFeatures$filename %in% outliers$filename[outliers$is.extreme == TRUE],c(leftSide, c(11))]

# totSPL
# Find extreme outliers
outliers <- allFeatures %>%
  group_by(cond, subj) %>%
  identify_outliers(totSPL)
sprintf("Found %d extreme outliers", nrow(outliers[outliers$is.extreme == TRUE,]))

# Remove extreme outliers
totSPL <- allFeatures[!allFeatures$filename %in% outliers$filename[outliers$is.extreme == TRUE],c(leftSide, c(12))]

# attSPL
# Find extreme outliers
outliers <- allFeatures %>%
  group_by(cond, subj) %>%
  identify_outliers(attSPL)
sprintf("Found %d extreme outliers", nrow(outliers[outliers$is.extreme == TRUE,]))

# Remove extreme outliers
attSPL <- allFeatures[!allFeatures$filename %in% outliers$filename[outliers$is.extreme == TRUE],c(leftSide, c(13))]

# decSPL
# Find extreme outliers
outliers <- allFeatures %>%
  group_by(cond, subj) %>%
  identify_outliers(decSPL)
sprintf("Found %d extreme outliers", nrow(outliers[outliers$is.extreme == TRUE,]))

# Remove extreme outliers
decSPL <- allFeatures[!allFeatures$filename %in% outliers$filename[outliers$is.extreme == TRUE],c(leftSide, c(14))]

# transSPL
# Find extreme outliers
outliers <- allFeatures %>%
  group_by(cond, subj) %>%
  identify_outliers(transSPL)
sprintf("Found %d extreme outliers", nrow(outliers[outliers$is.extreme == TRUE,]))

# Remove extreme outliers
transSPL <- allFeatures[!allFeatures$filename %in% outliers$filename[outliers$is.extreme == TRUE],c(leftSide, c(15))]

# totSC
# Find extreme outliers
outliers <- allFeatures %>%
  group_by(cond, subj) %>%
  identify_outliers(totSC)
sprintf("Found %d extreme outliers", nrow(outliers[outliers$is.extreme == TRUE,]))

# Remove extreme outliers
totSC <- allFeatures[!allFeatures$filename %in% outliers$filename[outliers$is.extreme == TRUE],c(leftSide, c(16))]

# attSC
# Find extreme outliers
outliers <- allFeatures %>%
  group_by(cond, subj) %>%
  identify_outliers(attSC)
sprintf("Found %d extreme outliers", nrow(outliers[outliers$is.extreme == TRUE,]))

# Remove extreme outliers
attSC <- allFeatures[!allFeatures$filename %in% outliers$filename[outliers$is.extreme == TRUE],c(leftSide, c(17))]

# decSC
# Find extreme outliers
outliers <- allFeatures %>%
  group_by(cond, subj) %>%
  identify_outliers(decSC)
sprintf("Found %d extreme outliers", nrow(outliers[outliers$is.extreme == TRUE,]))

# Remove extreme outliers
decSC <- allFeatures[!allFeatures$filename %in% outliers$filename[outliers$is.extreme == TRUE],c(leftSide, c(18))]

# transSC
# Find extreme outliers
outliers <- allFeatures %>%
  group_by(cond, subj) %>%
  identify_outliers(transSC)
sprintf("Found %d extreme outliers", nrow(outliers[outliers$is.extreme == TRUE,]))

# Remove extreme outliers
transSC <- allFeatures[!allFeatures$filename %in% outliers$filename[outliers$is.extreme == TRUE],c(leftSide, c(19))]

# TC
# Find extreme outliers
outliers <- allFeatures %>%
  group_by(cond, subj) %>%
  identify_outliers(TC)
sprintf("Found %d extreme outliers", nrow(outliers[outliers$is.extreme == TRUE,]))

# Remove extreme outliers
TC <- allFeatures[!allFeatures$filename %in% outliers$filename[outliers$is.extreme == TRUE],c(leftSide, c(20))]

# totFlat
# Find extreme outliers
outliers <- allFeatures %>%
  group_by(cond, subj) %>%
  identify_outliers(totFlat)
sprintf("Found %d extreme outliers", nrow(outliers[outliers$is.extreme == TRUE,]))

# Remove extreme outliers
totFlat <- allFeatures[!allFeatures$filename %in% outliers$filename[outliers$is.extreme == TRUE],c(leftSide, c(21))]

# attFlat
# Find extreme outliers
outliers <- allFeatures %>%
  group_by(cond, subj) %>%
  identify_outliers(attFlat)
sprintf("Found %d extreme outliers", nrow(outliers[outliers$is.extreme == TRUE,]))

# Remove extreme outliers
attFlat <- allFeatures[!allFeatures$filename %in% outliers$filename[outliers$is.extreme == TRUE],c(leftSide, c(22))]

# decFlat
# Find extreme outliers
outliers <- allFeatures %>%
  group_by(cond, subj) %>%
  identify_outliers(decFlat)
sprintf("Found %d extreme outliers", nrow(outliers[outliers$is.extreme == TRUE,]))

# Remove extreme outliers
decFlat <- allFeatures[!allFeatures$filename %in% outliers$filename[outliers$is.extreme == TRUE],c(leftSide, c(23))]

# transFlat
# Find extreme outliers
outliers <- allFeatures %>%
  group_by(cond, subj) %>%
  identify_outliers(transFlat)
sprintf("Found %d extreme outliers", nrow(outliers[outliers$is.extreme == TRUE,]))

# Remove extreme outliers
transFlat <- allFeatures[!allFeatures$filename %in% outliers$filename[outliers$is.extreme == TRUE],c(leftSide, c(24))]

# totSpecFlat
# Find extreme outliers
outliers <- allFeatures %>%
  group_by(cond, subj) %>%
  identify_outliers(totSpecFlat)
sprintf("Found %d extreme outliers", nrow(outliers[outliers$is.extreme == TRUE,]))

# Remove extreme outliers
totSpecFlat <- allFeatures[!allFeatures$filename %in% outliers$filename[outliers$is.extreme == TRUE],c(leftSide, c(25))]

# attSpecFlat
# Find extreme outliers
outliers <- allFeatures %>%
  group_by(cond, subj) %>%
  identify_outliers(attSpecFlat)
sprintf("Found %d extreme outliers", nrow(outliers[outliers$is.extreme == TRUE,]))

# Remove extreme outliers
attSpecFlat <- allFeatures[!allFeatures$filename %in% outliers$filename[outliers$is.extreme == TRUE],c(leftSide, c(26))]

# decSpecFlat
# Find extreme outliers
outliers <- allFeatures %>%
  group_by(cond, subj) %>%
  identify_outliers(decSpecFlat)
sprintf("Found %d extreme outliers", nrow(outliers[outliers$is.extreme == TRUE,]))

# Remove extreme outliers
decSpecFlat <- allFeatures[!allFeatures$filename %in% outliers$filename[outliers$is.extreme == TRUE],c(leftSide, c(27))]

# transSpecFlat
# Find extreme outliers
outliers <- allFeatures %>%
  group_by(cond, subj) %>%
  identify_outliers(transSpecFlat)
sprintf("Found %d extreme outliers", nrow(outliers[outliers$is.extreme == TRUE,]))

# Remove extreme outliers
transSpecFlat <- allFeatures[!allFeatures$filename %in% outliers$filename[outliers$is.extreme == TRUE],c(leftSide, c(28))]

# crest
# Find extreme outliers
outliers <- allFeatures %>%
  group_by(cond, subj) %>%
  identify_outliers(crest)
sprintf("Found %d extreme outliers", nrow(outliers[outliers$is.extreme == TRUE,]))

# Remove extreme outliers
crest <- allFeatures[!allFeatures$filename %in% outliers$filename[outliers$is.extreme == TRUE],c(leftSide, c(29))]

##### Correlation analysis ####

# transSPL vs transSC
ggscatter(allFeatures[allFeatures$subj=='P1',], x = "transSPL", y = "transSC", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Transient SPL [dB SPL]", ylab = "Transient spectral centroid [Hz]")

# transSPL vs transFlat
ggscatter(allFeatures[allFeatures$subj=='P1',], x = "transSPL", y = "transFlat", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Transient SPL [dB SPL]", ylab = "Transient envelope flatness [-]")

# transSPL vs transSpecFlat
ggscatter(allFeatures[allFeatures$subj=='P1',], x = "transSPL", y = "transSpecFlat", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Transient SPL [dB SPL]", ylab = "Transient spectral flatness [-]")

# transSC vs transSpecFlat
ggscatter(allFeatures[allFeatures$subj=='P1',], x = "transSC", y = "transSpecFlat", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Transient spectral centroid [Hz]", ylab = "Transient spectral flatness [-]")

##### Two-way ANOVA (subject x (condition, arm)) #####

# Get summary stats
allFeatures %>%
  group_by(cond, subj) %>%
  get_summary_stats(totDur, type = "common")

# attSC

# Create box plot
bxp <- ggboxplot(attSC, x = "subj", y = "attSC", color = "cond", palette = "jco")
bxp

# Aggregate by mean condition per subject
attSCMeans <- as_tibble(attSC %>%
                            group_by(subj,cond,arm) %>%
                            summarise_at(.vars = names(.)[7], .funs = c(mean="mean")))

resultsAov <- anova_test(data = attSCMeans, dv = mean, wid = subj, within = c(cond, arm))
get_anova_table(resultsAov)

# transSC

# Create box plot
bxp <- ggboxplot(transSC, x = "subj", y = "transSC", color = "cond", palette = "jco")
bxp

# Aggregate by mean condition per subject
transSCMeans <- as_tibble(transSC %>%
                           group_by(subj,cond,arm) %>%
                           summarise_at(.vars = names(.)[7], .funs = c(mean="mean")))

resultsAov <- anova_test(data = transSCMeans, dv = mean, wid = subj, within = c(cond, arm))
get_anova_table(resultsAov)

# transFlat

# Create box plot
bxp <- ggboxplot(transFlat, x = "subj", y = "transFlat", color = "cond", palette = "jco")
bxp

# Aggregate by mean condition per subject
transFlatMeans <- as_tibble(transFlat %>%
                            group_by(subj,cond,arm) %>%
                            summarise_at(.vars = names(.)[7], .funs = c(mean="mean")))

resultsAov <- anova_test(data = transFlatMeans, dv = mean, wid = subj, within = c(cond, arm))
get_anova_table(resultsAov)

# transSpecFlat

# Create box plot
bxp <- ggboxplot(transSpecFlat, x = "subj", y = "transSpecFlat", color = "cond", palette = "jco")
bxp

# Aggregate by mean condition per subject
transSpecFlatMeans <- as_tibble(transSpecFlat %>%
                              group_by(subj,cond,arm) %>%
                              summarise_at(.vars = names(.)[7], .funs = c(mean="mean")))

resultsAov <- anova_test(data = transSpecFlatMeans, dv = mean, wid = subj, within = c(cond, arm))
get_anova_table(resultsAov)

# transSPL

# Create box plot
bxp <- ggboxplot(transSPL, x = "subj", y = "transSPL", color = "cond", palette = "jco")
bxp

# Aggregate by mean condition per subject
transSPLMeans <- as_tibble(transSPL %>%
                                  group_by(subj,cond,arm) %>%
                                  summarise_at(.vars = names(.)[7], .funs = c(mean="mean")))

resultsAov <- anova_test(data = transSPLMeans, dv = mean, wid = subj, within = c(cond, arm))
get_anova_table(resultsAov)
