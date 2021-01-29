library(brms)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(kfigr)
library(knitr)
library(patchwork)

opts_chunk$set(comment = NULL,
               fig.width = 12,
               fig.height = 12,
               echo = TRUE)
theme_set(theme_light(base_size = 18,
                      base_line_size = 18/11,
                      base_rect_size = 18/11))
knit_hooks$set(anchor = kfigr::hook_anchor)
opts_knit$set(kfigr.link = TRUE,
              kfigr.prefix = TRUE)
knitr::opts_chunk$set(fig.width = 3.5,
                      fig.height = 3.5)
options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE)

setwd("C:/Coding/ASIAMS")

####### Pick window for early decay descriptors calculation ########

## Variable window: [maxPeak:TC] ##
read.csv("features.csv") %>%
  mutate(patient = factor(if_else(grepl("^P",
                                        subj),
                                  1L,
                                  0L)),
         series = factor(series),
         subj = factor(subj),
         arm = factor(arm),
         cond = factor(cond),
         no = factor(no)) ->
  drum_beats

## Fixed window: [maxPeak:maxPeak+50ms] ##
read.csv("features_fixed_earlyDecay.csv") %>%
  mutate(filename = NULL,
         patient = factor(if_else(grepl("^P",
                                        subj),
                                  1L,
                                  0L)),
         series = factor(series),
         subj = factor(subj),
         arm = factor(arm),
         cond = factor(cond),
         no = factor(no)) ->
  drum_beats

####### Spectral centroid ########

# Plot of early decay SC grouped by condition and subject
ggplot(drum_beats, aes(transSC,
                                         color = cond,
                                         group = series)) +
  geom_density(alpha = 0.1) +
  scale_color_colorblind() +
  scale_y_continuous(limits = c(0.0, 0.11)) +
  facet_wrap(~ subj, nrow = 2)

# Plot of early decay crest factor grouped by condition and subject
ggplot(drum_beats, aes(transCrest,
                       color = cond,
                       group = series)) +
  geom_density(alpha = 0.1) +
  scale_color_colorblind() +
  scale_y_continuous(limits = c(0.0, 15.0)) +
  facet_wrap(~ subj, nrow = 2)

# 1) Looking for strokes with low early decay sC for normal condition
#    and high early decay SC for controlled condition
# 2) Looking for strokes with low early decay crest factor for normal condition
#    and high early decay crest factor for controlled condition

# P3, Normal strokes
early_dec_p3_n <- subset(drum_beats, subj =="P3" & cond == "N", select = c(filename,transSC,transCrest))
# Sort by spectral centroid value
early_dec_p3_n_sorted <- early_dec_p3_n[order(early_dec_p3_n$transSC), ]
# Sort by crest factor value
# early_dec_p3_n_files_sorted <- early_dec_p3_n[order(early_dec_p3_n$transCrest)]
# Write to csv
write.table(early_dec_p3_n_sorted, "earlyDec_P3_N.csv", row.names=FALSE, sep=",")

# P3, Controlled strokes
early_dec_p3_c <- subset(drum_beats, subj =="P3" & cond == "C", select = c(filename,transSC,transCrest))
# Sort by spectral centroid value
early_dec_p3_c_sorted <- early_dec_p3_c[order(early_dec_p3_c$transSC), ]
# Sort by crest factor value
# early_dec_p3_c_files_sorted <- early_dec_p3_c[order(early_dec_p3_c$transCrest)]
# Write to csv
write.table(early_dec_p3_c_sorted, "earlyDec_P3_C.csv", row.names=FALSE, sep=",")

# S2, Normal strokes
early_dec_s2_n <- subset(drum_beats, subj =="S2" & cond == "N", select = c(filename,transSC,transCrest))
# Sort by spectral centroid value
early_dec_s2_n_sorted <- early_dec_s2_n[order(early_dec_s2_n$transSC), ]
# Sort by crest factor value
# early_dec_s2_n_files_sorted <- early_dec_s2_n[order(early_dec_s2_n$transCrest)]
# Write to csv
write.table(early_dec_s2_n_sorted, "earlyDec_S2_N.csv", row.names=FALSE, sep=",")

# P3, Controlled strokes
early_dec_s2_c <- subset(drum_beats, subj =="S2" & cond == "C", select = c(filename,transSC,transCrest))
# Sort by spectral centroid value
early_dec_s2_c_sorted <- early_dec_s2_c[order(early_dec_s2_c$transSC), ]
# Sort by crest factor value
# early_dec_s2_c_files_sorted <- early_dec_s2_c$filename[order(early_dec_s2_c$transCrest)]
# Write to csv
write.table(early_dec_s2_c_sorted, "earlyDec_S2_C.csv", row.names=FALSE, sep=",")

