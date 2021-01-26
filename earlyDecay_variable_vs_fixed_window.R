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

read.csv("features.csv") %>%
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
  drum_beats_earlyDec_varwin

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
  drum_beats_earlyDec_fixedwin

# Plot of early decay SC grouped by condition and subject, variable window (maxPeak:TC)
ggplot(drum_beats_earlyDec_varwin, aes(transSC,
                                         color = cond,
                                         group = series)) +
  geom_density(alpha = 0.1) +
  scale_color_colorblind() +
  scale_y_continuous(limits = c(0.0, 0.11)) +
  facet_wrap(~ subj, nrow = 2) +
  ggtitle("variable window (maxPeak:TC)")

# Plot of early decay SC grouped by condition and subject, fixed window (maxPeak:maxPeak+50ms)
ggplot(drum_beats_earlyDec_fixedwin, aes(transSC,
                       color = cond,
                       group = series)) +
  geom_density(alpha = 0.1) +
  scale_color_colorblind() +
  scale_y_continuous(limits = c(0.0, 0.11)) +
  facet_wrap(~ subj, nrow = 2) +
  ggtitle("fixed window (maxPeak:maxPeak+50ms)")

# Looking for strokes with low early decay sC for normal condition and high early decay SC for controlled condition
sort(drum_beats_earlyDec_fixedwin$transSC[drum_beats_earlyDec_fixedwin$subj=="P3"
                                          & drum_beats_earlyDec_fixedwin$cond=="N"],
     decreasing=TRUE)
sort(drum_beats_earlyDec_varwin$transSC[drum_beats_earlyDec_fixedwin$subj=="P3"
                                          & drum_beats_earlyDec_fixedwin$cond=="N"],
     decreasing=TRUE)
sort(drum_beats_earlyDec_fixedwin$transSC[drum_beats_earlyDec_fixedwin$subj=="P3"
                                          & drum_beats_earlyDec_fixedwin$cond=="C"],
     decreasing=TRUE)
sort(drum_beats_earlyDec_varwin$transSC[drum_beats_earlyDec_fixedwin$subj=="P3"
                                        & drum_beats_earlyDec_fixedwin$cond=="C"],
     decreasing=TRUE)
sort(drum_beats_earlyDec_fixedwin$transSC[drum_beats_earlyDec_fixedwin$subj=="S2"
                                          & drum_beats_earlyDec_fixedwin$cond=="N"],
     decreasing=TRUE)
sort(drum_beats_earlyDec_varwin$transSC[drum_beats_earlyDec_fixedwin$subj=="S2"
                                        & drum_beats_earlyDec_fixedwin$cond=="N"],
     decreasing=TRUE)
sort(drum_beats_earlyDec_fixedwin$transSC[drum_beats_earlyDec_fixedwin$subj=="S2"
                                          & drum_beats_earlyDec_fixedwin$cond=="C"],
     decreasing=TRUE)
sort(drum_beats_earlyDec_varwin$transSC[drum_beats_earlyDec_fixedwin$subj=="S2"
                                        & drum_beats_earlyDec_fixedwin$cond=="C"],
     decreasing=TRUE)

# Plot of early decay crest factor grouped by condition and subject, variable window (maxPeak:TC)
ggplot(drum_beats_earlyDec_varwin, aes(transCrest,
                                       color = cond,
                                       group = series)) +
  geom_density(alpha = 0.1) +
  scale_color_colorblind() +
  scale_y_continuous(limits = c(0.0, 15.0)) +
  facet_wrap(~ subj, nrow = 2) +
  ggtitle("variable window (maxPeak:TC)")

# Plot of early decay crest factor grouped by condition and subject, fixed window (maxPeak:maxPeak+50ms)
ggplot(drum_beats_earlyDec_fixedwin, aes(transCrest,
                                         color = cond,
                                         group = series)) +
  geom_density(alpha = 0.1) +
  scale_color_colorblind() +
  scale_y_continuous(limits = c(0.0, 15.0)) +
  facet_wrap(~ subj, nrow = 2) +
  ggtitle("fixed window (maxPeak:maxPeak+50ms)")

# Looking for strokes with low early decay crest factor
# for normal condition and high early decay crest factor for controlled condition
sort(drum_beats_earlyDec_fixedwin$transCrest[drum_beats_earlyDec_fixedwin$subj=="P3"
                                          & drum_beats_earlyDec_fixedwin$cond=="N"],
     decreasing=TRUE)
sort(drum_beats_earlyDec_varwin$transCrest[drum_beats_earlyDec_fixedwin$subj=="P3"
                                        & drum_beats_earlyDec_fixedwin$cond=="N"],
     decreasing=TRUE)
sort(drum_beats_earlyDec_fixedwin$transCrest[drum_beats_earlyDec_fixedwin$subj=="P3"
                                          & drum_beats_earlyDec_fixedwin$cond=="C"],
     decreasing=TRUE)
sort(drum_beats_earlyDec_varwin$transCrest[drum_beats_earlyDec_fixedwin$subj=="P3"
                                        & drum_beats_earlyDec_fixedwin$cond=="C"],
     decreasing=TRUE)
sort(drum_beats_earlyDec_fixedwin$transCrest[drum_beats_earlyDec_fixedwin$subj=="S2"
                                             & drum_beats_earlyDec_fixedwin$cond=="N"],
     decreasing=TRUE)
sort(drum_beats_earlyDec_varwin$transCrest[drum_beats_earlyDec_fixedwin$subj=="S2"
                                           & drum_beats_earlyDec_fixedwin$cond=="N"],
     decreasing=TRUE)
sort(drum_beats_earlyDec_fixedwin$transCrest[drum_beats_earlyDec_fixedwin$subj=="S2"
                                             & drum_beats_earlyDec_fixedwin$cond=="C"],
     decreasing=TRUE)
sort(drum_beats_earlyDec_varwin$transCrest[drum_beats_earlyDec_fixedwin$subj=="S2"
                                           & drum_beats_earlyDec_fixedwin$cond=="C"],
     decreasing=TRUE)

# Conclusions
# I don't see any particular benefit in moving to the fixed window method: it gets us less pointy distributions,
# but doesn't really increase the spread of the extreme values.
# Crest factor has a lot more overlap on S2, not sure if that will be an issue. I will try to prepare
# three lists of strokes:
# 1) Based on early decay SC alone
# 2) Based on early decay crest factor alone
# 3) Based on both early decay SC and crest factor
