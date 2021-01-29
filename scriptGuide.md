# Script guide

## storeCalibrationData.m

Takes calibration files (annotated with measured SPL) and calculates mic+preamp sensitivity. Note that the calibration file for subject P2 is missing. Sensitivities, measured SPLs and pRef are stored into a mat file.

## featExtractSingle.m

Requires **MIRtoolbox1.7.2** and the prerelease version of *mironsets.m* (available in the OneDrive folder under 'MIRToolbox addons').

Extracts features for a selected stroke (picked from the dir structure by setting the variable *selectedStroke*). The subdirectory *'singleStrokes'* is cleaned from spurious strokes (rim shots, double strokes, clicks and noticeable background noise): the missing files can be found in the *'P-data'* and *'S-data'* folders (useful for finding out which/how many strokes were removed for documentation purposes).

Finally, the script plots the waveform, amplitude envelope, onset, offset, max peak, temporal centroid for the selected stroke. This is useful for visual inspection and could easily be extended to make a clarifying figure regarding our feature extraction for the final paper.

Here's the extracted features:

- Duration (attack/transient/decay/total)
- Log attack time (LAT)
- SPL (attack/transient/decay/total)
- Temporal centroid
- Spectral centroid (attack/transient/decay/total)
- Temporal flatness (attack/transient/decay/total)
- Spectral flatness (attack/transient/decay/total)
- Crest factor (attack/transient/decay/total)

Attack phase = max peak time - onset time
Decay phase = offset time - max peak time
Transient phase = temporal centroid - max peak time
Total phase = offset time - onset time

By offset time we mean the end of the decay phase as estimated by the peak-picking algorithm in MIRToolbox (i.e. directly on the waveform).

Flatness is the ratio between the geometric and the aritmetic mean, so it indicates spiky vs flat distribution. Spectral flatness is well known; temporal flatness is obtained by passing the amplitude envelope as input.

Crest factor is the ratio between peak amplitude and rms amplitude.

Most of the procedures (windowing, etc.) are commented in the script, let me know if you need more details or have comments.

# featExtractAll.m

Same procedure as *featExtractSingle.m*, but done for all strokes. The extracted features are saved into *data.mat* as a struct array. Note that *data.mat* is larger than the allowed file size for pushing to Github, so you will need to run the script or get it from OneDrive if needed.

# exportCsv.m

Exports *data.mat* to a R-friendly table, saved into *features.csv*.

# batchSplit.md

Contains one-liners for splitting the files into single strokes (requires **sox**).

# analysis.R

Contains useful code blocks for statistical analysis: import, inspection of QQ plots, removal of extreme outliers, correlation analysis (to be continued), two-way repeated measures ANOVA (obsolete).

# analysisOLD.R, analysisOneWay.R, analysisTwoWay.R

Various obsolete R code chunks, including a linear mixed model which I never got to work.

# dahl_drumming_models.*

RStudio project files. The most interesting file is probably dahl_drumming_models.html, as it contains everything you want without the need to 'run' any files in R. Once you opened the html file with your browser you can click on the [Show Code] buttons to, well, show the R code. The dahl_drumming_models.Rmd file is the R markdown file from which the html is produced.

As of 2020-07-14 (version vive-la-france!) it only contains some exploratory graphs, some musings on response variable choice as well as on model choice. As of 2020-07-16, it also has a first model.

# listeningTest (folder)

The listening test runs remotely from [this server](https://drumstrokelisteningtest.aau.dk/test.html?url=tests/drumStrokes.xml).

At the moment, the test contains four pages: training (6 strokes from both P3 and S2, 3 per condition), training feedback (same 6 strokes), P3 (10 strokes x 2 repetitions, 5 per condition) and S2 (10 strokes x 2 repetitions, 5 per condition). The strokes on P3 and S2 are presented in random order.

The strokes are picked from the selected subjects, looking at the extreme values of the early decay spectral centroid. Check *select_strokes_listening_test.R* and the corresponding csv files for more details.

The server tracks a number of variables along with the actual response: total test time, time spent on each page, number of plays per stroke, master volume moved, etc.

The responses are saved as xml files and can be parsed using the included scripts (either locally or remote).

Status:

- Tested on Firefox/Win10.
- Feedback: check that the interface looks the same in Firefox and Chrome. Edge and Safari are also supported, but less recommended by the WAET authors. Let's check them and add a recommendation to the instructions if needed.
- Bug: for some reason, the xml parser shows "Submit" instead of "Next" at the end of the training feedback page (we can live with that).
- Enhancement: add AAU logo to the test page.
- Feedback: check description and test difficulty. If the test is too hard, consider moving to an audio evaluation test (e.g. a MUSHRA test on perceptual attributes). If the test doesn't work, strokes can be selected by looking at early decay crest factor as well.
