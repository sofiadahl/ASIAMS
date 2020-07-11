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