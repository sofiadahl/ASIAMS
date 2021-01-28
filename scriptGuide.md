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

To run locally, open a local server by running *~/ASIAMS/listeningTest/python/pythonServer.py* (requires Python), after which you can access the test at [http://localhost:8000/test.html?url=tests/drumStrokes.xml](http://localhost:8000/test.html?url=tests/drumStrokes.xml)

To be able to run the test, copy the following wav files in the folder *~/ASIAMS/listeningTest/media*:

- P1L36N008.wav
- P1L38N001.wav
- P1R62C001.wav
- P1R62C009.wav
- P1R63N006.wav
- P1R73N010.wav
- P1R73N006.wav
- P1R74C003.wav
- P1R74C005.wav
- P1R76C007.wav
- S2L56C001.wav
- S2L56C004.wav
- S2L56C009.wav
- S2L57N001.wav
- S2L58C001.wav
- S2L58C007.wav
- S2L59N003.wav
- S2L59N006.wav
- S2L68N005.wav
- S2L68N007.wav

Then, copy the following wav files from the [original repository](https://github.com/BrechtDeMan/WebAudioEvaluationTool) (folder */media/example*) to the folder *~/ASIAMS/listeningTest/media/example*:

- 1.wav
- 2.wav
- 3.wav
- 4.wav
- 5.wav

At the moment, the test contains three pages: training, P1, S2, so we are blocking per subject. The randomization happens on the client side: the pool size is 2, meaning that the test will contain the training page (always played, always on top) and one page between P1 and S2. Inside the P1/S2 pages, a pool size of 5 is selected, meaning that 5 strokes out of 10 (5 normal + 5 controlled) will be picked at random. This type of setup is affected by the sample size: with a few participants, we will be far from a uniform distribution of responses wrt subject and number of played strokes.

The server tracks a number of variables along with the actual response: total test time, time spent on each page, number of plays per stroke, master volume moved, etc.

The responses are saved as xml files in the *~/ASIAMS/listeningTest/saves/* folder (locally, in this case).

Status:

- Tested on Firefox/Win10 and Chrome/Win10.
- Todo: select strokes for training and test session.
- Todo: upload to server.
- Todo: check parsing scripts for the results.
- Bug: the interface looks slightly different in Firefox and Chrome (but we can live with that).
- Bug: for some reason, the xml parser shows "Submit" instead of "Next" at the end of the training feedback page.
- If the test is too hard, consider length and character of the training set.
- If the test is too hard, consider putting two reference strokes on each page.
- If the test is too hard, consider moving to an audio evaluation test (e.g. a MUSHRA test on perceptual attributes).
