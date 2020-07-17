clear
% close all

% Add MIRToolbox to path (replace with your own path)
addpath(genpath('C:\Coding\MATLAB\MIRtoolbox1.7.2\'));

%% Load calibration data

load calibration

%% Analysis

selectedStroke = 730;

% Legend:
% xxyzzkj -> PARTICIPANT# (xx), ARM (y), SERIES (zz), 
% CONDITION (k), STROKE# (j)

% Initialize data structure
% stroke has the following members:
% string filename : file name
% string subj : participant number
% string arm : arm
% string series : series
% string cond : condition
% string no : internal stroke number
% double[] audio : raw audio of stroke
% double fs : audio sample rate [Hz]
% double env: amplitude envelope of stroke
% double timeAxis : time axis for envelope (for plotting/alignment purposes) [ms]
% double onsetTime : onset time [ms]
% double onsetIdx : onset time [sample index]
% double offsetTime : offset time [ms]
% double offsetIdx : offset time [sample index]
% double maxPeakTime : max peak time [ms]
% double maxPeakIdx : max peak time [sample index]
% double TC : temporal centroid [ms]
% double totDur : total duration (offset time - onset time) [ms]
% double attDur : attack duration (max peak time - onset time) [ms]
% double decDur : decay duration (offset time - max peak time) [ms]
% double decDur : transient duration (temporal centroid - max peak time) [ms]
% double LAT : log attack time [log(s)]
% double totSPL : total SPL [dB SPL]
% double attSPL : SPL of attack phase [dB SPL]
% double decSPL : SPL of decay phase [dB SPL]
% double transSPL : SPL of transient phase [dB SPL]
% double totSC : spectral centroid (total duration) [Hz]
% double attSC : spectral centroid (attack phase) [Hz] 
% double decSC : spectral centroid (decay phase) [Hz]
% double transSC : spectral centroid (transient phase) [Hz]
% double totFlat : amplitude flatness (total duration)
% double attFlat : amplitude flatness (attack phase)
% double decFlat : amplitude flatness (decay phase)
% double transFlat : amplitude flatness (transient phase)
% double totCrest : crest factor (total duration)
% double attCrest : crest factor (attack phase)
% double decCrest : crest factor (decay phase)
% double transCrest : crest factor (transient phase)
% double totSpecFlat : spectral flatness (total duration)
% double attSpecFlat : spectral flatness (attack phase)
% double decSpecFlat : spectral flatness (decay phase)
% double transSpecFlat : spectral flatness (transient phase)
% transient phase = temporal centroid - max peak time

stroke = struct('filename', [], 'subj', [], 'arm', [], ...
    'series', [], 'cond', [], ...
    'no', [], 'audio', [], 'fs', [], 'env', [], ...
    'timeAxis', [], 'onsetTime', [], 'onsetIdx', [], ...
    'offsetTime', [], 'offsetIdx', [], ...
    'maxPeakTime', [], 'maxPeakIdx', [], ...
    'totDur', [], 'attDur', [], 'decDur', [], 'transDur', [], ...
    'LAT', [], ...
    'totSPL', [], 'attSPL', [], 'decSPL', [], 'transSPL', [], ...
    'totSC', [], 'attSC', [], 'decSC', [], 'transSC', [], ...
    'TC', [], ...
    'totFlat', [], 'attFlat', [], 'decFlat', [], 'transFlat', [], ...
    'totSpecFlat', [], 'attSpecFlat', [], 'decSpecFlat', [], 'transSpecFlat', [], ...
    'totCrest', [], 'attCrest', [], 'decCrest', [], 'transCrest', []);

% Retrieve wav-file names
cd singleStrokes
filesDir = dir('*.wav');

filename = filesDir(selectedStroke).name;
stroke.filename = filename;
stroke.subj = stroke.filename(1:2);
stroke.arm = stroke.filename(3);
stroke.series = stroke.filename(4:5);
stroke.cond = stroke.filename(6);
stroke.no = stroke.filename(7:end-4);
[audioStereo, stroke.fs] = audioread(filename);
% zero-pad audio file
stroke.audio = [zeros(100e-3*stroke.fs, 1); audioStereo(:,1); zeros(100e-3*stroke.fs, 1);];
cd ..
mirAudio = miraudio(stroke.audio);
% get onset time
attacks = mirevents(mirAudio,'Attacks','Waveform','WaveformThreshold',0.03);
peaks = uncell(get(attacks,'OnsetPosUnit'))*1e3;
stroke.onsetTime = peaks(1);
peaksIdx = uncell(get(attacks,'OnsetPos'));
stroke.onsetIdx = peaksIdx(1);
% Get offset time
decays = mirevents(mirAudio,'Decays','Waveform','WaveformThreshold',0.03);
peaks = uncell(get(decays,'OffsetPosUnit'))*1e3;
stroke.offsetTime = peaks(end);
peaksIdx = uncell(get(decays,'OffsetPos'));
stroke.offsetIdx = peaksIdx(end);
% Get max peak time
maxPeak = mirevents(mirAudio,'Attacks');
stroke.env = uncell(get(maxPeak,'Data'));
stroke.timeAxis = uncell(get(maxPeak,'Pos'))*1e3;
[~,stroke.maxPeakIdx] = max(stroke.env);
stroke.maxPeakTime = stroke.timeAxis(stroke.maxPeakIdx);
% Calculate temporal centroid
% Window length = 125 ms according to Danielsen et al. (2015)
% We need to allow a longer window due to the longer sustain.
% Note that the temporal centroid is affected by the window size (gets
% larger for a larger window).
% Three methods:
% 1) Fixed length (e.g. 500 ms)
winStartIdx = 1;
winEndIdx = round(500e-3*stroke.fs);
tcWindowedSignal = miraudio(stroke.audio(winStartIdx:winEndIdx),stroke.fs);
% 2) individual stroke duration
% tcWindowedSignal = miraudio(totalSegment,stroke.fs);
% 3) mean of the stroke durations
% to be done
tcWindowedEnvelope = mirenvelope(tcWindowedSignal);
tc = mircentroid(tcWindowedEnvelope,'MaxEntropy',0.99);
stroke.TC = mirgetdata(tc)*1e3;
% Calculate durations
stroke.totDur = stroke.offsetTime - stroke.onsetTime;
stroke.attDur = stroke.maxPeakTime - stroke.onsetTime;
stroke.decDur = stroke.offsetTime - stroke.maxPeakTime;
stroke.transDur = stroke.TC - stroke.maxPeakTime;
stroke.LAT = log(stroke.attDur*1e-3);
% Segment waveform (based on descriptors)
audioOnsetIdx = round(stroke.onsetTime*1e-3*stroke.fs);
audioOffsetIdx = round(stroke.offsetTime*1e-3*stroke.fs);
audioMaxPeakIdx = round(stroke.maxPeakTime*1e-3*stroke.fs);
audioTCIdx = round(stroke.TC*1e-3*stroke.fs);
attackSegment = stroke.audio(audioOnsetIdx:audioMaxPeakIdx);
decaySegment = stroke.audio(audioMaxPeakIdx:audioOffsetIdx);
totalSegment = stroke.audio(audioOnsetIdx:audioOffsetIdx);
transSegment = stroke.audio(audioMaxPeakIdx:audioTCIdx);
% Calculate SPLs
totalRMS = sqrt(mean((totalSegment.^2)));
attackRMS = sqrt(mean((attackSegment.^2)));
decayRMS = sqrt(mean((decaySegment.^2)));
transRMS = sqrt(mean((transSegment.^2)));
switch (stroke.subj)
    case 'S1'
        sens = sensS1;
    case 'S2'
        sens = nan;
    case 'S3'
        sens = sensS3;
    case 'S4'
        sens = sensS4;
    case 'P1'
        sens = sensP1;
    case 'P3'
        sens = sensP3;
    case 'P4'
        sens = sensP4;
    case 'P5'
        sens = sensP5;
    otherwise
        error('Illegal subject name');
end
totalPressureRMS = totalRMS/sens;
attackPressureRMS = attackRMS/sens;
decayPressureRMS = decayRMS/sens;
transPressureRMS = transRMS/sens;
stroke.totSPL = 20*log10(totalPressureRMS/pRef);
stroke.attSPL = 20*log10(attackPressureRMS/pRef);
stroke.decSPL = 20*log10(decayPressureRMS/pRef);
stroke.transSPL = 20*log10(transPressureRMS/pRef);
% Calculate spectral centroids
totSignal = miraudio(totalSegment.*hanning(size(totalSegment,1)),stroke.fs);
audioAttack = miraudio(attackSegment.*hanning(size(attackSegment,1)), stroke.fs);
audioDecay = miraudio(decaySegment.*hanning(size(decaySegment,1)), stroke.fs);
audioTrans = miraudio(transSegment.*hanning(size(transSegment,1)), stroke.fs);
specAttack = mirspectrum(audioAttack);
specDecay = mirspectrum(audioDecay);
specTrans = mirspectrum(audioTrans);
specTot = mirspectrum(totSignal);
scAttack = mircentroid(specAttack);
scDecay = mircentroid(specDecay);
scTot = mircentroid(specTot);
scTrans = mircentroid(specTrans);
stroke.attSC = mirgetdata(scAttack);
stroke.decSC = mirgetdata(scDecay);
stroke.totSC = mirgetdata(scTot);
stroke.transSC = mirgetdata(scTrans);
% Calculate temporal flatness
attFlat = mirflatness(mirenvelope(miraudio(unpad(attackSegment),stroke.fs),'Tau',1e-6));
decFlat = mirflatness(mirenvelope(miraudio(decaySegment, stroke.fs),'Tau',1e-6));
totFlat = mirflatness(mirenvelope(miraudio(unpad(totalSegment), stroke.fs),'Tau',1e-6));
transFlat = mirflatness(mirenvelope(miraudio(transSegment, stroke.fs),'Tau',1e-6));
stroke.attFlat = mirgetdata(attFlat);
stroke.decFlat = mirgetdata(decFlat);
stroke.totFlat = mirgetdata(totFlat);
stroke.transFlat = mirgetdata(transFlat);
% Calculate crest factor (|x_peak| / x_rms)
stroke.totCrest = max(abs(totalSegment)) / totalRMS;
stroke.attCrest = max(abs(attackSegment)) / attackRMS;
stroke.decCrest = max(abs(decaySegment)) / decayRMS;
stroke.transCrest = max(abs(transSegment)) / transRMS;
% Calculate spectral flatness
attSpecFlat = mirflatness(specAttack);
decSpecFlat = mirflatness(specDecay);
totSpecFlat = mirflatness(specTot);
transSpecFlat = mirflatness(audioTrans);
stroke.attSpecFlat = mirgetdata(attSpecFlat);
stroke.decSpecFlat = mirgetdata(decSpecFlat);
stroke.totSpecFlat = mirgetdata(totSpecFlat);
stroke.transSpecFlat = mirgetdata(transSpecFlat);
%% Plot single stroke
timeOffset = stroke.timeAxis(1);
tAudio = (0:length(stroke.audio)-1)/stroke.fs*1e3;
rectifiedAudio = abs(stroke.audio);
figure
plot(tAudio,rectifiedAudio)
hold on
plot(stroke.timeAxis, stroke.env)
plot(stroke.onsetTime, rectifiedAudio(stroke.onsetIdx), 'o')
plot(stroke.offsetTime, rectifiedAudio(stroke.offsetIdx), 'o')
plot(stroke.maxPeakTime, stroke.env(stroke.maxPeakIdx), 'o')
xline(stroke.TC, '--')
xlim([stroke.timeAxis(1) 1000])
legend('rectified waveform','amplitude envelope', 'onset', 'offset', 'max peak', ...
    'temporal centroid')
xlabel('time [ms]')
title({['Subject ' num2str(stroke.subj) ...
    ', arm = ' num2str(stroke.arm) ...
    ', condition = ' num2str(stroke.cond)]})

