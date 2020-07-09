clear
close all

%% Get mic+preamp sensitivity from calibration files

% P1
[calP1,fs] = audioread('calibrationFiles/P1-MicCal1-78.5dBA.wav');

% Inspection
t = (0:size(calP1,1)-1)/fs;

figure
subplot(2,1,1)
plot(t,calP1(:,1))
title('P1 - Left channel')
ylim([-0.2 0.2])
subplot(2,1,2)
plot(t,calP1(:,1),'r')
ylim([-0.2 0.2])
title('P1 - Right channel')

% Remove on/off portions, left channel
calP1Cropped = calP1(1*fs:9*fs,1);

% RMS of measured signal
calP1RMS = sqrt(mean(calP1Cropped.^2));

% Measured SPL
calP1SPL = 78.5;
pRef = 20e-6;
calP1p = 10^(calP1SPL/20)*pRef;

% Mic+preamp sensitivity, P1
sensP1 = calP1RMS/calP1p; % [1/Pa]

% P2 - audio file is missing!

% P3
[calP3,fs] = audioread('calibrationFiles/P3-MicCal1-77.5dBA.wav');

% Inspection
t = (0:size(calP3,1)-1)/fs;

figure
subplot(2,1,1)
plot(t,calP3(:,1))
title('P3 - Left channel')
ylim([-0.2 0.2])
subplot(2,1,2)
plot(t,calP3(:,1),'r')
ylim([-0.2 0.2])
title('P3 - Right channel')

% Remove on/off portions, left channel
calP3Cropped = calP3(1*fs:7*fs,1);

% RMS of measured signal
calP3RMS = sqrt(mean(calP3Cropped.^2));

% Measured SPL
calP3SPL = 77.5;
calP3p = 10^(calP3SPL/20)*pRef;

% Mic+preamp sensitivity, P3
sensP3 = calP3RMS/calP3p; % [1/Pa]

% Session 4
[calP4,fs] = audioread('calibrationFiles/P4-MicCal1-75.3dBA.wav');

% Inspection
t = (0:size(calP4,1)-1)/fs;

figure
subplot(2,1,1)
plot(t,calP4(:,1))
title('P4 - Left channel')
ylim([-0.2 0.2])
subplot(2,1,2)
plot(t,calP4(:,1),'r')
ylim([-0.2 0.2])
title('P4 - Right channel')

% Remove on/off portions, left channel
calP4Cropped = calP4(1*fs:7*fs,1);

% RMS of measured signal
calP4RMS = sqrt(mean(calP4Cropped.^2));

% Measured SPL
calP4SPL = 75.3;
calP4p = 10^(calP4SPL/20)*pRef;

% Mic+preamp sensitivity, P4
sensP4 = calP4RMS/calP4p; % [1/Pa]

% Session 5
[calP5,fs] = audioread('calibrationFiles/P5-MicCal1-80.7dBA.wav');

% Inspection
t = (0:size(calP5,1)-1)/fs;

% The signal seems to be less constant in amplitude, perhaps just the
% effect of the higher level? Let's consider avoiding the perturbation
% around 5 s

figure
subplot(2,1,1)
plot(t,calP5(:,1))
title('P5 - Left channel')
ylim([-0.2 0.2])
subplot(2,1,2)
plot(t,calP5(:,1),'r')
ylim([-0.2 0.2])
title('P5 - Right channel')

% Remove on/off portions, left channel
calP5Cropped = calP5(1*fs:7*fs,1);

% RMS of measured signal
calP5RMS = sqrt(mean(calP5Cropped.^2));

% Measured SPL
calP5SPL = 80.7;
calP5p = 10^(calP5SPL/20)*pRef;

% Mic+preamp sensitivity, P5
sensP5 = calP5RMS/calP5p; % [1/Pa]

%% Get mic+preamp sensitivity from calibration files

% Subject 1
[calS1,fs] = audioread('calibrationFiles/S1-MicCal2_83.1dBA.wav');

% Inspection
t = (0:size(calS1,1)-1)/fs;

figure
subplot(2,1,1)
plot(t,calS1(:,1))
title('S1 - Left channel')
ylim([-0.2 0.2])
subplot(2,1,2)
plot(t,calS1(:,1),'r')
ylim([-0.2 0.2])
title('S1 - Right channel')

% Remove on/off portions, left channel
calS1Cropped = calS1(1*fs:9*fs,1);

% RMS of measured signal
calS1RMS = sqrt(mean(calS1Cropped.^2));

% Measured SPL
calS1SPL = 83.1;
pRef = 20e-6;
calS1p = 10^(calS1SPL/20)*pRef;

% Mic+preamp sensitivity, S1
sensS1 = calS1RMS/calS1p; % [1/Pa]

% Subject 2 - audio file is empty!

% Subject 3
[calS3,fs] = audioread('calibrationFiles/S3-MicCal1-81.8dBA.wav');

% Inspection
t = (0:size(calS3,1)-1)/fs;

figure
subplot(2,1,1)
plot(t,calS3(:,1))
title('S3 - Left channel')
ylim([-0.2 0.2])
subplot(2,1,2)
plot(t,calS3(:,1),'r')
ylim([-0.2 0.2])
title('S3 - Right channel')

% Remove on/off portions, left channel
calS3Cropped = calS3(1*fs:9*fs,1);

% RMS of measured signal
calS3RMS = sqrt(mean(calS3Cropped.^2));

% Measured SPL
calS3SPL = 81.8;
calS3p = 10^(calS3SPL/20)*pRef;

% Mic+preamp sensitivity, S3
sensS3 = calS3RMS/calS3p; % [1/Pa]

% Session 4
[calS4,fs] = audioread('calibrationFiles/S4-MicCal1-86.4dBA.wav');

% Inspection
t = (0:size(calS4,1)-1)/fs;
% The signal seems to be less constant in amplitude, perhaps just the
% effect of the higher level? Let's consider avoiding the perturbation
% around 5 s

figure
subplot(2,1,1)
plot(t,calS4(:,1))
title('S4 - Left channel')
ylim([-0.2 0.2])
subplot(2,1,2)
plot(t,calS4(:,1),'r')
ylim([-0.2 0.2])
title('S4 - Right channel')

% Remove on/off portions, left channel
calS4Cropped = calS4(1*fs:7*fs,1);

% RMS of measured signal
calS4RMS = sqrt(mean(calS4Cropped.^2));

% Measured SPL
calS4SPL = 86.4;
calS4p = 10^(calS4SPL/20)*pRef;

% Mic+preamp sensitivity, S4
sensS4 = calS4RMS/calS4p; % [1/Pa]

save calibration.mat sensP1 sensP3 sensP4 sensP5 sensS1 sensS3 sensS4 ...
    calP1SPL calP3SPL calP4SPL calP5SPL calS1SPL pRef
