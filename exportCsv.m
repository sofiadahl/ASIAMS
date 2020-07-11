clear
close all

load data

dataTable = struct2table(data);
dataTable(:,7:16) = [];

% handedness:
% P1 - right
% P3 - right (affected)
% P4 - left (affected)
% P5 - right
% S1 - assume right
% S2 - assume right
% S3 - assume right
% S4 - assume right

% Replace arm labels with handedness labels

for i = 1:size(dataTable,1)
    if strcmp(dataTable.subj(i),'P1') || strcmp(dataTable.subj(i),'P3') ...
            || strcmp(dataTable.subj(i),'P5') || strcmp(dataTable.subj(i),'S1') ...
            || strcmp(dataTable.subj(i),'S2') || strcmp(dataTable.subj(i),'S3') ...
            || strcmp(dataTable.subj(i),'S4')
        if strcmp(dataTable.arm(i),'R')
            dataTable.arm(i) = {'D'};
        else
            dataTable.arm(i) = {'ND'};
        end
    else
        if strcmp(dataTable.arm(i),'L')
            dataTable.arm(i) = {'D'};
        else
            dataTable.arm(i) = {'ND'};
        end
    end
end      

% Replace series id's with integers

subjects = {'P1', 'P3', 'P4', 'P5', 'S1', 'S2', 'S3', 'S4'};

for s = 1:length(subjects)
    count = 1;
    subjIdx = find(strcmp(dataTable.subj,subjects{s}));
    [~,uniqueRows,~] = unique(dataTable.series(subjIdx),'stable');
    for i = 1:length(uniqueRows)-1
        dataTable.series(subjIdx(1)+uniqueRows(i)-1:subjIdx(1)+uniqueRows(i+1)-1) = count;
        count = count + 1;
    end
    dataTable.series(subjIdx(1)+uniqueRows(end)-1:subjIdx(end)) = count;
end

%% Save table to csv file
writetable(dataTable,'features.csv')
