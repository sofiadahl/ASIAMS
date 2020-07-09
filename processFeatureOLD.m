function [tableOverall,tableMeans,outlierList] = processFeature(data,featureToProcess,label, ...
    rmOutliers)

field = fieldnames(data);
featureOffset = 16;
featureIdx = featureOffset + featureToProcess;
outlierCountC = 0;
outlierCountN = 0;

% Remove outliers, i.e. values more than three times the interquartile
% range away from the median of each condition, participant, and audio descriptor 
% separately and excluded

outlierList = {};
% P1 - controlled
cIdx = (strcmp({data.subj},'P1') & strcmp({data.cond},'C'));

thisIqr = iqr([data(cIdx).(field{featureIdx})]);
thisMedian = median([data(cIdx).(field{featureIdx})]);

disp('Processing subject P1, condition C');

outlIdx = ([data(cIdx).(field{featureIdx})] > (thisMedian + 3 * thisIqr)) ...
    | ([data(cIdx).(field{featureIdx})] < (thisMedian - 3 * thisIqr));
if isempty(find(outlIdx, 1))
    disp('0 outliers found.');
else
    origIdx = find(cIdx);
    newIdx = origIdx(outlIdx);
    for j = 1:length(newIdx)
        outlier = data(newIdx(j)).filename;
        disp(['Stroke ' outlier ' is an outlier']);
        outlierList = [outlierList outlier];
        outlierCountC = outlierCountC + 1;
    end
end

% P1 - normal
nIdx = (strcmp({data.subj},'P1') & strcmp({data.cond},'N'));

thisIqr = iqr([data(nIdx).(field{featureIdx})]);
thisMedian = median([data(nIdx).(field{featureIdx})]);

disp('Processing subject P1, condition N');

outlIdx = ([data(nIdx).(field{featureIdx})] > (thisMedian + 3 * thisIqr)) ...
    | ([data(nIdx).(field{featureIdx})] < (thisMedian - 3 * thisIqr));
if isempty(find(outlIdx, 1))
    disp('0 outliers found.');
else
    origIdx = find(nIdx);
    newIdx = origIdx(outlIdx);
    for j = 1:length(newIdx)
        outlier = data(newIdx(j)).filename;
        disp(['Stroke ' outlier ' is an outlier']);
        outlierList = [outlierList outlier];
        outlierCountN = outlierCountN + 1;
    end
end

% P3 - controlled
cIdx = (strcmp({data.subj},'P3') & strcmp({data.cond},'C'));

thisIqr = iqr([data(cIdx).(field{featureIdx})]);
thisMedian = median([data(cIdx).(field{featureIdx})]);

disp('Processing subject P3, condition C');

outlIdx = ([data(cIdx).(field{featureIdx})] > (thisMedian + 3 * thisIqr)) ...
    | ([data(cIdx).(field{featureIdx})] < (thisMedian - 3 * thisIqr));
if isempty(find(outlIdx, 1))
    disp('0 outliers found.');
else
    origIdx = find(cIdx);
    newIdx = origIdx(outlIdx);
    for j = 1:length(newIdx)
        outlier = data(newIdx(j)).filename;
        disp(['Stroke ' outlier ' is an outlier']);
        outlierList = [outlierList outlier];
        outlierCountC = outlierCountC + 1;
    end
end

% P3 - normal
nIdx = (strcmp({data.subj},'P3') & strcmp({data.cond},'N'));

thisIqr = iqr([data(nIdx).(field{featureIdx})]);
thisMedian = median([data(nIdx).(field{featureIdx})]);

disp('Processing subject P3, condition N');

outlIdx = ([data(nIdx).(field{featureIdx})] > (thisMedian + 3 * thisIqr)) ...
    | ([data(nIdx).(field{featureIdx})] < (thisMedian - 3 * thisIqr));
if isempty(find(outlIdx, 1))
    disp('0 outliers found.');
else
    origIdx = find(nIdx);
    newIdx = origIdx(outlIdx);
    for j = 1:length(newIdx)
        outlier = data(newIdx(j)).filename;
        disp(['Stroke ' outlier ' is an outlier']);
        outlierList = [outlierList outlier];
        outlierCountN = outlierCountN + 1;
    end
end

% P4 - controlled
cIdx = (strcmp({data.subj},'P4') & strcmp({data.cond},'C'));

thisIqr = iqr([data(cIdx).(field{featureIdx})]);
thisMedian = median([data(cIdx).(field{featureIdx})]);

disp('Processing subject P4, condition C');

outlIdx = ([data(cIdx).(field{featureIdx})] > (thisMedian + 3 * thisIqr)) ...
    | ([data(cIdx).(field{featureIdx})] < (thisMedian - 3 * thisIqr));
if isempty(find(outlIdx, 1))
    disp('0 outliers found.');
else
    origIdx = find(cIdx);
    newIdx = origIdx(outlIdx);
    for j = 1:length(newIdx)
        outlier = data(newIdx(j)).filename;
        disp(['Stroke ' outlier ' is an outlier']);
        outlierList = [outlierList outlier];
        outlierCountC = outlierCountC + 1;
    end
end

% P4 - normal
nIdx = (strcmp({data.subj},'P4') & strcmp({data.cond},'N'));

thisIqr = iqr([data(nIdx).(field{featureIdx})]);
thisMedian = median([data(nIdx).(field{featureIdx})]);

disp('Processing subject P4, condition N');

outlIdx = ([data(nIdx).(field{featureIdx})] > (thisMedian + 3 * thisIqr)) ...
    | ([data(nIdx).(field{featureIdx})] < (thisMedian - 3 * thisIqr));
if isempty(find(outlIdx, 1))
    disp('0 outliers found.');
else
    origIdx = find(nIdx);
    newIdx = origIdx(outlIdx);
    for j = 1:length(newIdx)
        outlier = data(newIdx(j)).filename;
        disp(['Stroke ' outlier ' is an outlier']);
        outlierList = [outlierList outlier];
        outlierCountN = outlierCountN + 1;
    end
end

% P5 - controlled
cIdx = (strcmp({data.subj},'P5') & strcmp({data.cond},'C'));

thisIqr = iqr([data(cIdx).(field{featureIdx})]);
thisMedian = median([data(cIdx).(field{featureIdx})]);

disp('Processing subject P5, condition C');

outlIdx = ([data(cIdx).(field{featureIdx})] > (thisMedian + 3 * thisIqr)) ...
    | ([data(cIdx).(field{featureIdx})] < (thisMedian - 3 * thisIqr));
if isempty(find(outlIdx, 1))
    disp('0 outliers found.');
else
    origIdx = find(cIdx);
    newIdx = origIdx(outlIdx);
    for j = 1:length(newIdx)
        outlier = data(newIdx(j)).filename;
        disp(['Stroke ' outlier ' is an outlier']);
        outlierList = [outlierList outlier];
        outlierCountC = outlierCountC + 1;
    end
end

% P5 - normal
nIdx = (strcmp({data.subj},'P5') & strcmp({data.cond},'N'));

thisIqr = iqr([data(nIdx).(field{featureIdx})]);
thisMedian = median([data(nIdx).(field{featureIdx})]);

disp('Processing subject P5, condition N');

outlIdx = ([data(nIdx).(field{featureIdx})] > (thisMedian + 3 * thisIqr)) ...
    | ([data(nIdx).(field{featureIdx})] < (thisMedian - 3 * thisIqr));
if isempty(find(outlIdx, 1))
    disp('0 outliers found.');
else
    origIdx = find(nIdx);
    newIdx = origIdx(outlIdx);
    for j = 1:length(newIdx)
        outlier = data(newIdx(j)).filename;
        disp(['Stroke ' outlier ' is an outlier']);
        outlierList = [outlierList outlier];
        outlierCountN = outlierCountN + 1;
    end
end

% S1 - controlled
cIdx = (strcmp({data.subj},'S1') & strcmp({data.cond},'C'));

thisIqr = iqr([data(cIdx).(field{featureIdx})]);
thisMedian = median([data(cIdx).(field{featureIdx})]);

disp('Processing subject S1, condition C');

outlIdx = ([data(cIdx).(field{featureIdx})] > (thisMedian + 3 * thisIqr)) ...
    | ([data(cIdx).(field{featureIdx})] < (thisMedian - 3 * thisIqr));
if isempty(find(outlIdx, 1))
    disp('0 outliers found.');
else
    origIdx = find(cIdx);
    newIdx = origIdx(outlIdx);
    for j = 1:length(newIdx)
        outlier = data(newIdx(j)).filename;
        disp(['Stroke ' outlier ' is an outlier']);
        outlierList = [outlierList outlier];
        outlierCountC = outlierCountC + 1;
    end
end

% S1 - normal
nIdx = (strcmp({data.subj},'S1') & strcmp({data.cond},'N'));

thisIqr = iqr([data(nIdx).(field{featureIdx})]);
thisMedian = median([data(nIdx).(field{featureIdx})]);

disp('Processing subject S1, condition N');

outlIdx = ([data(nIdx).(field{featureIdx})] > (thisMedian + 3 * thisIqr)) ...
    | ([data(nIdx).(field{featureIdx})] < (thisMedian - 3 * thisIqr));
if isempty(find(outlIdx, 1))
    disp('0 outliers found.');
else
    origIdx = find(nIdx);
    newIdx = origIdx(outlIdx);
    for j = 1:length(newIdx)
        outlier = data(newIdx(j)).filename;
        disp(['Stroke ' outlier ' is an outlier']);
        outlierList = [outlierList outlier];
        outlierCountN = outlierCountN + 1;
    end
end

% S2 - controlled
cIdx = (strcmp({data.subj},'S2') & strcmp({data.cond},'C'));

thisIqr = iqr([data(cIdx).(field{featureIdx})]);
thisMedian = median([data(cIdx).(field{featureIdx})]);

disp('Processing subject S2, condition C');

outlIdx = ([data(cIdx).(field{featureIdx})] > (thisMedian + 3 * thisIqr)) ...
    | ([data(cIdx).(field{featureIdx})] < (thisMedian - 3 * thisIqr));
if isempty(find(outlIdx, 1))
    disp('0 outliers found.');
else
    origIdx = find(cIdx);
    newIdx = origIdx(outlIdx);
    for j = 1:length(newIdx)
        outlier = data(newIdx(j)).filename;
        disp(['Stroke ' outlier ' is an outlier']);
        outlierList = [outlierList outlier];
        outlierCountC = outlierCountC + 1;
    end
end

% S2 - normal
nIdx = (strcmp({data.subj},'S2') & strcmp({data.cond},'N'));

thisIqr = iqr([data(nIdx).(field{featureIdx})]);
thisMedian = median([data(nIdx).(field{featureIdx})]);

disp('Processing subject S2, condition N');

outlIdx = ([data(nIdx).(field{featureIdx})] > (thisMedian + 3 * thisIqr)) ...
    | ([data(nIdx).(field{featureIdx})] < (thisMedian - 3 * thisIqr));
if isempty(find(outlIdx, 1))
    disp('0 outliers found.');
else
    origIdx = find(nIdx);
    newIdx = origIdx(outlIdx);
    for j = 1:length(newIdx)
        outlier = data(newIdx(j)).filename;
        disp(['Stroke ' outlier ' is an outlier']);
        outlierList = [outlierList outlier];
        outlierCountN = outlierCountN + 1;
    end
end

% S3 - controlled
cIdx = (strcmp({data.subj},'S3') & strcmp({data.cond},'C'));

thisIqr = iqr([data(cIdx).(field{featureIdx})]);
thisMedian = median([data(cIdx).(field{featureIdx})]);

disp('Processing subject S3, condition C');

outlIdx = ([data(cIdx).(field{featureIdx})] > (thisMedian + 3 * thisIqr)) ...
    | ([data(cIdx).(field{featureIdx})] < (thisMedian - 3 * thisIqr));
if isempty(find(outlIdx, 1))
    disp('0 outliers found.');
else
    origIdx = find(cIdx);
    newIdx = origIdx(outlIdx);
    for j = 1:length(newIdx)
        outlier = data(newIdx(j)).filename;
        disp(['Stroke ' outlier ' is an outlier']);
        outlierList = [outlierList outlier];
        outlierCountC = outlierCountC + 1;
    end
end

% S3 - normal
nIdx = (strcmp({data.subj},'S3') & strcmp({data.cond},'N'));

thisIqr = iqr([data(nIdx).(field{featureIdx})]);
thisMedian = median([data(nIdx).(field{featureIdx})]);

disp('Processing subject S3, condition N');

outlIdx = ([data(nIdx).(field{featureIdx})] > (thisMedian + 3 * thisIqr)) ...
    | ([data(nIdx).(field{featureIdx})] < (thisMedian - 3 * thisIqr));
if isempty(find(outlIdx, 1))
    disp('0 outliers found.');
else
    origIdx = find(nIdx);
    newIdx = origIdx(outlIdx);
    for j = 1:length(newIdx)
        outlier = data(newIdx(j)).filename;
        disp(['Stroke ' outlier ' is an outlier']);
        outlierList = [outlierList outlier];
        outlierCountN = outlierCountN + 1;
    end
end

% S4 - controlled
cIdx = (strcmp({data.subj},'S4') & strcmp({data.cond},'C'));

thisIqr = iqr([data(cIdx).(field{featureIdx})]);
thisMedian = median([data(cIdx).(field{featureIdx})]);

disp('Processing subject S4, condition C');

outlIdx = ([data(cIdx).(field{featureIdx})] > (thisMedian + 3 * thisIqr)) ...
    | ([data(cIdx).(field{featureIdx})] < (thisMedian - 3 * thisIqr));
if isempty(find(outlIdx, 1))
    disp('0 outliers found.');
else
    origIdx = find(cIdx);
    newIdx = origIdx(outlIdx);
    for j = 1:length(newIdx)
        outlier = data(newIdx(j)).filename;
        disp(['Stroke ' outlier ' is an outlier']);
        outlierList = [outlierList outlier];
        outlierCountC = outlierCountC + 1;
    end
end

% S4 - normal
nIdx = (strcmp({data.subj},'S4') & strcmp({data.cond},'N'));

thisIqr = iqr([data(nIdx).(field{featureIdx})]);
thisMedian = median([data(nIdx).(field{featureIdx})]);

disp('Processing subject S4, condition N');

outlIdx = ([data(nIdx).(field{featureIdx})] > (thisMedian + 3 * thisIqr)) ...
    | ([data(nIdx).(field{featureIdx})] < (thisMedian - 3 * thisIqr));
if isempty(find(outlIdx, 1))
    disp('0 outliers found.');
else
    origIdx = find(nIdx);
    newIdx = origIdx(outlIdx);
    for j = 1:length(newIdx)
        outlier = data(newIdx(j)).filename;
        disp(['Stroke ' outlier ' is an outlier']);
        outlierList = [outlierList outlier];
        outlierCountN = outlierCountN + 1;
    end
end

dataClean = data;

% Remove nan values if present (S2, SPL)
xxx = [dataClean.(field{featureIdx})];
dataClean(isnan(xxx)) = [];
nanIdx = find(isnan(xxx));
if ~isempty(nanIdx)
    disp(['Removed ' num2str(length(nanIdx)) ' NaN values.']);
end

if (rmOutliers)
    for i = 1:length(outlierList)
        dataClean(strcmp(outlierList(i),{dataClean.filename})) = [];
    end
    nControlled = length(find(strcmp({data.cond},'C')));
    nNormal = length(find(strcmp({data.cond},'N')));
    
    disp(['Done. Removed ' num2str(outlierCountC) ' controlled strokes (' ...
        num2str(outlierCountC/nControlled*100) '%) and ' ...
        num2str(outlierCountN) ' normal strokes (' ...
        num2str(outlierCountN/nNormal*100) '%). Total ' ...
        num2str(outlierCountC+outlierCountN) ' strokes (' ...
        num2str((outlierCountC+outlierCountN)/length(data)*100) '%).'])
end

x = struct2cell(dataClean);
xx = x(featureIdx,:,:);
feature = cell2mat(reshape(xx, size(xx,1), size(xx,3)))';

featureMean = mean(feature);
featureCenterized = feature - featureMean;

% figure
% histogram(featureStandardized)
% title(label)
% figure
% qqplot(featureStandardized)
% title(label)

Stroke = {dataClean.filename}';
Subject = {dataClean.subj}';
Condition = {dataClean.cond}';
Arm = {dataClean.arm}';
tmp = {dataClean.series}';

% Replace series id's with integers
string = tmp{1};
cond = Condition{1};
subj = Subject{1};
if strcmp(cond,'C')
    countC = 1;
    countN = 0;
else
    countC = 0;
    countN = 1;
end

Series = ones(length(tmp),1);

for i = 1:length(tmp)
    if ~strcmp(subj,Subject{i})
        cond = Condition{i};
        if strcmp(cond,'C')
            countC = 1;
            countN = 0;
        else
            countC = 0;
            countN = 1;
        end
        subj = Subject{i};
        string = tmp{i};
    end
    if ~strcmp(tmp{i},string)
        string = tmp{i};
        cond = Condition{i};
        if strcmp(cond,'C')            
            countC = countC + 1;
        else
            countN = countN + 1;
        end
    end
    if strcmp(cond,'C')
        Series(i) = countC;
    else
        Series(i) = countN;
    end
end
Value = feature;
CenterizedValue = featureCenterized;

tableOverall = table(Stroke,Subject,Arm,Series,Condition,Value,CenterizedValue);

[uniqueSer,rowIdx,~] = unique(Series);
mSubject = {};
mValue = zeros(length(uniqueSer),1);
mCenterizedValue = zeros(length(uniqueSer),1);
mArm = {};
mCondition = {};
N = length(uniqueSer);

for i = 1:N-1
    mSubject{i} = Subject{rowIdx(i)};
    mValue(i) = mean(Value(rowIdx(i):rowIdx(i+1)-1));
    mCenterizedValue(i) = mean(CenterizedValue(rowIdx(i):rowIdx(i+1)-1));
    mArm{i} = Arm{rowIdx(i)};
    mCondition{i} = Condition{rowIdx(i)};
end

mSubject{N} = Subject{rowIdx(end)};
mArm{N} = Arm{rowIdx(end)};
mCondition{N} = Condition{rowIdx(end)};
mValue(N) = mean(Value(rowIdx(end):length(Value)));
mCenterizedValue(N) = mean(CenterizedValue(rowIdx(end):length(Value)));
mSubject = mSubject';
mArm = mArm';
mCondition = mCondition';

tableMeans = table(mSubject,mArm,mCondition,mValue,mCenterizedValue);

end