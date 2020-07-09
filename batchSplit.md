#

for %i in (*.wav) do sox %i ./PsingleStrokes/%i silence -l  1 0.2 0.15%   1 0.4 0.15% : newfile : restart

for %i in (*.wav) do sox %i ./SsingleStrokes/%i silence -l  1 0.2 0.15%   1 0.4 0.15% : newfile : restart