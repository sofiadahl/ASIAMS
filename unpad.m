function xUnpadded = unpad(x)

% removes zeros from the beginning of a vector

start = find(x~= 0, 1, 'first');
xUnpadded = x(start:end);

end