function file = newfile()

files = dir('pixels*.m');
i = size(files, 1);

% look for latest file
files = dir('*.m');
[dx, dx] = sort([files.datenum]);
file = files(dx(end)).name
file = file(1:end-2);