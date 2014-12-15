function file = newfile(name)

files = dir([name '*.m']);
i = size(files, 1);

[dx, dx] = sort([files.datenum]);
file = files(dx(end)).name;
file = file(1:end-2);