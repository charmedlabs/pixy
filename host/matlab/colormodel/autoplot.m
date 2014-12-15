clear;

files = dir('pixels*.m');
i = size(files, 1);

% look for latest file
files = dir('pixels*.m');
[dx, dx] = sort([files.datenum]);
file = files(dx(end)).name
Pixels = feval(file(1:end-2));
[L, LX, LY] = plotcluster(Pixels);
LUT = generatelut(L);
writedata('lut.bin', LUT, 'uint8');
