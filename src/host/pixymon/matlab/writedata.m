function writedata(filename, D, precision)

fid = fopen(filename, 'w');

fwrite(fid, D, precision);

fclose(fid);