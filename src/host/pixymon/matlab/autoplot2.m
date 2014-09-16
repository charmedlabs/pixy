clear;
close;
i=1;

% look for new files
while 1
	pause(.1);
	files = dir('pixels*.m');
	if size(files, 1) >= i
		i = size(files, 1);
		hold on;
		
		pause(.5);
		[L, LX, LY] = plotcluster(files(i).name);
		files(i).name
		LUT = generatelut(L);
		writedata('lut.bin', LUT, 'uint8');
	end
end

hold off;