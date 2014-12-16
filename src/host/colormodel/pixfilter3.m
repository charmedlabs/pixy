function pixfilter3(LUT1, LUT2, file)

if exist(file)
	I = imread(file, 'png');
	MI = membership3(LUT1, LUT2, I);
	I = blend(I, MI);
	imwrite(I, ['mod' file], 'PNG')
end
