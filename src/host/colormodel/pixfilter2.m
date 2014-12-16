function pixfilter2(LUT, file)

if exist(file)
	I = imread(file, 'png');
	MI = membership2(LUT, I);
	I = blend(I, MI);
	imwrite(I, ['mod' file], 'PNG')
end
