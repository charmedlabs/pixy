function pixfilter4(x0, x1, y0, y1, LUT, file)

if exist(file)
	I = imread(file, 'png');
	MI = membership4(x0, x1, y0, y1, LUT, I);
	I = blend(I, MI);
	imwrite(I, ['mod' file], 'PNG')
end
