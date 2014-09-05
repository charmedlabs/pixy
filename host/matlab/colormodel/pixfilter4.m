function pixfilter4(x0, x1, y0, y1, LUT, bits, file)

if exist(file)
	I = imread(file, 'png');
	MI = membership4(x0, x1, y0, y1, LUT, I, bits);
	I = blend(I, MI);
	imwrite(I, ['mod' file], 'PNG')
end
