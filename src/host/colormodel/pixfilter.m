function pixfilter(x0, x1, y0, y1, file)

if exist(file)
	I = imread(file, 'png');
	MI = membership(x0, x1, y0, y1, I);
	I = blend(I, MI);
	imwrite(I, ['mod' file], 'PNG')
end
