function script3(drc, pfile)

close;

addpath(pwd);
cd(drc);

eval(pfile);

[L, LX, LY] = plotcluster(pixels);
C1 = map(pixels);

meanx = mean(C1(:, 1));
meany = mean(C1(:, 2));

[LUT1 LUT2]= generatelut(L, [meanx meany]);
hold on;
plotlut(L, LUT1);
if 0
	for i=1:size(pixels, 1)
		r = pixels(i, 1);
		g = pixels(i, 2);
		b = pixels(i, 3);
		v = typecast(bitshift(typecast((int16(r) - int16(g)), 'uint16'), -1), 'uint8');
		c1 = v(1);
		v = typecast(bitshift(typecast((int16(b) - int16(g)), 'uint16'), -1), 'uint8');
		c2 = v(1);
		
		cs1 = typecast(c1, 'int8');
		cs2 = typecast(c2, 'int8');
				
		bin = uint32(c1)*256+uint32(c2)+1;
		
		if LUT1(bin) 

			cs1 = int16(r) - int16(g);
			cs2 = int16(b) - int16(g);
			v = double(r)+double(g)+double(b);
			v = v/(3*255);
			if abs(cs1)>abs(cs2)
				i = uint32(c1)+1;
			else
				i = uint32(c2)+1;
			end
			if (LUT2(i, 1)<v) & (v<LUT2(i, 2))
				%'***'
				%[double(LUT2(i, 1)) double(LUT2(i, 2)) double(v) double(i) double(cs1) double(cs2)]	
			else
				[r g b]
				[double(LUT2(i, 1)) double(LUT2(i, 2)) double(v) double(i) double(cs1) double(cs2) double(c1) double(c2)]
				
			end			
		else
			
		end
	end

else

pixfilter3(LUT1, LUT2, 'image1.png');
pixfilter3(LUT1, LUT2, 'image2.png');
pixfilter3(LUT1, LUT2, 'image2.png');
pixfilter3(LUT1, LUT2, 'image3.png');
pixfilter3(LUT1, LUT2, 'image4.png');
pixfilter3(LUT1, LUT2, 'image5.png');
pixfilter3(LUT1, LUT2, 'image6.png');
pixfilter3(LUT1, LUT2, 'image7.png');
pixfilter3(LUT1, LUT2, 'image8.png');
end

cd '../../colormodel'

