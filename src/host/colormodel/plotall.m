function plotall(drc, wait)

addpath(pwd);
cd(drc);

close;

hold on;
axis equal

if exist('pixels1.m')
	pixels1
	C1= map(pixels);
	plot(C1(:, 1), C1(:, 2), 'o');
	if wait
		pause;
	end
end
if exist('pixels2.m')
	pixels2
	C2=map(pixels);
	plot(C2(:, 1), C2(:, 2), '.');
	if wait
		pause;
	end
end
if exist('pixels3.m')
	pixels3
	C3= map(pixels);
	plot(C3(:, 1), C3(:, 2), 'x');
	if wait
		pause;
	end
end
if exist('pixels4.m')
	pixels4
	C4= map(pixels);
	plot(C4(:, 1), C4(:, 2), '*');
	if wait
		pause;
	end
end
if exist('pixels5.m')
	pixels5
	C5= map(pixels);
	plot(C5(:, 1), C5(:, 2), 'o');
	if wait
		pause;
	end
end
if exist('pixels6.m')
	pixels6
	C6= map(pixels);
	plot(C6(:, 1), C6(:, 2), '.');
	if wait
		pause;
	end
end
if exist('pixels7.m')
	pixels7
	C7= map(pixels);
	plot(C7(:, 1), C7(:, 2), 'x');
end

hold off;

cd '../../colormodel'
