clear;
pixels1;
pixels2;
pixels3;
pixels4;
pixels5;
pixels6;
pixels7;
pixels8;

P1 = plotPix(D1);
P2 = plotPix(D2);
P3 = plotPix(D3);
P4 = plotPix(D4);
P5 = plotPix(D5);
P6 = plotPix(D6);
P7 = plotPix(D7);
P8 = plotPix(D8);

L1 = polyfit(P1(1:end, 2), P1(1:end, 3), 1);
LX1 = [min(P1(1:end, 2)) - .1; max(P1(1:end, 2)) + .1];
LY1 = LX1*L1(1) + L1(2);
I1 = L1(2);
d = .001;
r = .95;
%while 1
%	if below(L1, [L1(1); I1])/size(P1, 1) >= r
%		break
%	end
%	I1 = I1 + d;
%end
L1U = [L1(1), I1];
PS1 = tan(atan(L1(1))+pi/2);
PX1 = mean(P1(1:end, 2));
LP1 = [PS1, L1U(1)*PX1 + L1U(2) - PS1*PX1];
LPX1 = [PX1 - .1; PX1 + .1];
LPY1 = LPX1*LP1(1) + LP1(2);

hold on;

plot(P1(1:end, 2), P1(1:end, 3), 'o');
plot(P2(1:end, 2), P2(1:end, 3), '.');
plot(P3(1:end, 2), P3(1:end, 3), 'x');
plot(P4(1:end, 2), P4(1:end, 3), '+', P5(1:end, 2), P5(1:end, 3), 'o', P6(1:end, 2), P6(1:end, 3), '.', P7(1:end, 2), P7(1:end, 3), 'x', P8(1:end, 2), P8(1:end, 3), '+', LX1, LY1, '-', LPX1, LPY1, '-');

hold off;
