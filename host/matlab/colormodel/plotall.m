clear;
close;

pixels1
C1= map(pixels);
pixels2
C2=map(pixels);
pixels3
C3= map(pixels);
pixels4
C4= map(pixels);
pixels5
C6= map(pixels);
pixels6
C5= map(pixels);
pixels7
C7= map(pixels);

hold on;
axis equal
plot(C1(:, 1), C1(:, 2), 'o');
plot(C2(:, 1), C2(:, 2), '.');
plot(C3(:, 1), C3(:, 2), 'x');
plot(C4(:, 1), C4(:, 2), '*');
plot(C5(:, 1), C5(:, 2), 'o');
plot(C6(:, 1), C6(:, 2), '.');
plot(C7(:, 1), C7(:, 2), 'x');


hold off;
