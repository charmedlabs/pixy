function showimage(P, x, y)
I = reshape(P, x, y, 3);
I = permute(I, [2, 1, 3]);
image(I/255);
axis equal;

