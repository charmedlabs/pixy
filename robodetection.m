clear 'all';
close 'all';

Path = 'Z:/Documents/PixyMon/';
files = dir(strcat(Path, '*.png'));
while 1
    for file = files'
        Image = imread(strcat(Path, file.name));

        figure(1);

        subplot(3,3,1);
        imshow(Image);
        title('Original');

        ImageGray = rgb2gray(Image);

        subplot(3,3,2);
        imshow(ImageGray);
        title('gray');

        averaged = imfilter(ImageGray, fspecial('gaussian'));

        subplot(3,3,3);
        imshow(averaged);
        title('averaged');

        DX = [-1 0 1; -2 0 2; -1 0 1];
        DY = DX';
        
        ImageDy = imfilter(ImageGray, DY);
        ImageDx = imfilter(ImageGray, DX);

        subplot(3,3,4);
        imshow(ImageDy);
        title('dl/dy');

        summed = sum(ImageDy, 2);


        [h, w] = size(ImageGray);

        filteredSummed = (summed > (sum(summed)/length(summed)));
        filterMat = [ 1 2 1 ] * [ 1; 2; 1 ];
        filtered = (imfilter(ImageDy, filterMat/sum(sum(filterMat))) > 120);
        
        subplot(3,3,5);
        imshow(filtered);
        title('filtered dl/dy');
        
        allPlatePixels = [];
        for x = 11:w-10
            platePixels = 0;
            for y = h:-1:1
                if ((filtered(y, x) == 1))
                    break;
                end
                platePixels = platePixels + 1;
            end
            allPlatePixels = [allPlatePixels platePixels];
        end

        avgPlatePixels = sum(allPlatePixels) / length(allPlatePixels);
        sortedAllPlatePixels = sort(allPlatePixels);
        medianPlatePixels = sortedAllPlatePixels(length(allPlatePixels)/2);

        subplot(3,3,7);
        plot(allPlatePixels);
        title('sum');

        %ImageCroppedGray = imcrop(ImageGray, [ 0 h - medianPlatePixels  w medianPlatePixels]);

        subplot(3,3,8);
        imshow(ImageDx);
        title('dl/dx');

        subplot(3,3,9);
        imshow(ImageDx);
        title('dl/dx');

        %ImageCroppedDx = imfilter(ImageCroppedGray, DX);

        %subplot(3,3,7);
        %imshow(ImageCroppedDx);
        %title('dl/dx');

        pause(1)
    end
end