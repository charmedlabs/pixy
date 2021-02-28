#!/usr/bin/python

##
# @file get_frame.py
# @brief This script captures frames from the Pixy over USB. The output image is 640x400 resolution.
#        The pixy can only store 320x200 pixels due to the limited RAM. To get 640x400, four 320x200 images
#        are captured and stitched together. You can see the stitching effect if you move the camera.
#        This is only used for capturing still images to analyze the image quality of the IR light.
#        The images are saved automatically to grayscale bmp image format. Bmp is used because it is lossless
#        and it's easy to parse for analysis.
#
#        Note the image is not demosaiced. Each pixel follows the bayer pattern BG/GR.
#
# @copyright Copyright 2021 Matternet. All rights reserved.
#

import cv2
from datetime import datetime
import numpy as np
import os
import pixy
import time


VERSION = "v1.0.0"
IMAGES_DIR = "images"

# Pixy can only hold this much of pixels in its memory.
# To get a bigger picture, the image is stitched together with subsequent frames.
SUB_WIDTH = 320
SUB_HEIGHT = 200

# This is the resolution we want to work with
FRAME_WIDTH = 640
FRAME_HEIGHT = 400

# The bitmap images files are not compressed.
# Limit the number of images saved so it doesn't use up all storage space
# if user left it running forever!
IMAGE_SIZE = FRAME_WIDTH * FRAME_HEIGHT
MAX_SAVED_IMAGES_SIZE = 1 * 1000 * 1000 * 1000  # 1GB
MAX_SAVED_IMAGES_COUNT = int(MAX_SAVED_IMAGES_SIZE / IMAGE_SIZE)


## Get a subframe (320x200) of the 640x400 image
# @param data Buffer for receiving subframe from pixy
# @param frame Full frame image buffer
# @param xoffset X offset
# @param yoffset Y offset
def get_subframe(data, frame, xoffset, yoffset):
    # Get frame from camera. Mode 0x11 is to tell pixy to capture frame at 640x400.
    # However, pixy can only store 320x200 in RAM. Caller must make multiple calls to this function.
    pixy.pixy_cam_get_frame(0x11, xoffset, yoffset, SUB_WIDTH, SUB_HEIGHT, data)

    # Convert to numpy matrix
    for h in xrange(SUB_HEIGHT):
        for w in xrange(SUB_WIDTH):
            frame[yoffset + h, xoffset + w] = data[h * SUB_WIDTH + w]


## Main function to capture images from pixy camera
def main():
    print("Pixy Python " + VERSION)

    # Create directory to save images
    if not os.path.exists(IMAGES_DIR):
        os.mkdir(IMAGES_DIR)

    # Create a directory for this session
    image_dir = IMAGES_DIR + "/" + str(datetime.now().strftime("%Y%m%d_%H%M%S"))
    os.mkdir(image_dir)
    print("Saving images to " + image_dir)
    print("Press Q to quit.")

    pixy.pixy_init()            # Initialize Pixy interface
    pixy.pixy_command("stop")   # Stop default program

    # Allocate memory to store image from camera
    data = pixy.byteArray(SUB_WIDTH * SUB_HEIGHT)

    # Allocate numpy matrix to display
    frame = np.zeros((FRAME_HEIGHT, FRAME_WIDTH, 1), dtype=np.uint8)
    frame_cnt = 0

    while True:
        get_subframe(data, frame,   0,   0)
        get_subframe(data, frame, 320,   0)
        get_subframe(data, frame,   0, 200)
        get_subframe(data, frame, 320, 200)

        # Show image
        cv2.imshow('pixy', frame)

        # Save image to file. Use bitmap format because it's lossless and easy to read back/analyze.
        if frame_cnt < MAX_SAVED_IMAGES_COUNT:
            cv2.imwrite(image_dir + '/{:06d}.bmp'.format(frame_cnt), frame)
        frame_cnt = frame_cnt + 1

        # Check user request to exit
        if cv2.waitKey(1) == ord('q'):
            break

        # Sleep 20ms. Camera can only sample at 50fps max.
        time.sleep(.02)

    cv2.destroyAllWindows()
    pixy.pixy_close()


if __name__ == '__main__':
    main()
