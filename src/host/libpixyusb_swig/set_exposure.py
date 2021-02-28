#!/usr/bin/python

##
# @file set_exposure.py
# @brief This script sets the gain and exposure settings of the camera.
#
# @copyright Copyright 2021 Matternet. All rights reserved.
#

import os
import pixy
import sys


## Prints the current gain and exposure compensation settings.
def print_exposure():
    retval, gain, exp = pixy.pixy_cam_get_exposure_compensation()
    print("Gain: 0x{:02x}, Exposure 0x{:04x}".format(gain, exp))


## Main function to set gain and exposure compensation settings.
#  @param ecv Scalar value that includes gain and compensation to match PixyMon.
def main(ecv):
    pixy.pixy_init()
    pixy.pixy_command("stop")

    # Get current exposure values
    print("Current exposure setting:")
    print_exposure()

    # Set exposure value
    gain = ecv & 0xFF
    comp = (ecv >> 8) & 0xFFFF
    pixy.pixy_cam_set_exposure_compensation(gain, comp)

    # Confirm exposure value
    print("")
    print("Exposure set to:")
    print_exposure()

    # Close connection
    pixy.pixy_close()


if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Usage: " + sys.argv[0] + " <ECV_Hex>     # e.g. 0xA598")
    else:
        ecv = int(sys.argv[1], 16)
        main(ecv)
