//
// begin license header
//
// This file is part of Pixy CMUcam5 or "Pixy" for short
//
// All Pixy source code is provided under the terms of the
// GNU General Public License v2 (http://www.gnu.org/licenses/gpl-2.0.html).
// Those wishing to use Pixy source code, software and/or
// technologies under different licensing terms should contact us at
// cmucam@cs.cmu.edu. Such licensing terms are available for
// all portions of the Pixy codebase presented here.
//
// end license header
//

#ifndef __PIXYDEFS_H__
#define __PIXYDEFS_H__

#include "libusb.h"

#define PIXY_VID       0xB1AC
#define PIXY_DID       0xF000
#define PIXY_DFU_VID   0x1FC9
#define PIXY_DFU_DID   0x000C

#define PIXY_ERROR_USB_IO                    LIBUSB_ERROR_IO
#define PIXY_ERROR_USB_NOT_FOUND             LIBUSB_ERROR_NOT_FOUND
#define PIXY_ERROR_USB_BUSY                  LIBUSB_ERROR_BUSY
#define PIXY_ERROR_USB_NO_DEVICE             LIBUSB_ERROR_NO_DEVICE
#define PIXY_ERROR_INVALID_PARAMETER        -150
#define PIXY_ERROR_CHIRP                    -151
#define PIXY_ERROR_INVALID_COMMAND          -152

#ifndef END
#ifdef __x86_64__
#define END            (int64_t) 0
#else
#define END            0
#endif
#endif

#define END_OUT_ARGS   END
#define END_IN_ARGS    END

#endif
