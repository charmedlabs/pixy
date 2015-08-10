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
#define PIXY_PID       0xF000
#define PIXY_DFU_VID   0x1FC9
#define PIXY_DFU_PID   0x000C

#define PIXY_ERROR_USB_IO                    LIBUSB_ERROR_IO
#define PIXY_ERROR_USB_NOT_FOUND             LIBUSB_ERROR_NOT_FOUND
#define PIXY_ERROR_USB_BUSY                  LIBUSB_ERROR_BUSY
#define PIXY_ERROR_USB_NO_DEVICE             LIBUSB_ERROR_NO_DEVICE
#define PIXY_ERROR_INVALID_PARAMETER        -150
#define PIXY_ERROR_CHIRP                    -151
#define PIXY_ERROR_INVALID_COMMAND          -152

#define CRP_ARRAY                       0x80 // bit
#define CRP_FLT                         0x10 // bit
#define CRP_NO_COPY                     (0x10 | 0x20)
#define CRP_NULLTERM_ARRAY              (0x20 | CRP_ARRAY) // bits
#define CRP_INT8                        0x01
#define CRP_UINT8                       0x01
#define CRP_INT16                       0x02
#define CRP_UINT16                      0x02
#define CRP_INT32                       0x04
#define CRP_UINT32                      0x04
#define CRP_FLT32                       (CRP_FLT | 0x04)
#define CRP_FLT64                       (CRP_FLT | 0x08)
#define CRP_STRING                      (CRP_NULLTERM_ARRAY | CRP_INT8)
#define CRP_TYPE_HINT                   0x64 // type hint identifier
#define CRP_INTS8                       (CRP_INT8 | CRP_ARRAY)
#define CRP_INTS16                      (CRP_INT16 | CRP_ARRAY)
#define CRP_INTS32                      (CRP_INT32 | CRP_ARRAY)
#define CRP_UINTS8                      CRP_INTS8
#define CRP_UINTS8_NO_COPY              (CRP_INTS8 | CRP_NO_COPY)
#define CRP_UINTS16_NO_COPY             (CRP_INTS16 | CRP_NO_COPY)
#define CRP_UINTS32_NO_COPY             (CRP_INTS32 | CRP_NO_COPY)
#define CRP_UINTS16                     CRP_INTS16
#define CRP_UINTS32                     CRP_INTS32
#define CRP_FLTS32                      (CRP_FLT32 | CRP_ARRAY)
#define CRP_FLTS64                      (CRP_FLT64 | CRP_ARRAY)

// regular call args
#define INT8(v)                         CRP_INT8, v
#define UINT8(v)                        CRP_INT8, v
#define INT16(v)                        CRP_INT16, v
#define UINT16(v)                       CRP_INT16, v
#define INT32(v)                        CRP_INT32, v
#define UINT32(v)                       CRP_INT32, v
#define FLT32(v)                        CRP_FLT32, v
#define FLT64(v)                        CRP_FLT64, v
#define STRING(s)                       CRP_STRING, s
#define INTS8(len, a)                   CRP_INTS8, len, a
#define UINTS8(len, a)                  CRP_INTS8, len, a
#define UINTS8_NO_COPY(len)             CRP_UINTS8_NO_COPY, len
#define UINTS16_NO_COPY(len)            CRP_UINTS16_NO_COPY, len
#define UINTS32_NO_COPY(len)            CRP_UINTS32_NO_COPY, len
#define INTS16(len, a)                  CRP_INTS16, len, a
#define UINTS16(len, a)                 CRP_INTS16, len, a
#define INTS32(len, a)                  CRP_INTS32, len, a
#define UINTS32(len, a)                 CRP_INTS32, len, a
#define FLTS32(len, a)                  CRP_FLTS32, len, a
#define FLTS64(len, a)                  CRP_FLTS64, len, a

#ifndef END
#ifdef __x86_64__
#define END (int64_t)0
#else
#define END 0
#endif
#endif
#define END_OUT_ARGS END
#define END_IN_ARGS END

#endif
