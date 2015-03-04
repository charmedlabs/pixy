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

#ifndef CHIRP_H
#define CHIRP_H

#include <stdint.h>
#include <stdlib.h>
#include <stdarg.h>

#undef FALSE
#define FALSE 0
#undef TRUE
#define TRUE 1

#define CRP_ERROR_CORRECTED
#define CRP_SHARED_MEM

#define CRP_BLK_SIZE                    0x200
#ifdef CRP_ERROR_CORRECTED
#define CRP_HEADER_LEN                  12
#else
#define CRP_HEADER_LEN                  8
#endif
#define CRP_MAX_NAK           		3
#define CRP_RETRIES                     3
#define CRP_HEADER_TIMEOUT    		500
#define CRP_DATA_TIMEOUT      		100
#define CRP_IDLE_TIMEOUT      		100
#define CRP_SEND_TIMEOUT      		500

#define CRP_RES_OK                      0
#define CRP_RES_ERROR                   -1
#define CRP_RES_ERROR_RECV_TIMEOUT      -100
#define CRP_RES_ERROR_SEND_TIMEOUT      -101
#define CRP_RES_ERROR_CRC               -2
#define CRP_RES_ERROR_PARSE             -3
#define CRP_RES_ERROR_MAX_NAK           -4
#define CRP_RES_ERROR_MEMORY            -5
#define CRP_RES_ERROR_NOT_CONNECTED     -6

#define CRP_MAX_ARGS          		10
#define CRP_BUFSIZE           		0x80
#define CRP_BUFPAD            		8
#define CRP_PROCTABLE_LEN     		0x20

#define CRP_START_CODE        		0xaaaa5555

#define CRP_CALL              		0x80
#define CRP_RESPONSE          		0x40
#define CRP_INTRINSIC          		0x20
#define CRP_DATA                    0x10
#define CRP_CALL_ENUMERATE    		(CRP_CALL | CRP_INTRINSIC | 0x00)
#define CRP_CALL_INIT         		(CRP_CALL | CRP_INTRINSIC | 0x01)

#define CRP_ACK                         0x59
#define CRP_NACK                        0x95
#define CRP_MAX_HEADER_LEN              64

#define CRP_ARRAY                       0x80 // bit
#define CRP_FLT                         0x10 // bit
#define CRP_HINT                        0x40 // bit
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
#define CRP_UINTS16                     CRP_INTS16
#define CRP_UINTS32                     CRP_INTS32
#define CRP_FLTS32                      (CRP_FLT32 | CRP_ARRAY)
#define CRP_FLTS64                      (CRP_FLT64 | CRP_ARRAY)
#define CRP_HINTS8                      (CRP_INT8 | CRP_ARRAY | CRP_HINT)
#define CRP_HINTS16                     (CRP_INT16 | CRP_ARRAY | CRP_HINT)
#define CRP_HINTS32                     (CRP_INT32 | CRP_ARRAY | CRP_HINT)
#define CRP_HFLTS32                     (CRP_FLT32 | CRP_ARRAY | CRP_HINT)
#define CRP_HFLTS64                     (CRP_FLT64 | CRP_ARRAY | CRP_HINT)
#define CRP_HSTRING                     (CRP_STRING | CRP_HINT)
// CRP_HTYPE is for arg lists which are uint8_t arrays
#define CRP_HTYPE(v)                    CRP_TYPE_HINT, (uint8_t)(v>>0&0xff), (uint8_t)(v>>8&0xff), (uint8_t)(v>>16&0xff), (uint8_t)(v>>24&0xff)

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
#define INTS16(len, a)                  CRP_INTS16, len, a
#define UINTS16(len, a)                 CRP_INTS16, len, a
#define INTS32(len, a)                  CRP_INTS32, len, a
#define UINTS32(len, a)                 CRP_INTS32, len, a
#define FLTS32(len, a)                  CRP_FLTS32, len, a
#define FLTS64(len, a)                  CRP_FLTS64, len, a

// hint call args
#define HINT8(v)                        CRP_HINT8, v
#define UHINT8(v)                       CRP_HINT8, v
#define HINT16(v)                       CRP_HINT16, v
#define UHINT16(v)                      CRP_HINT16, v
#define HINT32(v)                       CRP_HINT32, v
#define UHINT32(v)                      CRP_HINT32, v
#define HFLT32(v)                       CRP_HFLT32, v
#define HFLT64(v)                       CRP_HFLT64, v
#define HSTRING(s)                      CRP_HSTRING, s
#define HINTS8(len, a)                  CRP_HINTS8, len, a
#define UHINTS8(len, a)                 CRP_HINTS8, len, a
#define HINTS16(len, a)                 CRP_HINTS16, len, a
#define UHINTS16(len, a)                CRP_HINTS16, len, a
#define HINTS32(len, a)                 CRP_HINTS32, len, a
#define UHINTS32(len, a)                CRP_HINTS32, len, a
#define HFLTS32(len, a)                 CRP_HFLTS32, len, a
#define HFLTS64(len, a)                 CRP_HFLTS64, len, a
#define HTYPE(v)                        CRP_TYPE_HINT, v

#define INT8_IN(v)                      const int8_t * v
#define UINT8_IN(v)                     const uint8_t * v
#define INT16_IN(v)                     const int16_t * v
#define UINT16_IN(v)                    const uint16_t * v
#define INT32_IN(v)                     const int32_t * v
#define UINT32_IN(v)                    const uint32_t * v
#define FLT32_IN(v)                     const float * v
#define FLT64_IN(v)                     const double * v
#define STRING_IN(s)                    const const char * s
#define INTS8_IN(len, a)                const uint32_t * len, const int8_t * a
#define UINTS8_IN(len, a)               const uint32_t * len, const uint8_t * a
#define INTS16_IN(len, a)               const uint32_t * len, const int16_t * a
#define UINTS16_IN(len, a)              const uint32_t * len, const uint16_t * a
#define INTS32_IN(len, a)               const uint32_t * len, const int32_t * a
#define UINTS32_IN(len, a)              const uint32_t * len, const uint32_t * a
#define FLTS32_IN(len, a)               const uint32_t * len, const float * a
#define FLTS64_IN(len, a)               const uint32_t * len, const double * a

#define END                             0
#define END_SEND_ARGS                   END
#define END_RECV_ARGS                   END

// service types
#define SYNC                            0
#define ASYNC                           1

#define CRP_RETURN(...)                 chirpAssemble(0, __VA_ARGS__, END)
#define chirpCallSync(...)              chirpCall(SYNC, __VA_ARGS__, END)
#define chirpCallAsync(...)             chirpCall(ASYNC, __VA_ARGS__, END)


typedef int bool;
typedef int16_t ChirpProc;

typedef uint32_t (*ProcPtr)(void);

typedef struct
{
    const char *procName;
    ProcPtr procPtr;
    ChirpProc chirpProc;
}
ProcTableEntry;


int chirpOpen(void);
int chirpClose(void);
ChirpProc chirpGetProc(const char *procName, ProcPtr callback);
int chirpSetProc(const char *procName, ProcPtr proc);
int chirpCall(uint8_t service, ChirpProc proc, ...);
uint8_t chirpGetType(void *arg);
int chirpService(void);
int chirpAssemble(int dummy, ...);
int chirpRemoteInit(void);

// link flag index
#define LINK_FLAG_INDEX_SHARED_MEMORY_LOCATION          0x01
#define LINK_FLAG_INDEX_SHARED_MEMORY_SIZE              0x02

extern int32_t __attribute__((weak)) chirpInit(void);
extern int linkSend(const uint8_t *data, uint32_t len, uint16_t timeoutMs);
extern int linkReceive(uint8_t *data, uint32_t len, uint16_t timeoutMs);
extern uint32_t linkBlockSize(void);
extern uint32_t linkGetFlags(uint8_t index);
#endif // CHIRP_H
