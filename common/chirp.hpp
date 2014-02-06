#ifndef CHIRP_HPP
#define CHIRP_HPP

#include <stdint.h>
#include <stdlib.h>
#include <stdarg.h>
#include "link.h"

#define ALIGN(v, n)  v = v&((n)-1) ? (v&~((n)-1))+(n) : v
#define FOURCC(a, b, c, d)  (((uint32_t)a<<0)|((uint32_t)b<<8)|((uint32_t)c<<16)|((uint32_t)d<<24))

#define CRP_RES_OK                      0
#define CRP_RES_ERROR                   -1
#define CRP_RES_ERROR_RECV_TIMEOUT      LINK_RESULT_ERROR_RECV_TIMEOUT
#define CRP_RES_ERROR_SEND_TIMEOUT      LINK_RESULT_ERROR_SEND_TIMEOUT
#define CRP_RES_ERROR_CRC               -2
#define CRP_RES_ERROR_PARSE             -3
#define CRP_RES_ERROR_MAX_NAK           -4
#define CRP_RES_ERROR_MEMORY            -5
#define CRP_RES_ERROR_NOT_CONNECTED     -6

#define CRP_MAX_NAK           		3
#define CRP_RETRIES                     3
#define CRP_HEADER_TIMEOUT    		1000
#define CRP_DATA_TIMEOUT      		500
#define CRP_IDLE_TIMEOUT      		500
#define CRP_SEND_TIMEOUT      		1000
#define CRP_MAX_ARGS          		10
#define CRP_BUFSIZE           		0x80
#define CRP_BUFPAD            		8
#define CRP_PROCTABLE_LEN     		0x40

#define CRP_START_CODE        		0xaaaa5555

#define CRP_CALL              		0x80
#define CRP_RESPONSE          		0x40
#define CRP_INTRINSIC          		0x20
#define CRP_DATA                        0x10
#define CRP_XDATA                       0x18 // data not associated with no associated procedure)
#define CRP_CALL_ENUMERATE    		(CRP_CALL | CRP_INTRINSIC | 0x00)
#define CRP_CALL_INIT         		(CRP_CALL | CRP_INTRINSIC | 0x01)
#define CRP_CALL_ENUMERATE_INFO         (CRP_CALL | CRP_INTRINSIC | 0x02)

#define CRP_ACK                         0x59
#define CRP_NACK                        0x95
#define CRP_MAX_HEADER_LEN              64

#define CRP_ARRAY                       0x80 // bit
#define CRP_FLT                         0x10 // bit
#define CRP_NO_COPY                     (0x10 | 0x20)
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
#define CRP_UINTS8_NO_COPY              (CRP_INTS8 | CRP_NO_COPY)
#define CRP_UINTS16_NO_COPY             (CRP_INTS16 | CRP_NO_COPY)
#define CRP_UINTS32_NO_COPY             (CRP_INTS32 | CRP_NO_COPY)
#define CRP_UINTS16                     CRP_INTS16
#define CRP_UINTS32                     CRP_INTS32
#define CRP_FLTS32                      (CRP_FLT32 | CRP_ARRAY)
#define CRP_FLTS64                      (CRP_FLT64 | CRP_ARRAY)
#define CRP_HINT8                       (CRP_INT8 | CRP_HINT)
#define CRP_HINT16                      (CRP_INT16 | CRP_HINT)
#define CRP_HINT32                      (CRP_INT32 | CRP_HINT)
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
#define UINTS8_NO_COPY(len)             CRP_UINTS8_NO_COPY, len
#define UINTS16_NO_COPY(len)            CRP_UINTS16_NO_COPY, len
#define UINTS32_NO_COPY(len)            CRP_UINTS32_NO_COPY, len
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

#define INT8_IN(v)                      int8_t & v
#define UINT8_IN(v)                     uint8_t & v
#define INT16_IN(v)                     int16_t & v
#define UINT16_IN(v)                    uint16_t & v
#define INT32_IN(v)                     int32_t & v
#define UINT32_IN(v)                    uint32_t & v
#define FLT32_IN(v)                     float & v
#define FLT64_IN(v)                     double & v
#define STRING_IN(s)                    const char * s
#define INTS8_IN(len, a)                uint32_t & len, int8_t * a
#define UINTS8_IN(len, a)               uint32_t & len, uint8_t * a
#define INTS16_IN(len, a)               uint32_t & len, int16_t * a
#define UINTS16_IN(len, a)              uint32_t & len, uint16_t * a
#define INTS32_IN(len, a)               uint32_t & len, int32_t * a
#define UINTS32_IN(len, a)              uint32_t & len, uint32_t * a
#define FLTS32_IN(len, a)               uint32_t & len, float * a
#define FLTS64_IN(len, a)               uint32_t & len, double * a

#ifdef __x86_64__
#define END                             (int64_t)0
#else
#define END                             0
#endif
#define END_OUT_ARGS                    END
#define END_IN_ARGS                     END

// service types
#define SYNC                            0
#define ASYNC                           0x01 // bit
#define RETURN_ARRAY                    0x02 // bit
#define SYNC_RETURN_ARRAY               (SYNC | RETURN_ARRAY)

#define CRP_RETURN(chirp, ...)          chirp->assemble(0, __VA_ARGS__, END)
#define CRP_SEND_XDATA(chirp, ...)      chirp->assemble(CRP_XDATA, __VA_ARGS__, END)
#define callSync(...)                   call(SYNC, __VA_ARGS__, END)
#define callAsync(...)                  call(ASYNC, __VA_ARGS__, END)
#define callSyncArray(...)              call(SYNC_RETURN_ARRAY, __VA_ARGS__, END)

class Chirp;

typedef int16_t ChirpProc; // negative values are invalid

typedef uint32_t (*ProcPtr)(Chirp *);

struct ProcModule
{
    char *procName;
    ProcPtr procPtr;
    uint8_t argTypes[CRP_MAX_ARGS];
    char *procInfo;
};

struct ProcTableExtension
{
    uint8_t argTypes[CRP_MAX_ARGS];
    char *procInfo;
};

struct ProcInfo
{
    char *procName;
    uint8_t *argTypes;
    char *procInfo;
};

struct ProcTableEntry
{
    const char *procName;
    ProcPtr procPtr;
    ChirpProc chirpProc;
    const ProcTableExtension *extension;
};

class Chirp
{
public:
    Chirp(bool hinterested=false, bool client=false, Link *link=NULL);
    ~Chirp();

    virtual int init(bool connect);
    int setLink(Link *link);
    ChirpProc getProc(const char *procName, ProcPtr callback=0);
    int setProc(const char *procName, ProcPtr proc,  ProcTableExtension *extension=NULL);
    int getProcInfo(ChirpProc proc, ProcInfo *info);
    int registerModule(const ProcModule *module);

    int call(uint8_t service, ChirpProc proc, ...);
    static uint8_t getType(void *arg);
    int service(bool all=true);
    int assemble(uint8_t type, ...);
    bool connected();

    // utility methods
    static int serialize(Chirp *chirp, uint8_t *buf, uint32_t bufSize, ...);
    static int deserialize(uint8_t *buf, uint32_t len, ...);
    static int vserialize(Chirp *chirp, uint8_t *buf, uint32_t bufSize, va_list *args);
    static int vdeserialize(uint8_t *buf, uint32_t len, va_list *args);
    static int getArgList(uint8_t *buf, uint32_t len, uint8_t *argList);
    int useBuffer(uint8_t *buf, uint32_t len);

    static uint16_t calcCrc(uint8_t *buf, uint32_t len);

protected:
    int remoteInit(bool connect);
    int recvChirp(uint8_t *type, ChirpProc *proc, void *args[], bool wait=false); // null pointer terminates
    virtual int handleChirp(uint8_t type, ChirpProc proc, void *args[]); // null pointer terminates
    virtual void handleXdata(void *data[]) {}
    virtual int sendChirp(uint8_t type, ChirpProc proc);

    uint8_t *m_buf;
    uint8_t *m_bufSave;
    uint32_t m_len;
    uint32_t m_offset;
    uint32_t m_bufSize;
    bool m_errorCorrected;
    bool m_sharedMem;
    bool m_hinformer;
    bool m_hinterested;
    bool m_client;
    uint32_t m_headerLen;
    uint16_t m_headerTimeout;
    uint16_t m_dataTimeout;
    uint16_t m_idleTimeout;
    uint16_t m_sendTimeout;

private:
    int sendHeader(uint8_t type, ChirpProc proc);
    int sendFull(uint8_t type, ChirpProc proc);
    int sendData();
    int sendAck(bool ack); // false=nack
    int sendChirpRetry(uint8_t type, ChirpProc proc);
    int recvHeader(uint8_t *type, ChirpProc *proc, bool wait);
    int recvFull(uint8_t *type, ChirpProc *proc, bool wait);
    int recvData();
    int recvAck(bool *ack, uint16_t timeout); // false=nack
    int32_t handleEnumerate(char *procName, ChirpProc *callback);
    int32_t handleInit(uint16_t *blkSize, uint8_t *hintSource);
    int32_t handleEnumerateInfo(ChirpProc *proc);
    int vassemble(va_list *args);
    static int deserializeParse(uint8_t *buf, uint32_t len, void *args[]);
    static int loadArgs(va_list *args, void *recvArgs[]);
    void restoreBuffer();

    ChirpProc updateTable(const char *procName, ProcPtr procPtr);
    ChirpProc lookupTable(const char *procName);
    int realloc(uint32_t min=0);
    int reallocTable();

    Link *m_link;
    ProcTableEntry *m_procTable;
    uint16_t m_procTableSize;
    uint16_t m_blkSize;
    uint8_t m_maxNak;
    uint8_t m_retries;
    bool m_call;
    bool m_connected;
};

#endif // CHIRP_H
