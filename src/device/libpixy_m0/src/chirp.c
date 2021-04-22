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

#include <string.h>
#include "chirp.h"

// todo yield, sleep() while waiting for sync response
// todo

#define ALIGN(v, n)  v = v&((n)-1) ? (v&~((n)-1))+(n) : v

static ProcTableEntry *g_procTable;
static uint16_t g_procTableSize;
static uint16_t g_blkSize;
static bool g_connected;
static uint8_t *g_buf;
#ifndef CRP_ERROR_CORRECTED
static uint32_t g_offset;
#endif
static uint32_t g_len;
static uint32_t g_bufSize;
static bool g_remoteInit;
static bool g_hinformer;



#ifdef CRP_ERROR_CORRECTED
static int sendFull(uint8_t type, ChirpProc proc);
static int recvFull(uint8_t *type, ChirpProc *proc, bool wait);
#else
static int sendHeader(uint8_t type, ChirpProc proc);
static int sendData(void);
static int recvHeader(uint8_t *type, ChirpProc *proc, bool wait);
static int recvData(void);
static int sendAck(bool ack); // false=nack
static int recvAck(bool *ack, uint16_t timeout); // false=nack
static uint16_t calcCrc(uint8_t *buf, uint32_t len);
#endif
static int sendChirp(uint8_t type, ChirpProc proc);
static int sendChirpRetry(uint8_t type, ChirpProc proc);
static int recvChirp(uint8_t *type, ChirpProc *proc, void *args[], bool wait); // null pointer terminates
static int handleChirp(uint8_t type, ChirpProc proc, void *args[]); // null pointer terminates
static int32_t handleEnumerate(char *procName, ChirpProc *callback);
static int32_t handleInit(uint16_t *blkSize, uint8_t *hinformer);
static int assembleHelper(va_list *args);
static int loadArgs(va_list *args, void *recvArgs[]);
static ChirpProc updateTable(const char *procName, ProcPtr procPtr);
static ChirpProc lookupTable(const char *procName);
static int reallocate(uint32_t min);
static int reallocTable(void);


// weak definitions (need to change this keyword for different compiler)
// for gcc-- void __attribute__((weak)) foo()
uint32_t __attribute__((weak)) linkBlockSize()
{
    return CRP_BLK_SIZE;
}

uint32_t __attribute__((weak)) linkGetFlags(uint8_t index)
{
    return 0;
}

int32_t __attribute__((weak)) chirpInit(void)
{
    return 0;
}

// assume that destination is aligned on the correct boundary and copy the source byte by byte
void copyAlign(char *dest, const char *src, int size)
{
    int i;
    for (i=0; i<size; i++)
        dest[i] = src[i];
}

int chirpOpen()
{
    g_connected = FALSE;
    g_remoteInit = FALSE;
    g_hinformer = FALSE;

#ifndef CRP_SHARED_MEM
    g_bufSize = CRP_BUFSIZE;
    g_buf = malloc(g_bufSize);
#else
    g_bufSize = linkGetFlags(LINK_FLAG_INDEX_SHARED_MEMORY_SIZE);
    g_buf = (uint8_t *)linkGetFlags(LINK_FLAG_INDEX_SHARED_MEMORY_LOCATION);
#endif

    g_blkSize = linkBlockSize();
    g_procTableSize = CRP_PROCTABLE_LEN;
    g_procTable = malloc(sizeof(ProcTableEntry)*g_procTableSize);
    memset(g_procTable, 0, sizeof(ProcTableEntry)*g_procTableSize);

    return CRP_RES_OK;
}

int chirpClose()
{
#ifndef CRP_SHARED_MEM
    free(g_buf);
#endif
    free(g_procTable);
    return CRP_RES_OK;
}


int chirpAssemble(int dummy, ...)
{
    int res;
    va_list args;

    va_start(args, dummy);
    res = assembleHelper(&args);
    va_end(args);

    return res;
}

int assembleHelper(va_list *args)
{
    int res;
    uint8_t type, origType;
    uint32_t i, si;
    int8_t *ptr;

    for (i=CRP_HEADER_LEN+g_len; TRUE;)
    {
#if defined(__WIN32__) || defined(__arm__)
        type = va_arg(*args, int);
#else
        type = va_arg(*args, uint8_t);
#endif

        if (type==END)
            break;

        si = i; // save index so we can skip over data if needed
        g_buf[i++] = type;

        // treat hints like other types for now
        // but if gotoe isn't interested  in hints (m_hinformer=false),
        // we'll restore index to si and effectively skip data.
        origType = type;
        type &= ~CRP_HINT;

        if (type==CRP_INT8)
        {
#if defined(__WIN32__) || defined(__arm__)
            int8_t val = va_arg(*args, int);
#else
            int8_t val = va_arg(*args, int8_t);
#endif
            *(int8_t *)(g_buf+i) = val;
            i += 1;
        }
        else if (type==CRP_INT16)
        {
#if defined(__WIN32__) || defined(__arm__)
            int16_t val = va_arg(*args, int);
#else
            int16_t val = va_arg(*args, int);
#endif
            ALIGN(i, 2);
            // rewrite type so chirpGetType will work (even though we might add padding between type and data)
            g_buf[i-1] = origType;
            *(int16_t *)(g_buf+i) = val;
            i += 2;
        }
        else if (type==CRP_INT32 || origType==CRP_TYPE_HINT) // CRP_TYPE_HINT is a special case...
        {
            int32_t val = va_arg(*args, int32_t);
            ALIGN(i, 4);
            g_buf[i-1] = origType;
            *(int32_t *)(g_buf+i) = val;
            i += 4;
        }
        else if (type==CRP_FLT32)
        {
#if defined(__WIN32__) || defined(__arm__)
            float val = va_arg(*args, double);
#else
            float val = va_arg(*args, float);
#endif
            ALIGN(i, 4);
            g_buf[i-1] = origType;
            *(float *)(g_buf+i) = val;
            i += 4;
        }
        else if (type==CRP_STRING)
        {
            int8_t *s = va_arg(*args, int8_t *);
            uint32_t len = strlen((char *)s)+1; // include null

            if (len+i > g_bufSize-CRP_BUFPAD && (res=reallocate(len+i))<0)
                return res;

            memcpy(g_buf+i, s, len);
            i += len;
        }
        else if (type&CRP_ARRAY)
        {
            uint8_t size = type&0x0f;
            uint32_t len = va_arg(*args, int32_t);

            ALIGN(i, 4);
            g_buf[i-1] = origType;
            *(uint32_t *)(g_buf+i) = len;
            i += 4;
            ALIGN(i, size);
            len *= size; // scale by size of array elements

            if (len+i>g_bufSize-CRP_BUFPAD && (res=reallocate(len+i))<0)
                return res;

            ptr = va_arg(*args, int8_t *);
            memcpy(g_buf+i, ptr, len);
            i += len;
        }
        else
            return CRP_RES_ERROR_PARSE;

        // skip hint data if we're not a source
        if (!g_hinformer && origType&CRP_HINT)
            i = si;

        if (i>g_bufSize-CRP_BUFPAD && (res=reallocate(g_bufSize))<0)
            return res;
    }

    // set length
    g_len = i-CRP_HEADER_LEN;

    return CRP_RES_OK;
}

// this isn't completely necessary, but it makes things a lot easier to use.
// passing a pointer to a pointer and then having to dereference is just confusing....
// so for scalars (ints, floats) you don't need to pass in ** pointers, just * pointers so
// chirp can write the value into the * pointer and hand it back.
// But for arrays you need ** pointers, so chirp doesn't need to copy the whole array into your buffer---
// chirp will write the * pointer value into your ** pointer.
int loadArgs(va_list *args, void *recvArgs[])
{
    int i;
    uint8_t type, size;
    void **recvArg;

    for (i=0; recvArgs[i]!=NULL && i<CRP_MAX_ARGS; i++)
    {
        type = chirpGetType(recvArgs[i]);
        recvArg = va_arg(*args, void **);
        if (recvArg==NULL)
            return CRP_RES_ERROR_PARSE;

        if (!(type&CRP_ARRAY)) // if we're a scalar
        {
            size = type&0x0f;
            if (size==1) *(uint8_t *)recvArg = *(uint8_t *)recvArgs[i];
            else if (size==2) *(uint16_t *)recvArg = *(uint16_t *)recvArgs[i];
            else if (size==4) *(uint32_t *)recvArg = *(uint32_t *)recvArgs[i];
            //else if (size==8) *recvArg = *(double *)recvArgs[i];
            else return CRP_RES_ERROR_PARSE;
        }
        else // we're an array
        {
            if (type==CRP_STRING)
                *(char **)recvArg = (char *)recvArgs[i];
            else
            {
                *(uint32_t *)recvArg = *(uint32_t *)recvArgs[i++];
                recvArg = va_arg(*args, void **);
                if (recvArg==NULL)
                    return CRP_RES_ERROR_PARSE;
                *(void **)recvArg = recvArgs[i];
            }
        }
    }
    // check to see if last arg is NULL, if not, we have a parse error
    // if the arg isn't null, it means the caller is expecting data to be
    // put there.  If data isn't put there, and the caller dereferences, segfault
    if (va_arg(*args, void **)!=NULL)
        return CRP_RES_ERROR_PARSE;

    return CRP_RES_OK;
}

int chirpCall(uint8_t service, ChirpProc proc, ...)
{
    int res;
    uint8_t type;
    va_list args;

    // if it's just a regular chirpCall (not init or enumerate), we need to be connected
    if (!(service&CRP_CALL) && !g_connected)
        return CRP_RES_ERROR_NOT_CONNECTED;

    // parse args and assemble in g_buf
    va_start(args, proc);
    g_len = 0;
    if ((res=assembleHelper(&args))<0)
    {
        va_end(args);
        return res;
    }

    if (service&CRP_CALL) // special case for enumerate and init (internal calls)
    {
        type = service;
        service = SYNC;
    }
    else
        type = CRP_CALL;

    // linkSend chirpCall data
    if ((res=sendChirpRetry(type, proc))!=CRP_RES_OK) // convert chirpCall into response
    {
        va_end(args);
        return res;
    }

    // if the service is synchronous, linkReceive response while servicing other calls
    if (service==SYNC)
    {
        ChirpProc recvProc;
        void *recvArgs[CRP_MAX_ARGS+1];

        while(1)
        {
            if ((res=recvChirp(&type, &recvProc, recvArgs, TRUE))==CRP_RES_OK)
            {
                if (type&CRP_RESPONSE)
                    break;
                else // handle calls as they come in
                    handleChirp(type, recvProc, recvArgs);
            }
            else
            {
                va_end(args);
                return res;
            }
        }

        // load args
        if ((res=loadArgs(&args, recvArgs))<0)
        {
            va_end(args);
            return res;
        }
    }


    va_end(args);
    return CRP_RES_OK;
}

int sendChirpRetry(uint8_t type, ChirpProc proc)
{
    int i, res;

    for (i=0; i<CRP_RETRIES; i++)
    {
        res = sendChirp(type, proc);
        if (res==CRP_RES_OK)
            break;
    }

    // if sending the chirp fails after retries, we should assume we're no longer connected
    if (res<0)
        g_connected = FALSE;

    return res;
}

int sendChirp(uint8_t type, ChirpProc proc)
{
    int res;
#ifdef CRP_ERROR_CORRECTED
    res = sendFull(type, proc);
#else
    // we'll linkSend forever as long as we get naks
    // we rely on receiver to give up
    while((res=sendHeader(type, proc))==CRP_RES_ERROR_CRC);
    if (res!=CRP_RES_OK)
        return res;
    res = sendData();
#endif

    if (res!=CRP_RES_OK)
        return res;
    return CRP_RES_OK;
}

int handleChirp(uint8_t type, ChirpProc proc, void *args[])
{
    int res;
    uint32_t responseInt = 0;
    uint8_t n;
    ProcPtr ptr;

    // reset data in case there is a null response
    g_len = 4; // leave room for responseInt

    // check for intrinsic calls
    if (type&CRP_INTRINSIC)
    {
        if (type==CRP_CALL_ENUMERATE)
            responseInt = handleEnumerate((char *)args[0], (ChirpProc *)args[1]);
        else if (type==CRP_CALL_INIT)
            responseInt = handleInit((uint16_t *)args[0], (uint8_t *)args[1]);
        else
            return CRP_RES_ERROR;
    }
    else // normal chirpCall
    {
        if (proc>=g_procTableSize)
            return CRP_RES_ERROR; // index exceeded

        ptr = g_procTable[proc].procPtr;
        if (ptr==NULL)
            return CRP_RES_ERROR; // some chirps are not meant to be called in both directions

        // count args
        for (n=0; args[n]!=NULL; n++);

        if (n==0)
            responseInt = (*ptr)();
        else if (n==1)
            responseInt = (*(uint32_t(*)(void*))ptr)(args[0]);
        else if (n==2)
            responseInt = (*(uint32_t(*)(void*,void*))ptr)(args[0],args[1]);
        else if (n==3)
            responseInt = (*(uint32_t(*)(void*,void*,void*))ptr)(args[0],args[1],args[2]);
        else if (n==4)
            responseInt = (*(uint32_t(*)(void*,void*,void*,void*))ptr)(args[0],args[1],args[2],args[3]);
        else if (n==5)
            responseInt = (*(uint32_t(*)(void*,void*,void*,void*,void*))ptr)(args[0],args[1],args[2],args[3],args[4]);
        else if (n==6)
            responseInt = (*(uint32_t(*)(void*,void*,void*,void*,void*,void*))ptr)(args[0],args[1],args[2],args[3],args[4],args[5]);
        else if (n==7)
            responseInt = (*(uint32_t(*)(void*,void*,void*,void*,void*,void*,void*))ptr)(args[0],args[1],args[2],args[3],args[4],args[5],args[6]);
        else if (n==8)
            responseInt = (*(uint32_t(*)(void*,void*,void*,void*,void*,void*,void*,void*))ptr)(args[0],args[1],args[2],args[3],args[4],args[5],args[6],args[7]);
        else if (n==9)
            responseInt = (*(uint32_t(*)(void*,void*,void*,void*,void*,void*,void*,void*,void*))ptr)(args[0],args[1],args[2],args[3],args[4],args[5],args[6],args[7],args[8]);
        else if (n==10)
            responseInt = (*(uint32_t(*)(void*,void*,void*,void*,void*,void*,void*,void*,void*,void *))ptr)(args[0],args[1],args[2],args[3],args[4],args[5],args[6],args[7],args[8],args[9]);
    }

    // if it's a chirp chirpCall, we need to linkSend back the result
    // result is in g_buf
    if (type&CRP_CALL)
    {
        // write responseInt
        *(uint32_t *)(g_buf+CRP_HEADER_LEN) = responseInt;
        // send response
        if ((res=sendChirpRetry(CRP_RESPONSE | (type&~CRP_CALL), g_procTable[proc].chirpProc))!=CRP_RES_OK) // convert chirpCall into response
            return res;
    }

    return CRP_RES_OK;
}

int reallocTable(void)
{
    ProcTableEntry *newProcTable;
    int newProcTableSize;

    // allocate new table, zero
    newProcTableSize = g_procTableSize+CRP_PROCTABLE_LEN;
    newProcTable = (ProcTableEntry *)malloc(sizeof(ProcTableEntry)*newProcTableSize);
    memset(newProcTable, 0, sizeof(ProcTableEntry)*newProcTableSize);
    // copy to new table
    memcpy(newProcTable, g_procTable, sizeof(ProcTableEntry)*g_procTableSize);
    // delete old table
    free(g_procTable);
    // set to new
    g_procTable = newProcTable;
    g_procTableSize = newProcTableSize;

    return CRP_RES_OK;
}

ChirpProc lookupTable(const char *procName)
{
    ChirpProc i;

    for(i=0; i<g_procTableSize; i++)
    {
        if (g_procTable[i].procName!=NULL && strcmp(g_procTable[i].procName, procName)==0)
            return i;
    }
    return -1;
}


ChirpProc updateTable(const char *procName, ProcPtr procPtr)
{
    ChirpProc proc;
    // if it exists already, update,
    // if it doesn't exist, add it
    if (procName==NULL)
        return -1;

    proc = lookupTable(procName);
    if (proc<0) // next empty entry
    {
        for (proc=0; proc<g_procTableSize && g_procTable[proc].procName; proc++);
        if (proc==g_procTableSize)
        {
            reallocTable();
            return updateTable(procName, procPtr);
        }
    }

    // add to table
    g_procTable[proc].procName = procName;
    g_procTable[proc].procPtr = procPtr;

    return proc;
}

ChirpProc chirpGetProc(const char *procName, ProcPtr callback)
{
    uint32_t res;
    ChirpProc cproc = -1;

    if (callback)
        cproc = updateTable(procName, callback);

    if (chirpCall(CRP_CALL_ENUMERATE, 0,
                  STRING(procName), // linkSend name
                  INT16(cproc), // linkSend local index
                  END_SEND_ARGS,
                  &res, // get remote index
                  END_RECV_ARGS
                  )>=0)
        return res;

    // a negative ChirpProc is an error
    return -1;
}

int chirpRemoteInit()
{
    int res;
    uint32_t responseInt;
    uint8_t hinformer;

    if (g_remoteInit)
        return CRP_RES_OK;

    res = chirpCall(CRP_CALL_INIT, 0,
                    INT16(g_blkSize), // linkSend block size
                    UINT8(0),         // send whether we're interested in hints or not (we're not)
                    END_SEND_ARGS,
                    &responseInt,
                    &hinformer,       // receive whether we should send hints
                    END_RECV_ARGS
                    );
    if (res>=0)
    {
        g_connected = TRUE;
        g_hinformer = hinformer;
        return responseInt;
    }
    return res;
}

int chirpSetProc(const char *procName, ProcPtr proc)
{
    if (updateTable(procName, proc)<0)
        return CRP_RES_ERROR;
    return CRP_RES_OK;
}

int32_t handleEnumerate(char *procName, ChirpProc *callback)
{
    ChirpProc proc;
    // lookup in table
    proc = lookupTable(procName);
    // set remote index in table
    g_procTable[proc].chirpProc = *callback;

    return proc;
}

int32_t handleInit(uint16_t *blkSize, uint8_t *hinformer)
{
    int32_t responseInt;

    g_remoteInit = TRUE;
    responseInt = chirpInit();
    g_remoteInit = FALSE;
    g_connected = TRUE;
    g_blkSize = *blkSize;  // get block size, write it
    g_hinformer = *hinformer;

    CRP_RETURN(UINT8(0), END);

    return responseInt;

}

int reallocate(uint32_t min)
{
#ifdef CRP_SHARED_MEM
    return CRP_RES_ERROR_MEMORY;
#else
    uint8_t *newbuf;

    min += CRP_BUFSIZE;
    newbuf = malloc(min);
    memcpy(newbuf, g_buf, g_bufSize);
    free(g_buf);
    g_buf = newbuf;
    g_bufSize = min;

    return CRP_RES_OK;
#endif
}

// service deals with calls and callbacks
int chirpService()
{
    int i = 0;
    uint8_t type;
    ChirpProc recvProc;
    void *args[CRP_MAX_ARGS+1];

    while(recvChirp(&type, &recvProc, args, FALSE)==CRP_RES_OK)
    {
        handleChirp(type, recvProc, args);
        i++;
    }

    return i;
}

int recvChirp(uint8_t *type, ChirpProc *proc, void *args[], bool wait) // null pointer terminates
{
    int res;
    uint8_t dataType, size, a;
    uint32_t i, len;

    // linkReceive
#ifdef CRP_ERROR_CORRECTED
    res = recvFull(type, proc, wait);
#else
    for (i=0; TRUE; i++)
    {
        res = recvHeader(type, proc, wait);
        if (res==CRP_RES_ERROR_CRC)
        {
            if (i<CRP_MAX_NAK)
                continue;
            else
                return CRP_RES_ERROR_MAX_NAK;
        }
        else if (res==CRP_RES_OK)
            break;
        else
            return res;
    }
    res = recvData();
#endif
    if (res!=CRP_RES_OK)
        return res;

    // get responseInt from response
    if (*type&CRP_RESPONSE)
    {
        // add responseInt to arg list
        args[0] = (void *)(g_buf+CRP_HEADER_LEN);
        *(g_buf+CRP_HEADER_LEN-1) = CRP_UINT32; // write type so it parses correctly
        // increment pointer
        i = CRP_HEADER_LEN+4;
        a = 1;
    }
    else // call has no responseInt
    {
        i = CRP_HEADER_LEN;
        a = 0;
    }
    // parse remaining args
    for(; i<g_len+CRP_HEADER_LEN; a++)
    {
        if (a==CRP_MAX_ARGS)
            return CRP_RES_ERROR;

        dataType = g_buf[i++];
        size = dataType&0x0f;
        if (!(dataType&CRP_ARRAY)) // if we're a scalar
        {
            ALIGN(i, size);
            args[a] = (void *)(g_buf+i);
            i += dataType&0x0f; // extract size of scalar, add it
        }
        else // we're an array
        {
            if (dataType==CRP_STRING) // string is a special case
            {
                args[a] = (void *)(g_buf+i);
                i += strlen((char *)(g_buf+i))+1; // +1 include null character
            }
            else
            {
                ALIGN(i, 4);
                len = *(uint32_t *)(g_buf+i);
                args[a++] = (void *)(g_buf+i);
                i += 4;
                ALIGN(i, size);
                args[a] = (void *)(g_buf+i);
                i += len*size;
            }
        }
    }
    args[a] = NULL; // terminate list

    return CRP_RES_OK;
}

uint8_t chirpGetType(void *arg)
{
    return *((uint8_t *)arg - 1);
}

uint16_t calcCrc(uint8_t *buf, uint32_t len)
{
    uint32_t i;
    uint16_t crc;

    // this isn't a real crc, but it's cheap and prob good enough
    for (i=0, crc=0; i<len; i++)
        crc += buf[i];
    crc += len;

    return crc;
}

#ifdef CRP_ERROR_CORRECTED
int sendFull(uint8_t type, ChirpProc proc)
{
    int res;

    *(uint32_t *)g_buf = CRP_START_CODE;
    *(uint8_t *)(g_buf+4) = type;
    *(ChirpProc *)(g_buf+6) = proc;
    *(uint32_t *)(g_buf+8) = g_len;
    // linkSend header
    if ((res=linkSend(g_buf, CRP_MAX_HEADER_LEN, CRP_SEND_TIMEOUT))<0)
        return res;

#ifndef CRP_SHARED_MEM
    // if we haven't sent everything yet....
    if (g_len+CRP_HEADER_LEN>CRP_MAX_HEADER_LEN)
    {
        if ((res=linkSend(g_buf+CRP_MAX_HEADER_LEN, g_len-(CRP_MAX_HEADER_LEN-CRP_HEADER_LEN), CRP_SEND_TIMEOUT))<0)
            return res;
    }
#endif

    return CRP_RES_OK;
}
#endif

#ifndef CRP_ERROR_CORRECTED
int sendHeader(uint8_t type, ChirpProc proc)
{
    int res;
    bool ack;
    uint32_t chunk, startCode = CRP_START_CODE;
    uint16_t crc;

    if ((res=linkSend((uint8_t *)&startCode, 4, CRP_SEND_TIMEOUT))<0)
        return res;

    *(uint8_t *)g_buf = type;
    *(uint16_t *)(g_buf+2) = proc;
    *(uint32_t *)(g_buf+4) = g_len;
    if ((res=linkSend(g_buf, CRP_HEADER_LEN, CRP_SEND_TIMEOUT))<0)
        return res;
    crc = calcCrc(g_buf, CRP_HEADER_LEN);

    if (g_len>=CRP_MAX_HEADER_LEN)
        chunk = CRP_MAX_HEADER_LEN;
    else
        chunk = g_len;
    if (linkSend(g_buf, chunk, CRP_SEND_TIMEOUT)<0)
        return CRP_RES_ERROR_SEND_TIMEOUT;

    // send crc
    crc += calcCrc(g_buf, chunk);
    if (linkSend((uint8_t *)&crc, 2, CRP_SEND_TIMEOUT)<0)
        return CRP_RES_ERROR_SEND_TIMEOUT;

    if ((res=recvAck(&ack, CRP_HEADER_TIMEOUT))<0)
        return res;

    if (ack)
        g_offset = chunk;
    else
        return CRP_RES_ERROR_CRC;

    return CRP_RES_OK;
}
#endif

#ifndef CRP_ERROR_CORRECTED
int sendData()
{
    uint16_t crc;
    uint32_t chunk;
    uint8_t sequence;
    bool ack;
    int res;

    for (sequence=0; g_offset<g_len; )
    {
        if (g_len-g_offset>=g_blkSize)
            chunk = g_blkSize;
        else
            chunk = g_len-g_offset;
        // send data
        if (linkSend(g_buf+g_offset, chunk, CRP_SEND_TIMEOUT)<0)
            return CRP_RES_ERROR_SEND_TIMEOUT;
        // send sequence
        if (linkSend((uint8_t *)&sequence, 1, CRP_SEND_TIMEOUT)<0)
            return CRP_RES_ERROR_SEND_TIMEOUT;
        // send crc
        crc = calcCrc(g_buf+g_offset, chunk) + calcCrc((uint8_t *)&sequence, 1);
        if (linkSend((uint8_t *)&crc, 2, CRP_SEND_TIMEOUT)<0)
            return CRP_RES_ERROR_SEND_TIMEOUT;

        if ((res=recvAck(&ack, CRP_DATA_TIMEOUT))<0)
            return res;
        if (ack)
        {
            g_offset += chunk;
            sequence++;
        }
    }
    return CRP_RES_OK;
}
#endif

#ifndef CRP_ERROR_CORRECTED
int sendAck(bool ack) // FALSE=nack
{
    uint8_t c;

    if (ack)
        c = CRP_ACK;
    else
        c = CRP_NACK;

    if (linkSend(&c, 1, CRP_SEND_TIMEOUT)<0)
        return CRP_RES_ERROR_SEND_TIMEOUT;

    return CRP_RES_OK;
}
#endif

#ifndef CRP_ERROR_CORRECTED
int recvHeader(uint8_t *type, ChirpProc *proc, bool wait)
{
    int res;
    uint8_t c;
    uint32_t chunk, startCode = 0;
    uint16_t crc, rcrc;

    if ((res=linkReceive(&c, 1, wait?CRP_HEADER_TIMEOUT:0))<0)
        return res;
    if (res<1)
        return CRP_RES_ERROR;

    // find start code
    while(1)
    {
        startCode >>= 8;
        startCode |= (uint32_t)c<<24;
        if (startCode==CRP_START_CODE)
            break;
        if ((res=linkReceive(&c, 1, CRP_IDLE_TIMEOUT))<0)
            return res;
        if (res<1)
            return CRP_RES_ERROR;
    }
    // receive rest of header
    if (linkReceive(g_buf, CRP_HEADER_LEN, CRP_IDLE_TIMEOUT)<0)
        return CRP_RES_ERROR_RECV_TIMEOUT;
    if (res<(int)CRP_HEADER_LEN)
        return CRP_RES_ERROR;
    *type = *(uint8_t *)g_buf;
    *proc = *(ChirpProc *)(g_buf+2);
    g_len = *(uint32_t *)(g_buf+4);
    crc = calcCrc(g_buf, CRP_HEADER_LEN);

    if (g_len>=CRP_MAX_HEADER_LEN-CRP_HEADER_LEN)
        chunk = CRP_MAX_HEADER_LEN-CRP_HEADER_LEN;
    else
        chunk = g_len;
    if ((res=linkReceive(g_buf, chunk+2, CRP_IDLE_TIMEOUT))<0) // +2 for crc
        return res;
    if (res<(int)chunk+2)
        return CRP_RES_ERROR;
    copyAlign((char *)&rcrc, (char *)(g_buf+chunk), 2);
    if (rcrc==crc+calcCrc(g_buf, chunk))
    {
        g_offset = chunk;
        sendAck(TRUE);
    }
    else
    {
        sendAck(FALSE); // linkSend nack
        return CRP_RES_ERROR_CRC;
    }

    return CRP_RES_OK;
}
#endif

#ifdef CRP_ERROR_CORRECTED
int recvFull(uint8_t *type, ChirpProc *proc, bool wait)
{
    int res;
    uint32_t startCode;

    // receive header, with startcode check to make sure we're synced
    while(1)
    {
        if ((res=linkReceive(g_buf, CRP_MAX_HEADER_LEN, wait?CRP_HEADER_TIMEOUT:0))<0)
            return res;
        // check to see if we received less data than expected
        if (res<CRP_MAX_HEADER_LEN)
            return CRP_RES_ERROR;

        startCode = *(uint32_t *)g_buf;
        if (startCode==CRP_START_CODE)
            break;
    }
    *type = *(uint8_t *)(g_buf+4);
    *proc = *(ChirpProc *)(g_buf+6);
    g_len = *(uint32_t *)(g_buf+8);

#ifndef CRP_SHARED_MEM
    if (g_len+CRP_HEADER_LEN>g_bufSize && (res=reallocate(g_len+CRP_HEADER_LEN))<0)
        return res;

    if (g_len+CRP_HEADER_LEN>CRP_MAX_HEADER_LEN)
    {
        if ((res=linkReceive(g_buf+CRP_MAX_HEADER_LEN, g_len-(CRP_MAX_HEADER_LEN-CRP_HEADER_LEN), CRP_IDLE_TIMEOUT))<0)
            return res;
        // check to see if we received less data than expected
        if (res<(int)g_len-(CRP_MAX_HEADER_LEN-(int)CRP_HEADER_LEN))
            return CRP_RES_ERROR;
    }
#endif

    return CRP_RES_OK;
}
#endif

// We assume that the probability that we linkSend a nack and the receiver interprets a nack is 100%
// We can't assume that the probability that we linkSend an ack and the receiver interprets it is 100%
// Scenario
// 1) we receive packet 0, redo is 0, crc is good, we increment offset, linkSend ack. (inc) (inc) rs=0, s=0, inc
// 2) we receive packet 1, crc is bad, we linkSend nack (!inc) (!inc) rs=1, s=1
// 3) sender gets nack, so it resends
// 4) we receive packet 1, redo is 1, crc is good, we don't increment offset, linkSend ack (inc) (inc) rs=1, s=1
// 5) we receive packet 2, redo is 0, crc is bad, we linkSend nack (!inc) (!inc) rs=2, s=2
// 6) we receive packet 2, redo is 1, crc is good, we linkSend ack (inc) (inc) rs=2, s=2
// 7) we receive packet 3, redo is 0, crc is good, we linkSend ack (inc) (inc)
// different scenario
// 1) we receive packet 0, redo is 0, crc is good, we increment offset, linkSend ack. (inc) (inc) rs=0, s=0
// 2) sender thinks it gets a nack, so it resends
// 3) we receive packet 0 again, but crc is bad, we linkSend nack (!inc) (!inc) rs=1, s=0
// 4) sender gets nack, so it resends
// 5) we receive packet 0, redo is 1, crc is good, we don't increment offset, linkSend ack (!inc) (inc) rs=1, s=0
// (we've essentially thrown out this packet, but that's ok, because we have a good packet 0)
// 6) we receive packet 1, redo is 0, crc is bad, we linkSend nack (!inc) (!inc) rs=1, s=1
// 7) we receive packet 1, redo is 1, crc is good, we linkSend ack (inc) (inc) rs=1, s=1
// 8) we receive packet 2, redo is 0, crc is good, we linkSend ack (inc) (inc) rs=2, s=2
// a redo flag is not sufficient to communicate which packet we're on because the sender can misinterpret
// any number of nacks
#ifndef CRP_ERROR_CORRECTED
int recvData()
{
    int res;
    uint32_t chunk;
    uint16_t crc;
    uint8_t sequence, rsequence, naks;

    if (g_len+3+CRP_HEADER_LEN>g_bufSize && (res=reallocate(g_len+3+CRP_HEADER_LEN))<0) // +3 to read sequence, crc
        return res;

    for (rsequence=0, naks=0; g_offset<g_len; )
    {
        if (g_len-g_offset>=g_blkSize)
            chunk = g_blkSize;
        else
            chunk = g_len-g_offset;
        if (linkReceive(g_buf+g_offset, chunk+3, CRP_DATA_TIMEOUT)<0) // +3 to read sequence, crc
            return CRP_RES_ERROR_RECV_TIMEOUT;
        if (res<(int)chunk+3)
            return CRP_RES_ERROR;
        sequence = *(uint8_t *)(g_buf+g_offset+chunk);
        copyAlign((char *)&crc, (char *)(g_buf+g_offset+chunk+1), 2);
        if (crc==calcCrc(g_buf+g_offset, chunk+1))
        {
            if (rsequence==sequence)
            {
                g_offset += chunk;
                rsequence++;
            }
            sendAck(TRUE);
            naks = 0;
        }
        else
        {
            sendAck(FALSE);
            naks++;
            if (naks<CRP_MAX_NAK)
                naks++;
            else
                return CRP_RES_ERROR_MAX_NAK;
        }
    }
    return CRP_RES_OK;
}
#endif

#ifndef CRP_ERROR_CORRECTED
int recvAck(bool *ack, uint16_t timeout) // FALSE=nack
{
    int res;
    uint8_t c;
    if ((res=linkReceive(&c, 1, timeout))<0)
        return CRP_RES_ERROR_RECV_TIMEOUT;
    if (res<1)
        return CRP_RES_ERROR;

    if (c==CRP_ACK)
        *ack = TRUE;
    else
        *ack = FALSE;

    return CRP_RES_OK;
}
#endif
