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
#include <new>
#include "chirp.hpp"
#include "debuglog.h"


// todo yield, sleep() while waiting for sync response
// todo

// assume that destination is aligned on the correct boundary and copy the source byte by byte
void copyAlign(char *dest, const char *src, int size)
{
    int i;
    for (i=0; i<size; i++)
        dest[i] = src[i];
}

Chirp::Chirp(bool hinterested, bool client, Link *link)
{
  log("pixydebug: Chirp::Chirp()\n");
    m_link = NULL;
    m_errorCorrected = false;
    m_sharedMem = false;
    m_buf = NULL;
    m_bufSave = NULL;

    m_maxNak = CRP_MAX_NAK;
    m_retries = CRP_RETRIES;
    m_headerTimeout = CRP_HEADER_TIMEOUT;
    m_dataTimeout = CRP_DATA_TIMEOUT;
    m_idleTimeout = CRP_IDLE_TIMEOUT;
    m_sendTimeout = CRP_SEND_TIMEOUT;
    m_call = false;
    m_connected = false;
    m_hinformer = false;
    m_hinterested = hinterested;
    m_client = client;

    m_procTableSize = CRP_PROCTABLE_LEN;
    m_procTable = new (std::nothrow) ProcTableEntry[m_procTableSize];
    memset(m_procTable, 0, sizeof(ProcTableEntry)*m_procTableSize);

    if (link)
        setLink(link);
  log("pixydebug: Chirp::Chirp() returned\n");
}

Chirp::~Chirp()
{
  log("pixydebug: Chirp::~Chirp()\n");
    // if we're a client, disconnect (let server know)
    if (m_client)
        remoteInit(false);
    if (!m_sharedMem)
    {
        restoreBuffer();
        delete[] m_buf;
    }
    delete[] m_procTable;
  log("pixydebug: Chirp::~Chirp() returned\n");
}

int Chirp::init(bool connect)
{
    return CRP_RES_OK;
}

int Chirp::setLink(Link *link)
{
  int return_value;

  log("pixydebug: Chirp::setLink()\n");
    m_link = link;
    m_errorCorrected = m_link->getFlags()&LINK_FLAG_ERROR_CORRECTED;
    m_sharedMem = m_link->getFlags()&LINK_FLAG_SHARED_MEM;
    m_blkSize = m_link->blockSize();

    if (m_errorCorrected)
        m_headerLen = 12; // startcode (uint32_t), type (uint8_t), (pad), proc (uint16_t), len (uint32_t)
    else
        m_headerLen = 8;  // type (uint8_t), (pad), proc (uint16_t), len (uint32_t)

    if (m_sharedMem)
    {
        m_buf = (uint8_t *)m_link->getFlags(LINK_FLAG_INDEX_SHARED_MEMORY_LOCATION);
        m_bufSize = m_link->getFlags(LINK_FLAG_INDEX_SHARED_MEMORY_SIZE);
    }
    else
    {
        m_bufSize = CRP_BUFSIZE;
        m_buf = new (std::nothrow) uint8_t[m_bufSize];
    }

    // link is set up, need to call init
    if (m_client) {
      return_value = remoteInit(true);
      log("pixydebug:  remoteInit() = %d\n", return_value);
      log("pixydebug: setLink() returned %d\n", return_value);
      return return_value;
    }

    log("pixydebug: setLink() returned %d\n", CRP_RES_OK);
    return CRP_RES_OK;
}


int Chirp::assemble(uint8_t type, ...)
{
    int res;
    va_list args;
    bool save = m_call;
    uint32_t saveLen = m_len;

    if (type==CRP_XDATA)
        m_call = false;

    va_start(args, type);
    res = vassemble(&args);
    va_end(args);

    if (type==CRP_XDATA || (!m_call && res==CRP_RES_OK)) // if we're not a call, we're extra data, so we need to send
    {
        res = sendChirpRetry(CRP_XDATA, 0);
        m_len = saveLen;
    }

    m_call = save;

    return res;
}

int Chirp::vassemble(va_list *args)
{
    int len;

    len = vserialize(this, m_buf, m_bufSize, args);
    // check for error
    if (len<0)
        return len;

    // set length (don't include header)
    m_len = len - m_headerLen;

    return CRP_RES_OK;
}

bool Chirp::connected()
{
    return m_connected;
}

int Chirp::useBuffer(uint8_t *buf, uint32_t len)
{
    int res;

    if (m_bufSave==NULL)
    {
        m_bufSave = m_buf;
        m_buf = buf;
    }
    else if (buf!=m_buf)
        return CRP_RES_ERROR_MEMORY;

    m_len = len-m_headerLen;
    if (!m_call) // if we're not a call, we're extra data, so we need to send
    {
        res = sendChirpRetry(CRP_XDATA, 0);
        restoreBuffer(); // restore buffer immediately!
        if (res!=CRP_RES_OK) // convert call into response
            return res;
    }
    return CRP_RES_OK;
}

void Chirp::restoreBuffer()
{
    if (m_bufSave)
    {
        m_buf = m_bufSave;
        m_bufSave = NULL;
    }
}


int Chirp::serialize(Chirp *chirp, uint8_t *buf, uint32_t bufSize, ...)
{
    int res;
    va_list args;

    va_start(args, bufSize);
    res = vserialize(chirp, buf, bufSize, &args);
    va_end(args);

    return res;
}

#define RESIZE_BUF(size) \
    if (size > bufSize-CRP_BUFPAD) \
{ \
    if (!chirp) \
    return CRP_RES_ERROR_MEMORY; \
    else \
{ \
    if ((res=chirp->realloc(size))<0) \
    return res; \
    buf = chirp->m_buf; \
    bufSize = chirp->m_bufSize; \
    } \
    } \


int Chirp::vserialize(Chirp *chirp, uint8_t *buf, uint32_t bufSize, va_list *args)
{
    int res;
    uint8_t type, origType;
    uint32_t i, si;
    bool copy = true;

    if (chirp)
    {
        if (chirp->m_call) // reserve an extra 4 for responseint
            i = chirp->m_headerLen+4;
        else // if it's a chirp call, just reserve the header
            i = chirp->m_headerLen;
    }
    else
        i = 0;

    bufSize -= i;

    while(1)
    {
#if 1
        type = va_arg(*args, int);
#else
        type = va_arg(*args, uint8_t);
#endif

        if (type==END)
            break;

        si = i; // save index so we can skip over data if needed
        buf[i++] = type;

        // treat hints like other types for now
        // but if gotoe (guy on the other end) isn't interested in hints (m_hinformer=false),
        // we'll restore index to si and effectively skip data.
        origType = type;
        type &= ~CRP_HINT;

        if (type==CRP_INT8)
        {
#if 1
            int8_t val = va_arg(*args, int);
#else
            int8_t val = va_arg(*args, int8_t);
#endif
            *(int8_t *)(buf+i) = val;
            i += 1;
        }
        else if (type==CRP_INT16)
        {
#if 1
            int16_t val = va_arg(*args, int);
#else
            int16_t val = va_arg(*args, int16_t);
#endif
            ALIGN(i, 2);
            // rewrite type so getType will work (even though we might add padding between type and data)
            buf[i-1] = origType;
            *(int16_t *)(buf+i) = val;
            i += 2;
        }
        else if (type==CRP_INT32 || origType==CRP_TYPE_HINT) // CRP_TYPE_HINT is a special case...
        {
            int32_t val = va_arg(*args, int32_t);
            ALIGN(i, 4);
            buf[i-1] = origType;
            *(int32_t *)(buf+i) = val;
            i += 4;
        }
        else if (type==CRP_FLT32)
        {
#if 1
            float val = va_arg(*args, double);
#else
            float val = va_arg(*args, float);
#endif
            ALIGN(i, 4);
            buf[i-1] = origType;
            *(float *)(buf+i) = val;
            i += 4;
        }
        else if (type==CRP_STRING)
        {
            int8_t *s = va_arg(*args, int8_t *);
            uint32_t len = strlen((char *)s)+1; // include null

            RESIZE_BUF(len+i);

            memcpy(buf+i, s, len);
            i += len;
        }
        else if (type&CRP_ARRAY)
        {
            uint8_t size = type&0x0f;
            uint32_t len = va_arg(*args, int32_t);

            // deal with no copy case (use our own buffer)
            if ((type&CRP_NO_COPY)==CRP_NO_COPY)
            {
                // rewrite type so as not to confuse gotoe
                origType = type &= ~CRP_NO_COPY;
                buf[i-1] = origType;
                copy = false;
            }

            ALIGN(i, 4);
            buf[i-1] = origType;
            *(uint32_t *)(buf+i) = len;
            i += 4;
            ALIGN(i, size);

            if (copy)
            {
                len *= size; // scale by size of array elements

                RESIZE_BUF(len+i);

                int8_t *ptr = va_arg(*args, int8_t *);
                memcpy(buf+i, ptr, len);
                i += len;
            }
        }
        else
            return CRP_RES_ERROR_PARSE;

        // skip hint data if we're not a source
        if (chirp && !chirp->m_hinformer && origType&CRP_HINT)
            i = si;

        RESIZE_BUF(i);
    }

    // return length
    return i;
}


// this isn't completely necessary, but it makes things a lot easier to use.
// passing a pointer to a pointer and then having to dereference is just confusing....
// so for scalars (ints, floats) you don't need to pass in ** pointers, just * pointers so
// chirp can write the value into the * pointer and hand it back.
// But for arrays you need ** pointers, so chirp doesn't need to copy the whole array into your buffer---
// chirp will write the * pointer value into your ** pointer.
int Chirp::loadArgs(va_list *args, void *recvArgs[])
{
    int i;
    uint8_t type, size;
    void **recvArg;

    for (i=0; recvArgs[i]!=NULL && i<CRP_MAX_ARGS; i++)
    {
        type = getType(recvArgs[i]);
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

int Chirp::call(uint8_t service, ChirpProc proc, va_list args)
{
    int res, i;
    uint8_t type;
    va_list arguments;

    va_copy(arguments, args);

    // if it's just a regular call (not init or enumerate), we need to be connected
    if (!(service&CRP_CALL) && !m_connected)
        return CRP_RES_ERROR_NOT_CONNECTED;

    // parse arguments and assemble in m_buf
    m_len = 0;
    // restore buffer in case it was changed
    restoreBuffer();
    if ((res=vassemble(&arguments))<0)
    {
        va_end(arguments);
        return res;
    }

    if (service&CRP_CALL) // special case for enumerate and init (internal calls)
    {
        type = service;
        service = SYNC;
    }
    else
        type = CRP_CALL;

    // send call data
    if ((res=sendChirpRetry(type, proc))!=CRP_RES_OK) // convert call into response
    {
        va_end(arguments);
        return res;
    }


    // if the service is synchronous, receive response while servicing other calls
    if (!(service&ASYNC))
    {
        ChirpProc recvProc;
        void *recvArgs[CRP_MAX_ARGS+1];
        m_link->setTimer(); // set timer, so we can check to see if we're taking too much time

        while(1)
        {
            if ((res=recvChirp(&type, &recvProc, recvArgs, true))==CRP_RES_OK)
            {
                if (type&CRP_RESPONSE)
                    break;
                else // handle calls as they come in
                    handleChirp(type, recvProc, (const void **)recvArgs);
            }
            else
            {
                va_end(arguments);
                return res;
            }
            if (m_link->getTimer()>m_headerTimeout) // we could receive XDATA (for example) and never exit this while loop
                return CRP_RES_ERROR_RECV_TIMEOUT;
        }

        // deal with arguments
        if (service&RETURN_ARRAY) // copy array of arguments
        {
            void **recvArray;
            while(1)
            {
                recvArray = va_arg(arguments, void **);
                if (recvArray!=NULL)
                    break;
            }
            for (i=0; recvArgs[i]; i++)
                recvArray[i] = recvArgs[i];
            recvArray[i] = NULL;
        }
        else if ((res=loadArgs(&arguments, recvArgs))<0)
        {
            va_end(arguments);
            return res;
        }
    }


    va_end(arguments);
    return CRP_RES_OK;
}

int Chirp::call(uint8_t service, ChirpProc proc, ...)
{
  int result; 
  
  va_list arguments;

  va_start(arguments, proc);
  result = call(service, proc, arguments);
  va_end(arguments);

  return result;
}

int Chirp::sendChirpRetry(uint8_t type, ChirpProc proc)
{
    int i, res=-1;

    if (!m_connected && !(type&CRP_INTRINSIC))
        return CRP_RES_ERROR_NOT_CONNECTED;
    // deal with case where there is no actual data (e.g. it's all hint data and gotoe isn't hinterested)
    // but chirp calls can have no data of course
    if (m_len==0 && !(type&CRP_CALL))
        return CRP_RES_OK;
    for (i=0; i<m_retries; i++)
    {
        res = sendChirp(type, proc);
        if (res==CRP_RES_OK)
            break;
    }

    // if sending the chirp fails after retries, we should assume we're no longer connected
    if (res<0)
        m_connected = false;

    return res;
}

int Chirp::sendChirp(uint8_t type, ChirpProc proc)
{
    int res;
    if (m_errorCorrected)
        res = sendFull(type, proc);
    else
    {
        // we'll send forever as long as we get naks
        // we rely on receiver to give up
        while((res=sendHeader(type, proc))==CRP_RES_ERROR_CRC);
        if (res!=CRP_RES_OK)
            return res;
        res = sendData();
    }
    if (res!=CRP_RES_OK)
        return res;
    return CRP_RES_OK;
}

int Chirp::handleChirp(uint8_t type, ChirpProc proc, const void *args[])
{
    int res;
    int32_t responseInt = 0;
    uint8_t n;

    // default case, we return one integer (responseint)
    m_len = 4;

    // check for intrinsic calls
    if (type&CRP_INTRINSIC)
    {
        m_call = true; // indicate to ourselves that this is a chirp call
        if (type==CRP_CALL_ENUMERATE)
            responseInt = handleEnumerate((char *)args[0], (ChirpProc *)args[1]);
        else if (type==CRP_CALL_INIT)
            responseInt = handleInit((uint16_t *)args[0], (uint8_t *)args[1]);
        else if (type==CRP_CALL_ENUMERATE_INFO)
            responseInt = handleEnumerateInfo((ChirpProc *)args[0]);
        else
            responseInt = CRP_RES_ERROR;
        m_call = false;
    }
    else if (type==CRP_XDATA)
    {
        handleXdata(args);
        return CRP_RES_OK;
    }
    else // normal call
    {
        if (proc>=m_procTableSize || proc<0)
            return CRP_RES_ERROR; // index exceeded

        ProcPtr ptr = m_procTable[proc].procPtr;
        if (ptr==NULL)
            return CRP_RES_ERROR; // some chirps are not meant to be called in both directions

        // count args
        for (n=0; args[n]!=NULL; n++);

        m_call = true; // indicate to ourselves that this is a chirp call
        // this is probably overkill....
        if (n==0)
            responseInt = (*ptr)(this);
        else if (n==1)
            responseInt = (*(uint32_t(*)(const void*,Chirp*))ptr)(args[0],this);
        else if (n==2)
            responseInt = (*(uint32_t(*)(const void*,const void*,Chirp*))ptr)(args[0],args[1],this);
        else if (n==3)
            responseInt = (*(uint32_t(*)(const void*,const void*,const void*,Chirp*))ptr)(args[0],args[1],args[2],this);
        else if (n==4)
            responseInt = (*(uint32_t(*)(const void*,const void*,const void*,const void*,Chirp*))ptr)(args[0],args[1],args[2],args[3],this);
        else if (n==5)
            responseInt = (*(uint32_t(*)(const void*,const void*,const void*,const void*,const void*,Chirp*))ptr)(args[0],args[1],args[2],args[3],args[4],this);
        else if (n==6)
            responseInt = (*(uint32_t(*)(const void*,const void*,const void*,const void*,const void*,const void*,Chirp*))ptr)(args[0],args[1],args[2],args[3],args[4],args[5],this);
        else if (n==7)
            responseInt = (*(uint32_t(*)(const void*,const void*,const void*,const void*,const void*,const void*,const void*,Chirp*))ptr)(args[0],args[1],args[2],args[3],args[4],args[5],args[6],this);
        else if (n==8)
            responseInt = (*(uint32_t(*)(const void*,const void*,const void*,const void*,const void*,const void*,const void*,const void*,Chirp*))ptr)(args[0],args[1],args[2],args[3],args[4],args[5],args[6],args[7],this);
        else if (n==9)
            responseInt = (*(uint32_t(*)(const void*,const void*,const void*,const void*,const void*,const void*,const void*,const void*,const void*,Chirp*))ptr)(args[0],args[1],args[2],args[3],args[4],args[5],args[6],args[7],args[8],this);
        else if (n==10)
            responseInt = (*(uint32_t(*)(const void*,const void*,const void*,const void*,const void*,const void*,const void*,const void*,const void*,const void*,Chirp*))ptr)(args[0],args[1],args[2],args[3],args[4],args[5],args[6],args[7],args[8],args[9],this);
        else
            responseInt = CRP_RES_ERROR;
        m_call = false;
    }

    // if it's a chirp call, we need to send back the result
    // result is in m_buf
    if (type&CRP_CALL)
    {
        // write responseInt
        *(uint32_t *)(m_buf+m_headerLen) = responseInt;
        // send response
        res = sendChirpRetry(CRP_RESPONSE | (type&~CRP_CALL), m_procTable[proc].chirpProc);	// convert call into response
        restoreBuffer(); // restore buffer immediately!
        if (res!=CRP_RES_OK) 
            return res;
    }

    return CRP_RES_OK;
}

int Chirp::reallocTable()
{
    ProcTableEntry *newProcTable;
    int newProcTableSize;

    // allocate new table, zero
    newProcTableSize = m_procTableSize+CRP_PROCTABLE_LEN;
    newProcTable = new (std::nothrow) ProcTableEntry[newProcTableSize];
    if (newProcTable==NULL)
        return CRP_RES_ERROR_MEMORY;
    memset(newProcTable, 0, sizeof(ProcTableEntry)*newProcTableSize);
    // copy to new table
    memcpy(newProcTable, m_procTable, sizeof(ProcTableEntry)*m_procTableSize);
    // delete old table
    delete [] m_procTable;
    // set to new
    m_procTable = newProcTable;
    m_procTableSize = newProcTableSize;

    return CRP_RES_OK;
}

ChirpProc Chirp::lookupTable(const char *procName)
{
    ChirpProc i;

    for(i=0; i<m_procTableSize; i++)
    {
        if (m_procTable[i].procName!=NULL && strcmp(m_procTable[i].procName, procName)==0)
            return i;
    }
    return -1;
}


ChirpProc Chirp::updateTable(const char *procName, ProcPtr procPtr)
{
    // if it exists already, update,
    // if it doesn't exist, add it
    if (procName==NULL)
        return -1;

    ChirpProc proc = lookupTable(procName);
    if (proc<0) // next empty entry
    {
        for (proc=0; proc<m_procTableSize && m_procTable[proc].procName; proc++);
        if (proc==m_procTableSize)
        {
            reallocTable();
            return updateTable(procName, procPtr);
        }
    }

    // add to table
    m_procTable[proc].procName = procName;
    m_procTable[proc].procPtr = procPtr;

    return proc;
}

ChirpProc Chirp::getProc(const char *procName, ProcPtr callback)
{
    uint32_t res;
    ChirpProc cproc = -1;

    if (callback)
        cproc = updateTable(procName, callback);

    if (call(CRP_CALL_ENUMERATE, 0,
             STRING(procName), // send name
             INT16(cproc), // send local index
             END_OUT_ARGS,
             &res, // get remote index
             END_IN_ARGS
             )>=0)
        return res;

    // a negative ChirpProc is an error
    return -1;
}

int Chirp::remoteInit(bool connect)
{
    int res;
    uint32_t responseInt;
    uint8_t hinformer;

    res = call(CRP_CALL_INIT, 0,
               UINT16(connect ? m_blkSize : 0), // send block size
               UINT8(m_hinterested), // send whether we're interested in hints or not
               END_OUT_ARGS,
               &responseInt,
               &hinformer,       // receive whether we should send hints
               END_IN_ARGS
               );
    if (res>=0)
    {
        m_connected = connect;
        m_hinformer = hinformer;
        return responseInt;
    }
    return res;
}

int Chirp::getProcInfo(ChirpProc proc, ProcInfo *info)
{
    uint32_t responseInt;
    int res;

    res = call(CRP_CALL_ENUMERATE_INFO, 0,
               UINT16(proc),
               END_OUT_ARGS,
               &responseInt,
               &info->procName,
               &info->argTypes,
               &info->procInfo,
               END_IN_ARGS
               );

    if (res==CRP_RES_OK)
        return responseInt;
    return res;
}

int Chirp::setProc(const char *procName, ProcPtr proc, ProcTableExtension *extension)
{
    ChirpProc cProc = updateTable(procName, proc);
    if (cProc<0)
        return CRP_RES_ERROR;

    m_procTable[cProc].extension = extension;
    return CRP_RES_OK;
}

int Chirp::registerModule(const ProcModule *module)
{
    int i;

    for (i=0; module[i].procName; i++)
        setProc(module[i].procName, module[i].procPtr, (ProcTableExtension *)module[i].argTypes);

    return CRP_RES_OK;
}

void Chirp::setSendTimeout(uint32_t timeout)
{
    m_sendTimeout = timeout;
}

void Chirp::setRecvTimeout(uint32_t timeout)
{
    m_headerTimeout = timeout;
}

int32_t Chirp::handleEnumerate(char *procName, ChirpProc *callback)
{
    ChirpProc proc;
    // lookup in table
    proc = lookupTable(procName);
    // set remote index in table
    m_procTable[proc].chirpProc = *callback;

    return proc;
}

int32_t Chirp::handleInit(uint16_t *blkSize, uint8_t *hinformer)
{
    int32_t responseInt;

    bool connect = *blkSize ? true : false;
    responseInt = init(connect);
    m_connected = connect;
    m_blkSize = *blkSize;  // get block size, write it
    m_hinformer = *hinformer;

    CRP_RETURN(this, UINT8(m_hinterested), END);

    return responseInt;
}

int32_t Chirp::handleEnumerateInfo(ChirpProc *proc)
{
    const ProcTableExtension *extension;
    uint8_t null = '\0';

    if (*proc>=m_procTableSize || m_procTable[*proc].procName==NULL)
        extension = NULL;
    else
        extension = m_procTable[*proc].extension;

    if (extension)
    {
        CRP_RETURN(this, STRING(m_procTable[*proc].procName), STRING(extension->argTypes),
                   STRING(extension->procInfo), END);
        return CRP_RES_OK;
    }
    else // no extension, just send procedure name
    {
        CRP_RETURN(this, STRING(m_procTable[*proc].procName), STRING(&null),
                   STRING(&null), END);
        return CRP_RES_ERROR;
    }
}

int Chirp::realloc(uint32_t min)
{
    if (m_sharedMem)
        return CRP_RES_ERROR_MEMORY;

    if (!min)
        min = m_bufSize+CRP_BUFSIZE;
    else
        min += CRP_BUFSIZE;
    uint8_t *newbuf = new (std::nothrow) uint8_t[min];
    if (newbuf==NULL)
        return CRP_RES_ERROR_MEMORY;
    memcpy(newbuf, m_buf, m_bufSize);
    delete[] m_buf;
    m_buf = newbuf;
    m_bufSize = min;

    return CRP_RES_OK;
}

// service deals with calls and callbacks
int Chirp::service(bool all)
{
    int i;
    uint8_t type;
    ChirpProc recvProc;
    void *args[CRP_MAX_ARGS+1];

    for (i=0; true; i++)
    {
        if (recvChirp(&type, &recvProc, args)==CRP_RES_OK)
            handleChirp(type, recvProc, (const void **)args);
        else
            break;
        if (!all)
            break;
    }

    return i;
}

int Chirp::recvChirp(uint8_t *type, ChirpProc *proc, void *args[], bool wait) // null pointer terminates
{
    int res;
    uint32_t i, offset;

    restoreBuffer();

    // receive
    if (m_errorCorrected)
        res = recvFull(type, proc, wait);
    else
    {
        for (i=0; true; i++)
        {
            res = recvHeader(type, proc, wait);
            if (res==CRP_RES_ERROR_CRC)
            {
                if (i<m_maxNak)
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
    }
    if (res!=CRP_RES_OK)
        return res;

    // get responseInt from response
    if (*type&CRP_RESPONSE)
    {
        // fake responseInt so it gets inserted
        *(m_buf+m_headerLen-4) = CRP_UINT32; // write type so it parses correctly
        *(m_buf+m_headerLen-1) = CRP_UINT32;
        // increment pointer
        offset = m_headerLen-4;
        m_len+=4;
    }
    else // call has no responseInt
        offset = m_headerLen;

    return deserializeParse(m_buf+offset, m_len, args);
}

int Chirp::deserialize(uint8_t *buf, uint32_t len, ...)
{
    int res;
    va_list args;
    void *recvArgs[CRP_MAX_ARGS+1];

    if ((res=deserializeParse(buf, len, recvArgs))<0)
        return res;

    va_start(args, len);
    res = loadArgs(&args, recvArgs);
    va_end(args);

    return res;
}

int Chirp::vdeserialize(uint8_t *buf, uint32_t len, va_list *args)
{
    int res;
    void *recvArgs[CRP_MAX_ARGS+1];

    if ((res=deserializeParse(buf, len, recvArgs))<0)
        return res;

    return loadArgs(args, recvArgs);
}

int Chirp::getArgList(uint8_t *buf, uint32_t len, uint8_t *argList)
{
    uint8_t dataType, size, a;
    uint32_t i;

    // parse remaining args
    for(i=0, a=0; i<len; a++)
    {
        if (a==CRP_MAX_ARGS)
            return CRP_RES_ERROR;

        dataType = buf[i++];
        argList[a] = dataType;
        size = dataType&0x0f;
        if (!(dataType&CRP_ARRAY)) // if we're a scalar
        {
            ALIGN(i, size);
            i += dataType&0x0f; // extract size of scalar, add it
        }
        else // we're an array
        {
            if (dataType==CRP_STRING || dataType==CRP_HSTRING) // string is a special case
                i += strlen((char *)(buf+i))+1; // +1 include null character
            else
            {
                ALIGN(i, 4);
                uint32_t len = *(uint32_t *)(buf+i);
                i += 4;
                ALIGN(i, size);
                i += len*size;
            }
        }
    }
    argList[a] = '\0'; // terminate list
    return CRP_RES_OK;
}


int Chirp::deserializeParse(uint8_t *buf, uint32_t len, void *args[])
{
    uint8_t dataType, size, a;
    uint32_t i;

    // parse remaining args
    for(i=0, a=0; i<len; a++)
    {
        if (a==CRP_MAX_ARGS)
            return CRP_RES_ERROR;

        dataType = buf[i++];
        size = dataType&0x0f;
        if (!(dataType&CRP_ARRAY)) // if we're a scalar
        {
            ALIGN(i, size);
            args[a] = (void *)(buf+i);
            i += dataType&0x0f; // extract size of scalar, add it
        }
        else // we're an array
        {
            if (dataType==CRP_STRING || dataType==CRP_HSTRING) // string is a special case
            {
                args[a] = (void *)(buf+i);
                i += strlen((char *)(buf+i))+1; // +1 include null character
            }
            else
            {
                ALIGN(i, 4);
                uint32_t len = *(uint32_t *)(buf+i);
                args[a++] = (void *)(buf+i);
                i += 4;
                ALIGN(i, size);
                args[a] = (void *)(buf+i);
                i += len*size;
            }
        }
    }
    args[a] = NULL; // terminate list
    return CRP_RES_OK;
}

uint8_t Chirp::getType(const void *arg)
{
    return *((uint8_t *)arg - 1);
}

uint16_t Chirp::calcCrc(uint8_t *buf, uint32_t len)
{
    uint32_t i;
    uint16_t crc;

    // this isn't a real crc, but it's cheap and prob good enough
    for (i=0, crc=0; i<len; i++)
        crc += buf[i];
    crc += len;

    return crc;
}


int Chirp::sendFull(uint8_t type, ChirpProc proc)
{
    int res;

    *(uint32_t *)m_buf = CRP_START_CODE;
    *(uint8_t *)(m_buf+4) = type;
    *(ChirpProc *)(m_buf+6) = proc;
    *(uint32_t *)(m_buf+8) = m_len;
    // send header
    if ((res=m_link->send(m_buf, CRP_MAX_HEADER_LEN, m_sendTimeout))<0)
        return res;
    // if we haven't sent everything yet....
    if (m_len+m_headerLen>CRP_MAX_HEADER_LEN && !m_sharedMem)
    {
        if ((res=m_link->send(m_buf+CRP_MAX_HEADER_LEN, m_len-(CRP_MAX_HEADER_LEN-m_headerLen), m_sendTimeout))<0)
            return res;
    }
    return CRP_RES_OK;
}

int Chirp::sendHeader(uint8_t type, ChirpProc proc)
{
    int res;
    bool ack;
    uint32_t chunk, startCode = CRP_START_CODE;
    uint16_t crc;

    if ((res=m_link->send((uint8_t *)&startCode, 4, m_sendTimeout))<0)
        return res;

    *(uint8_t *)m_buf = type;
    *(uint16_t *)(m_buf+2) = proc;
    *(uint32_t *)(m_buf+4) = m_len;
    if ((res=m_link->send(m_buf, m_headerLen, m_sendTimeout))<0)
        return res;
    crc = calcCrc(m_buf, m_headerLen);

    if (m_len>=CRP_MAX_HEADER_LEN)
        chunk = CRP_MAX_HEADER_LEN;
    else
        chunk = m_len;
    if (m_link->send(m_buf, chunk, m_sendTimeout)<0)
        return CRP_RES_ERROR_SEND_TIMEOUT;

    // send crc
    crc += calcCrc(m_buf, chunk);
    if (m_link->send((uint8_t *)&crc, 2, m_sendTimeout)<0)
        return CRP_RES_ERROR_SEND_TIMEOUT;

    if ((res=recvAck(&ack, m_headerTimeout))<0)
        return res;

    if (ack)
        m_offset = chunk;
    else
        return CRP_RES_ERROR_CRC;

    return CRP_RES_OK;
}

int Chirp::sendData()
{
    uint16_t crc;
    uint32_t chunk;
    uint8_t sequence;
    bool ack;
    int res;

    for (sequence=0; m_offset<m_len; )
    {
        if (m_len-m_offset>=m_blkSize)
            chunk = m_blkSize;
        else
            chunk = m_len-m_offset;
        // send data
        if (m_link->send(m_buf+m_offset, chunk, m_sendTimeout)<0)
            return CRP_RES_ERROR_SEND_TIMEOUT;
        // send sequence
        if (m_link->send((uint8_t *)&sequence, 1, m_sendTimeout)<0)
            return CRP_RES_ERROR_SEND_TIMEOUT;
        // send crc
        crc = calcCrc(m_buf+m_offset, chunk) + calcCrc((uint8_t *)&sequence, 1);
        if (m_link->send((uint8_t *)&crc, 2, m_sendTimeout)<0)
            return CRP_RES_ERROR_SEND_TIMEOUT;

        if ((res=recvAck(&ack, m_dataTimeout))<0)
            return res;
        if (ack)
        {
            m_offset += chunk;
            sequence++;
        }
    }
    return CRP_RES_OK;
}

int Chirp::sendAck(bool ack) // false=nack
{
    uint8_t c;

    if (ack)
        c = CRP_ACK;
    else
        c = CRP_NACK;

    if (m_link->send(&c, 1, m_sendTimeout)<0)
        return CRP_RES_ERROR_SEND_TIMEOUT;

    return CRP_RES_OK;
}

int Chirp::recvHeader(uint8_t *type, ChirpProc *proc, bool wait)
{
    uint8_t c;
    uint32_t chunk, startCode = 0;
    uint16_t crc, rcrc;

    int return_value;

    return_value = m_link->receive(&c, 1, wait?m_headerTimeout:0);

    if (return_value < 0) {
      goto chirp_recvheader__exit;
    }
    if (return_value == 0) {
      return_value = CRP_RES_ERROR;
      goto chirp_recvheader__exit;
    }

    // find start code
    while(1)
    {
        startCode >>= 8;
        startCode |= (uint32_t)c<<24;
        if (startCode==CRP_START_CODE)
            break;

        return_value = m_link->receive(&c, 1, m_idleTimeout);

        if (return_value < 0) {
          goto chirp_recvheader__exit;
        }
        if (return_value == 0) {
          return_value = CRP_RES_ERROR;
          goto chirp_recvheader__exit;
        }
    }
    // receive rest of header
    if (m_link->receive(m_buf, m_headerLen, m_idleTimeout) < 0) {
      return_value = CRP_RES_ERROR_RECV_TIMEOUT;
      goto chirp_recvheader__exit;
    }

    if (return_value < (int) m_headerLen) {
      return_value = CRP_RES_ERROR; 
      goto chirp_recvheader__exit;
    }

    *type = *(uint8_t *)m_buf;
    *proc = *(ChirpProc *)(m_buf+2);
    m_len = *(uint32_t *)(m_buf+4);
    crc = calcCrc(m_buf, m_headerLen);

    if (m_len>=CRP_MAX_HEADER_LEN-m_headerLen)
        chunk = CRP_MAX_HEADER_LEN-m_headerLen;
    else
        chunk = m_len;

    return_value = m_link->receive(m_buf, chunk+2, m_idleTimeout);

    if (return_value < 0) { // +2 for crc
      goto chirp_recvheader__exit;
    }
    if (return_value < (int) (chunk + 2)) {
      return_value = CRP_RES_ERROR;
      goto chirp_recvheader__exit;
    }
    copyAlign((char *)&rcrc, (char *)(m_buf+chunk), 2);
    if (rcrc==crc+calcCrc(m_buf, chunk))
    {
        m_offset = chunk;
        sendAck(true);
    }
    else
    {
        sendAck(false); // send nack
        return_value = CRP_RES_ERROR_CRC;
        goto chirp_recvheader__exit;
    }

    return_value = CRP_RES_OK;

chirp_recvheader__exit:

    return return_value;
}

int Chirp::recvFull(uint8_t *type, ChirpProc *proc, bool wait)
{
    int res;
    uint32_t startCode;
    uint32_t len, recvd;

    // receive header, with startcode check to make sure we're synced
    while(1)
    {
        if ((res=m_link->receive(m_buf, CRP_MAX_HEADER_LEN, wait?m_headerTimeout:0))<0)
            return res;
        // check to see if we received less data than expected
        if (res<sizeof(uint32_t))
            continue;
		recvd = res;
        startCode = *(uint32_t *)m_buf;
        if (startCode==CRP_START_CODE)
            break;
    }
    *type = *(uint8_t *)(m_buf+4);
    *proc = *(ChirpProc *)(m_buf+6);
    m_len = *(uint32_t *)(m_buf+8);

    if (m_len+m_headerLen>m_bufSize && (res=realloc(m_len+m_headerLen))<0)
        return res;

    if (m_len+m_headerLen>recvd && !m_sharedMem)
    {
        len = m_len+m_headerLen;
        while(recvd<len)
        {
            if ((res=m_link->receive(m_buf+recvd, len-recvd, m_idleTimeout))<0)
                return res;
            recvd += res;
        }
    }

    return CRP_RES_OK;
}

// We assume that the probability that we send a nack and the receiver interprets a nack is 100%
// We can't assume that the probability that we send an ack and the receiver interprets it is 100%
// Scenario
// 1) we receive packet 0, redo is 0, crc is good, we increment offset, send ack. (inc) (inc) rs=0, s=0, inc
// 2) we receive packet 1, crc is bad, we send nack (!inc) (!inc) rs=1, s=1
// 3) sender gets nack, so it resends
// 4) we receive packet 1, redo is 1, crc is good, we don't increment offset, send ack (inc) (inc) rs=1, s=1
// 5) we receive packet 2, redo is 0, crc is bad, we send nack (!inc) (!inc) rs=2, s=2
// 6) we receive packet 2, redo is 1, crc is good, we send ack (inc) (inc) rs=2, s=2
// 7) we receive packet 3, redo is 0, crc is good, we send ack (inc) (inc)
// different scenario
// 1) we receive packet 0, redo is 0, crc is good, we increment offset, send ack. (inc) (inc) rs=0, s=0
// 2) sender thinks it gets a nack, so it resends
// 3) we receive packet 0 again, but crc is bad, we send nack (!inc) (!inc) rs=1, s=0
// 4) sender gets nack, so it resends
// 5) we receive packet 0, redo is 1, crc is good, we don't increment offset, send ack (!inc) (inc) rs=1, s=0
// (we've essentially thrown out this packet, but that's ok, because we have a good packet 0)
// 6) we receive packet 1, redo is 0, crc is bad, we send nack (!inc) (!inc) rs=1, s=1
// 7) we receive packet 1, redo is 1, crc is good, we send ack (inc) (inc) rs=1, s=1
// 8) we receive packet 2, redo is 0, crc is good, we send ack (inc) (inc) rs=2, s=2
// a redo flag is not sufficient to communicate which packet we're on because the sender can misinterpret
// any number of nacks
int Chirp::recvData()
{
    int res;
    uint32_t chunk;
    uint16_t crc;
    uint8_t sequence, rsequence, naks;

    if (m_len+3+m_headerLen>m_bufSize && (res=realloc(m_len+3+m_headerLen))<0) // +3 to read sequence, crc
        return res;

    for (rsequence=0, naks=0; m_offset<m_len; )
    {
        if (m_len-m_offset>=m_blkSize)
            chunk = m_blkSize;
        else
            chunk = m_len-m_offset;
        if (m_link->receive(m_buf+m_offset, chunk+3, m_dataTimeout)<0) // +3 to read sequence, crc
            return CRP_RES_ERROR_RECV_TIMEOUT;
        if (res<(int)chunk+3)
            return CRP_RES_ERROR;
        sequence = *(uint8_t *)(m_buf+m_offset+chunk);
        copyAlign((char *)&crc, (char *)(m_buf+m_offset+chunk+1), 2);
        if (crc==calcCrc(m_buf+m_offset, chunk+1))
        {
            if (rsequence==sequence)
            {
                m_offset += chunk;
                rsequence++;
            }
            sendAck(true);
            naks = 0;
        }
        else
        {
            sendAck(false);
            naks++;
            if (naks<m_maxNak)
                naks++;
            else
                return CRP_RES_ERROR_MAX_NAK;
        }
    }
    return CRP_RES_OK;
}

int Chirp::recvAck(bool *ack, uint16_t timeout) // false=nack
{
    int res;
    uint8_t c;
    if ((res=m_link->receive(&c, 1, timeout))<0)
        return CRP_RES_ERROR_RECV_TIMEOUT;
    if (res<1)
        return CRP_RES_ERROR;

    if (c==CRP_ACK)
        *ack = true;
    else
        *ack = false;

    return CRP_RES_OK;
}
