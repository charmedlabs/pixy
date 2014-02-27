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

#include "reader.h"
#include <stdio.h>


IReader *createReader(const QString &filename)
{
    IReader *reader;

#if 0
    if (buf[0]==0x2e)
        pread = new BinReader;
    else
    {
        i = 0;
        while(buf[i]=='\t' || buf[i]=='\r' || buf[i]=='\n' || buf[i]==' ')
            i++;
        if (buf[i]=='S')
            pread = new IntelReader;
    }
#endif

    reader = new IntelReader(filename);

    return reader;
}


void destroyReader(IReader *reader)
{
    if (reader)
        delete reader;
}

int BinReader::read(unsigned char *dataBuf, unsigned long sizeBuf, unsigned long *addr, unsigned long *len)
{
    unsigned long readNum;

    readNum = m_file.read((char *)dataBuf, sizeBuf);

    *addr = m_addr;
    *len = readNum;
    m_addr += readNum;

    if (m_file.atEnd())
        return -1;
    else
        return 0;
}

int IntelReader::hex2digit(char c)
{
    if(c&0x40)
        c += 9;
    return c&0x0f;
}

void IntelReader::getChar(char &c)
{
    if (m_file.read(&c, 1)!=1)
        throw std::runtime_error("Parse error reading hex file.");
}

int IntelReader::read(unsigned char *dataBuf, unsigned long sizeBuf, unsigned long *addr, unsigned long *len)
{
    unsigned char val;
    char c, code;
    unsigned int i, length, address, offset = 0;

    *len = 0;

    while(1)
    {
        // Skip whitespace characters until we find an S
        while(1)
        {
            getChar(c);
            if (c!='\r' && c!='\n' && c!=' ')
                break;
        }

        // Check that this is an S record
        if (c!=':')
            throw std::runtime_error("Invalid hex file.");

        // get length
        getChar(c);
        length = hex2digit(c)<<4;
        getChar(c);
        length |= hex2digit(c);

        // get address
        for (i=0, address=0; i<4; i++)
        {
            getChar(c);
            val = hex2digit(c);
            address = (address<<4) | val;
        }
        if (offset==0)
            *addr = m_addr + address;

        // next 2 bytes are the code
        getChar(c);
        code = hex2digit(c)<<4;
        getChar(c);
        code |= hex2digit(c);

        if (code==0) // data
        {
            for (i=0; i<length; i++, offset++)
            {
                getChar(c);
                val = hex2digit(c)<<4;
                getChar(c);
                val |= hex2digit(c);
                dataBuf[offset] = val;
            }
        }
        else if (code==4) // extended address
        {
            for (i=0, address=0; i<4; i++)
            {
                getChar(c);
                val = hex2digit(c);
                address = (address<<4) | val;
            }
            m_addr = address << 16;
        }
        // else if (code==5), etc (ignore)

        // read checksum, ignore rest of line
        while(1)
        {
            getChar(c);
            if (c=='\r' || c=='\n' || c==' ')
                break;
        }

        if (code==1 || (code==4 && offset) || offset+64>sizeBuf)
        {
            *len = offset;
            if (code==1)
                return -1;
            else
                return 0;
        }
    }
}



