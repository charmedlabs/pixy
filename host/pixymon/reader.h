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

#ifndef READER_H
#define READER_H

#include <stdexcept>
#include <QString>
#include <QFile>

#define S0				0
#define S1				1
#define S2				2
#define S3				3
#define S5				5
#define S7				7
#define S8				8
#define S9				9


class QFile;
class IReader;

IReader *createReader(const QString &filename);
void destroyReader(IReader *reader);

class IReader // combined interface and base class for readin' stuff
{
public:
    IReader(const QString &filename)
    {
        m_addr = 0;
        m_file.setFileName(filename);
        if (!m_file.open(QIODevice::ReadOnly))
            throw std::runtime_error((QString("Cannot open file ") + filename + QString(".")).toStdString());
    }
    virtual ~IReader()
    {
        m_file.close();
    }
    virtual int read(unsigned char *dataBuf, unsigned long sizeBuf, unsigned long *addr, unsigned long *len) = 0;

protected:
    unsigned long m_addr;
    QFile m_file;
};

class BinReader : public IReader
{
public:
    virtual int read(unsigned char *dataBuf, unsigned long sizeBuf, unsigned long *addr, unsigned long *len);
};


class IntelReader : public IReader
{
public:
    IntelReader(const QString &filename) : IReader(filename)
    {
    }
    virtual int read(unsigned char *dataBuf, unsigned long sizeBuf, unsigned long *addr, unsigned long *len);

private:
    void getChar(char &c);
    int hex2digit(char c);
};


#endif // READER_H
