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
    ~IReader()
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
