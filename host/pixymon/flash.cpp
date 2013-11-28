#include <QFile>
#include <stdexcept>
#include "flash.h"
#include "reader.h"


Flash::Flash() : m_chirp(false, true) // not hinterested, client
{
    uint32_t sectorSizeProc;

    if (m_link.open()<0)
        throw std::runtime_error("Cannot initialize USB for flash programming.");
    m_chirp.setLink(&m_link);

    sectorSizeProc = m_chirp.getProc("flash_sectorSize");
    m_programProc = m_chirp.getProc("flash_program");
    m_reset = m_chirp.getProc("flash_reset");

    if (sectorSizeProc<0 || m_programProc<0)
        throw std::runtime_error("Cannot get flash procedures.");

    if (m_chirp.callSync(sectorSizeProc, END_OUT_ARGS,
                     &m_sectorSize, END_IN_ARGS)<0)
        throw std::runtime_error("Cannot get flash sector size.");

    m_buf = new char[m_sectorSize];
}

Flash::~Flash()
{
    delete[] m_buf;
}

void Flash::program(const QString &filename)
{
#if 0
    QFile file(filename);
    uint32_t len;
    uint32_t addr;
    int32_t response;

    if (!file.open(QIODevice::ReadOnly))
        throw std::runtime_error((QString("Cannot open file ") + filename + QString(".")).toStdString());

    for(addr=0x14000000; !file.atEnd(); addr+=len)
    {
        len =(uint32_t)file.read(m_buf, m_sectorSize);
        m_chirp.callSync(m_programProc, UINT32(addr), UINTS8(len, m_buf), END_OUT_ARGS,
                         &response, END_IN_ARGS);
        if (response==-1)
            throw std::runtime_error("Invalid address range.");
        else if (response==-3)
            throw std::runtime_error("Error during verify.");
        else if (response<0)
            throw std::runtime_error("Error during programming.");
    }

#else
    IReader *reader;
    unsigned long addr, len;
    int32_t res, response;

    reader = createReader(filename);
    while(1)
    {
        res = reader->read((unsigned char *)m_buf, m_sectorSize, &addr, &len);
        if (len)
        {
            if (m_chirp.callSync(m_programProc, UINT32(addr), UINTS8(len, m_buf), END_OUT_ARGS,
                             &response, END_IN_ARGS)<0)
                throw std::runtime_error("Communication error during programming.");
            if (response==-1)
                throw std::runtime_error("Invalid address range.");
            else if (response==-3)
                throw std::runtime_error("Error during verify.");
            else if (response<0)
                throw std::runtime_error("Error during programming.");
        }
        if (res<0)
            break;
    }
#endif
    // reset Pixy
    if (m_chirp.callSync(m_reset, END_OUT_ARGS,
                         &response, END_IN_ARGS)<0)
        throw std::runtime_error("Unable to reset.");
    destroyReader(reader);
}

