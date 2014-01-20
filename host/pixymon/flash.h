#ifndef FLASH_H
#define FLASH_H

#include <QString>
#include "usblink.h"
#include <chirp.hpp>

class Flash
{
public:
    Flash();
    ~Flash();

    void program(const QString &filename);

private:
    char *m_buf;
    USBLink m_link;
    Chirp m_chirp;
    uint32_t m_sectorSize;
    ChirpProc m_programProc;
    ChirpProc m_reset;
};

#endif // FLASH_H
