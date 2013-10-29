#ifndef DFU_H
#define DFU_H

#include <QString>
#include "dfu_info.h"

#define DFU_TIMEOUT   5000

class Dfu
{
public:
    Dfu();
    ~Dfu();

    int download(const QString &filename);

private:
    libusb_context *m_context;
    unsigned int m_transfer_size;
    dfu_if m_dif;

};

#endif // DFU_H
