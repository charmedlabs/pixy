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
