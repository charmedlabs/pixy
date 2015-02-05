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

#ifndef FDMODULE_H
#define FDMODULE_H

#include "monmodule.h"
#include "facedetect.h"

// color connected components
class FdModule : public MonModule
{
public:
    FdModule(Interpreter *interpreter);
    ~FdModule();

    virtual bool render(uint32_t fourcc, const void *args[]);
    virtual bool command(const QStringList &argv);
    virtual void paramChange();

private:
    void renderEX01(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame);
    void faceDetect(QImage &image);

    CascadeClassifier m_cascade;
    float m_stepSize;
    float m_scaleFactor;
};

#endif // FDMODULE_H

