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

#ifndef PROCESSBLOBS_H
#define PROCESSBLOBS_H

#include <QObject>
#include "blobs.h"

class Interpreter;

class ProcessBlobs : public QObject
{
    Q_OBJECT

public:
    ProcessBlobs(Interpreter *interpreter);
    ~ProcessBlobs();

    void process(const Frame8 &frame, uint32_t *numBlobs, BlobA **blobs, uint32_t *numCCBlobs, BlobB **ccBlobs, uint32_t *numQvals, Qval **qMem);

    Blobs *m_blobs;

private slots:
    void handleParamChange();

private:
    void rls(const Frame8 &frame);

    Interpreter *m_interpreter;
    uint32_t *m_qMem;
    uint32_t m_numQvals;
    Qqueue *m_qq;

    uint16_t m_maxBlobs;
    uint16_t m_maxBlobsPerModel;
    uint32_t m_minArea;
    uint8_t m_ccMode;
};

#endif // PROCESSBLOBS_H
