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

#include "blobs.h"

class ProcessBlobs
{
public:
    ProcessBlobs();
    ~ProcessBlobs();

    void process(const Frame8 &frame, uint32_t *numBlobs, BlobA **blobs, uint32_t *numCCBlobs, BlobB **ccBlobs, uint32_t *numQvals, Qval **qMem);

    Blobs *m_blobs;

private:
    void rls(const Frame8 &frame);

    uint32_t *m_qMem;
    uint32_t m_numQvals;
    Qqueue *m_qq;
};

#endif // PROCESSBLOBS_H
