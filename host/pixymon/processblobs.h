#ifndef PROCESSBLOBS_H
#define PROCESSBLOBS_H

#include "blobs.h"

class ProcessBlobs
{
public:
    ProcessBlobs();
    ~ProcessBlobs();

    void process(const Frame8 &frame, uint32_t *numBlobs, BlobA **blobs, uint32_t *numQvals, Qval **qMem);

    Blobs *m_blobs;

private:
    void rls(const Frame8 &frame);

    uint32_t *m_qMem;
    uint32_t m_numQvals;
    Qqueue *m_qq;
};

#endif // PROCESSBLOBS_H
