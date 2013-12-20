#ifndef BLOBS_H
#include <stdint.h>
#include "cblob.h"

#define NUM_MODELS      7
#define MAX_BLOBS       256
#define MAX_MERGE_DIST  5
#define MIN_AREA        20


class Blobs
{
public:
    Blobs();
    ~Blobs();

private:
    void blobify();
    uint16_t combine(uint16_t *boxes, uint16_t numBoxes);
    uint16_t combine2(uint16_t *boxes, uint16_t numBoxes);
    uint16_t compress(uint16_t *boxes, uint16_t numBoxes);

    bool closeby(int a, int b, int dist);
    void addCoded(int a, int b);
    void processCoded();


    CBlobAssembler m_assembler[NUM_MODELS];
    //SSegment *m_qmem;
    uint32_t *m_qmem;
    uint8_t *m_lut;
    uint32_t m_qindex;
    uint16_t *m_boxes;
    uint16_t m_maxBoxes;
    uint16_t m_numBoxes;
    uint16_t m_numCodedBoxes;
    uint32_t m_minArea;
    uint16_t m_mergeDist;
};

#define BLOBS_H

#endif // BLOBS_H
