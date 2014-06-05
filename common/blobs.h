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
#ifndef BLOBS_H
#define BLOBS_H

#include <stdint.h>
#include "blob.h"
#include "colorlut.h"
#include "pixytypes.h"
#include "qqueue.h"

#define NUM_MODELS            7
#define MAX_BLOBS             100
#define MAX_BLOBS_PER_MODEL   20
#define MAX_MERGE_DIST        5
#define MIN_AREA              20
#define MIN_COLOR_CODE_AREA   10
#define MAX_CODED_DIST        6
#define MAX_COLOR_CODE_MODELS 5

#define LUT_MEMORY		((uint8_t *)SRAM1_LOC + SRAM1_SIZE-CL_LUT_SIZE)  // +0x100 make room for prebuf and palette

#define BL_BEGIN_MARKER	0xaa55


class Blobs
{
public:
    Blobs(Qqueue *qq);
    ~Blobs();
    void blobify();
    uint16_t getBlock(uint8_t *buf, uint32_t buflen);
    uint16_t *getMaxBlob(uint16_t signature=0);
    void getBlobs(BlobA **blobs, uint32_t *len, BlobB **ccBlobs, uint32_t *ccLen);
	int setParams(uint16_t maxBlobs, uint16_t maxBlobsPerModel, uint32_t minArea); 

    int generateLUT(uint8_t model, const Frame8 &frame, const RectA &region, ColorModel *pcmodel=NULL);
    int generateLUT(uint8_t model, const Frame8 &frame, const Point16 &seed, ColorModel *pcmodel=NULL, RectA *region=NULL);

    ColorLUT *m_clut;
#ifndef PIXY
    uint8_t *m_lut;
#endif

private:
    void unpack();
    uint16_t combine(uint16_t *blobs, uint16_t numBlobs);
    uint16_t combine2(uint16_t *blobs, uint16_t numBlobs);
    uint16_t compress(uint16_t *blobs, uint16_t numBlobs);

    bool closeby(BlobA *blob0, BlobA *blob1);
    int16_t distance(BlobA *blob0, BlobA *blob1);
    void sort(BlobA *blobs[], uint16_t len, BlobA *firstBlob, bool horiz);
    int16_t angle(BlobA *blob0, BlobA *blob1);
    int16_t distance(BlobA *blob0, BlobA *blob1, bool horiz);
    void processCoded();
    void cleanup(BlobA *blobs[], int16_t *numBlobs);
    void analyzeDistances(BlobA *blobs0[], int16_t numBlobs0, BlobA *blobs[], int16_t numBlobs, BlobA **blobA, BlobA **blobB);

    CBlobAssembler m_assembler[NUM_MODELS];
    Qqueue *m_qq;

    uint16_t *m_blobs;
    uint16_t m_numBlobs;

    BlobB *m_codedBlobs;
	uint16_t m_numCodedBlobs;

    bool m_mutex;
    uint16_t m_maxBlobs;
    uint16_t m_maxBlobsPerModel;

    uint16_t m_blobReadIndex;

    uint32_t m_minArea;
    uint16_t m_mergeDist;
    uint16_t m_maxCodedDist;
    uint8_t m_codedMode;
};


#endif // BLOBS_H
