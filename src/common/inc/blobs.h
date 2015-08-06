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
#include "pixytypes.h"
#include "colorlut.h"
#include "qqueue.h"

#define MAX_BLOBS             100
#define MAX_BLOBS_PER_MODEL   20
#define MAX_MERGE_DIST        7
#define MIN_AREA              20
#define MIN_COLOR_CODE_AREA   10
#define MAX_CODED_DIST        8
#define MAX_COLOR_CODE_MODELS 5

#define BL_BEGIN_MARKER	      0xaa55
#define BL_BEGIN_MARKER_CC    0xaa56

enum ColorCodeMode
{
    DISABLED = 0,
    ENABLED = 1,
    CC_ONLY = 2,
    MIXED = 3 // experimental
};

class Blobs
{
public:
    Blobs(Qqueue *qq, uint8_t *lut);
    ~Blobs();
    int blobify();
    uint16_t getBlock(uint8_t *buf, uint32_t buflen);
    uint16_t getCCBlock(uint8_t *buf, uint32_t buflen);
    BlobA *getMaxBlob(uint16_t signature=0, uint16_t *numBlobs=NULL);
    void getBlobs(BlobA **blobs, uint32_t *len, BlobB **ccBlobs, uint32_t *ccLen);
    int setParams(uint16_t maxBlobs, uint16_t maxBlobsPerModel, uint32_t minArea, ColorCodeMode ccMode);
    int runlengthAnalysis();
#ifndef PIXY
    void getRunlengths(uint32_t **qvals, uint32_t *len);
#endif

	ColorLUT m_clut;
    Qqueue *m_qq;

private:
    int handleSegment(uint8_t signature, uint16_t row, uint16_t startCol, uint16_t length);
	void endFrame();
    uint16_t combine(uint16_t *blobs, uint16_t numBlobs);
    uint16_t combine2(uint16_t *blobs, uint16_t numBlobs);
    uint16_t compress(uint16_t *blobs, uint16_t numBlobs);

    bool closeby(BlobA *blob0, BlobA *blob1);
    int16_t distance(BlobA *blob0, BlobA *blob1);
    void sort(BlobA *blobs[], uint16_t len, BlobA *firstBlob, bool horiz);
    int16_t angle(BlobA *blob0, BlobA *blob1);
    int16_t distance(BlobA *blob0, BlobA *blob1, bool horiz);
    void processCC();
    void cleanup(BlobA *blobs[], int16_t *numBlobs);
    void cleanup2(BlobA *blobs[], int16_t *numBlobs);
    bool analyzeDistances(BlobA *blobs0[], int16_t numBlobs0, BlobA *blobs[], int16_t numBlobs, BlobA **blobA, BlobA **blobB);
    void mergeClumps(uint16_t scount0, uint16_t scount1);

    void printBlobs();

    CBlobAssembler m_assembler[CL_NUM_SIGNATURES];

    uint16_t *m_blobs;
    uint16_t m_numBlobs;

    BlobB *m_ccBlobs;
    uint16_t m_numCCBlobs;

    bool m_mutex;
    uint16_t m_maxBlobs;
    uint16_t m_maxBlobsPerModel;

    uint16_t m_blobReadIndex;
    uint16_t m_ccBlobReadIndex;

    uint32_t m_minArea;
    uint16_t m_mergeDist;
    uint16_t m_maxCodedDist;
    ColorCodeMode m_ccMode;
    BlobA *m_maxBlob;

#ifndef PIXY
    uint32_t m_numQvals;
    uint32_t *m_qvals;
#endif
};



#endif // BLOBS_H
