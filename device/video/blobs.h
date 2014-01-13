#ifndef BLOBS_H
#include <stdint.h>
#include "cblob.h"
#include "colorlut.h"
#include "pixytypes.h"

#define NUM_MODELS      7
#define MAX_BLOBS       200
#define MAX_MERGE_DIST  5
#define MIN_AREA        20

#define LUT_MEMORY		((uint8_t *)SRAM1_LOC + SRAM1_SIZE-CL_LUT_SIZE)  // +0x100 make room for prebuf and palette

#define BL_BEGIN_MARKER	0xaa55
#define BL_END_MARKER	0xccaa

class Qqueue;

class Blobs
{
public:
    Blobs(Qqueue *qq);
    ~Blobs();
    void blobify();
	uint16_t getBlock(uint16_t *buf);
	uint16_t *getMaxBlob(uint16_t signature); 
	void getBlobs(BlobA **blobs, uint32_t *len);

	int generateLUT(uint8_t model, const Frame8 &frame, const RectA &region, ColorModel *pcmodel=NULL);
	int generateLUT(uint8_t model, const Frame8 &frame, const Point16 &seed, ColorModel *pcmodel=NULL, RectA *region=NULL);

	ColorLUT *m_clut;

private:
    uint16_t combine(uint16_t *blobs, uint16_t numBlobs);
    uint16_t combine2(uint16_t *blobs, uint16_t numBlobs);
    uint16_t compress(uint16_t *blobs, uint16_t numBlobs);

    bool closeby(int a, int b, int dist);
    void addCoded(int a, int b);
    void processCoded();
	void copyBlobs();

    CBlobAssembler m_assembler[NUM_MODELS];
	Qqueue *m_qq;

    uint16_t *m_blobs;
    uint16_t m_numBlobs;

	bool m_mutex;
    uint16_t m_maxBlobs;

	uint16_t m_blobReadIndex;

    uint16_t m_numCodedBlobs;
    uint32_t m_minArea;
    uint16_t m_mergeDist;
};

#define BLOBS_H

#endif // BLOBS_H
