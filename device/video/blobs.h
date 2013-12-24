#ifndef BLOBS_H
#include <stdint.h>
#include "cblob.h"

#define NUM_MODELS      7
#define MAX_BLOBS       200
#define MAX_MERGE_DIST  5
#define MIN_AREA        20

class Qqueue;

class Blobs
{
public:
    Blobs(Qqueue *qq);
    ~Blobs();
    void blobify();
	uint16_t getBlock(uint16_t *buf);
	uint16_t *getMaxBlob(uint16_t signature); 

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

    uint16_t *m_blobsCopy;
    uint16_t m_numBlobsCopy;

	bool m_mutex;
    uint16_t m_maxBlobs;

	uint16_t m_blobReadIndex;

    uint16_t m_numCodedBlobs;
    uint32_t m_minArea;
    uint16_t m_mergeDist;
};

#define BLOBS_H

#endif // BLOBS_H
