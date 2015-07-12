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

#ifndef CCCMODULE_H
#define CCCMODULE_H

#include "monmodule.h"
#include "qqueue.h"
#include "blobs.h"

// color connected components
class CccModule : public MonModule
{
public:
    CccModule(Interpreter *interpreter);
    ~CccModule();

    virtual bool render(uint32_t fourcc, const void *args[]);
    virtual bool command(const QStringList &argv);
    virtual void paramChange();

private:

    int renderCCB1(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t numBlobs, uint16_t *blobs);
    int renderCCB2(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t numBlobs, uint16_t *blobs, uint32_t numCCBlobs, uint16_t *ccBlobs);
    int renderCMV2(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame);
    void renderCCQ2(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame);
    void rls(const Frame8 *frame);
    void handleLine(uint8_t *line, uint16_t width);
    void renderBlobsB(bool blend, QImage *image, float scale, BlobB *blobs, uint32_t numBlobs);
    void renderBlobsA(bool blend, QImage *image, float scale, BlobA *blobs, uint32_t numBlobs);
    QString lookup(uint16_t signum);

    uint32_t m_crc;
    uint8_t *m_lut;
    Qqueue *m_qq;
    Blobs *m_blobs;
    uint8_t m_renderMode;
    uint32_t m_palette[CL_NUM_SIGNATURES];
    QList<QPair<uint16_t, QString> > m_labels;
};

#endif // CCCMODULE_H
