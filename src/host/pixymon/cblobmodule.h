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

#ifndef CBLOBMODULE_H
#define CBLOBMODULE_H

#include "monmodule.h"
#include "colorblob.h"
#include "blobs2.h"

struct Qval2
{
    Qval2()
    {
        m_u = m_v = m_y = m_col = 0;
    }

    Qval2(int16_t u, int16_t v, uint16_t y, uint16_t col)
    {
        m_u = u;
        m_v = v;
        m_y = y;
        m_col = col;
    }

    int16_t m_u;
    int16_t m_v;
    uint16_t m_y;
    uint16_t m_col;
};

struct Qval3
{
    Qval3()
    {
        m_u = m_v = m_y = m_col = 0;
    }

    Qval3(int16_t u, int16_t v, uint16_t y, uint16_t col)
    {
        m_u = u;
        m_v = v;
        m_y = y;
        m_col = col;
    }

    uint16_t m_col;
    int16_t m_v;
    int16_t m_u;
    uint16_t m_y;
};

struct QqueueFields2
{
    uint16_t readIndex;
    uint16_t writeIndex;

    uint16_t produced;
    uint16_t consumed;

    // (array size below doesn't matter-- we're just going to cast a pointer to this struct)
    Qval2 data[1]; // data
};

#undef QQ_SIZE
#undef QQ_MEM_SIZE
#define QQ_SIZE       0x30000
#define QQ_MEM_SIZE  ((QQ_SIZE-sizeof(struct QqueueFields2)+sizeof(Qval2))/sizeof(Qval2))

class Qqueue2
{
public:
    Qqueue2();
    ~Qqueue2();

    uint32_t dequeue(Qval2 *val);
    uint32_t queued()
    {
        return m_fields->produced - m_fields->consumed;
    }
    int enqueue(const Qval2 &val);

    uint32_t readAll(Qval2 *mem, uint32_t size);
    void flush();


//private:
    QqueueFields2 *m_fields;
};

class CBlobModule : public MonModule
{
public:
    CBlobModule(Interpreter *interpreter);
    ~CBlobModule();

    virtual bool render(uint32_t fourcc, const void *args[]);
    virtual bool command(const QStringList &argv);
    virtual void paramChange();

    void processBlobs(BlobA *blobs, uint32_t *numBlobs);
    void renderEX00(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame);
    void renderCCQ2(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame);

private:
    void handleLine(uint8_t *line, uint16_t width);
    void handleSegment(uint8_t signature, uint16_t row, uint16_t startCol, uint16_t length, bool blobs=true);
    void updateSignatures();
    void rls(const Frame8 *frame);
    void rla();
    void rla(uint8_t *qmem, uint32_t qmemSize);
    int uploadLut();


    ColorBlob *m_cblob;
    Qqueue2 *m_qq;
    uint8_t *m_lut;
    uint32_t *m_qvals;
    uint32_t m_numQvals;

    Blobs2 m_blobs;

    ColorSignature m_signatures[NUM_SIGNATURES];
    RuntimeSignature m_runtimeSigs[NUM_SIGNATURES];
    float m_acqRange;
    float m_trackRange;
    float m_miny;
    bool m_yfilter;
    bool m_fixedLength;
    bool m_yexp;
    uint32_t m_maxDist;
    float m_minRatio;
};

#endif // CBLOBMODULE_H
