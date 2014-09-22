#ifndef CBLOBMODULE_H
#define CBLOBMODULE_H

#include "monmodule.h"
#include "colorblob.h"

#define MAX_QVALS  0x10000

class Qqueue;

class CBlobModule : public MonModule
{
public:
    CBlobModule(Interpreter *interpreter);
    ~CBlobModule();

    virtual bool render(uint32_t fourcc, const void *args[]);
    virtual bool command(const QStringList &argv);
    virtual void paramChange();

    void renderEX00(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame);
    void renderCCQ2(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame);

private:
    void rls(const Frame8 *frame);

    ColorBlob *m_cblob;
    Qqueue *m_qq;
    uint8_t *m_lut;

    ColorSignature m_signatures[NUM_SIGNATURES];
    float m_acqRange;
    float m_trackRange;
    float m_miny;

};

#endif // CBLOBMODULE_H
