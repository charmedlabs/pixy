#ifndef CBLOBMODULE_H
#define CBLOBMODULE_H

#include "monmodule.h"
#include "colorblob.h"

#define MAX_QVALS  0x10000

class CBlobModule : public MonModule
{
public:
    CBlobModule(Interpreter *interpreter);
    ~CBlobModule();

    virtual void selection(int x0, int y0, int width, int height);
    virtual bool render(uint32_t fourcc, const void *args[]);
    virtual bool command(const QStringList &argv);

    void renderEX00(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame);

private:
    void rls(const Frame8 *frame);

    ColorBlob *m_cblob;
    uint32_t *m_qVals;
    uint8_t *m_lut;
    uint32_t m_numqVals;

};

#endif // CBLOBMODULE_H
