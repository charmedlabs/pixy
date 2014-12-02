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

    int renderCMV2(uint8_t renderFlags, uint32_t sigLen, uint8_t *sigs, uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame);
    void rls(const Frame8 *frame);
    void handleLine(uint8_t *line, uint16_t width);

    uint32_t m_crc;
    uint8_t *m_lut;
    Qqueue *m_qq;
    Blobs *m_blobs;
    uint8_t m_renderMode;
};

#endif // CCCMODULE_H
