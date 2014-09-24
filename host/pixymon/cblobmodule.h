#ifndef CBLOBMODULE_H
#define CBLOBMODULE_H

#include "monmodule.h"
#include "colorblob.h"


struct Qval2
{
    Qval2()
    {
        m_qval = m_rgsum = m_bsum = 0;
    }

    Qval2(uint32_t qval, uint32_t rgsum, uint32_t bsum)
    {
        m_qval = qval;
        m_rgsum = rgsum;
        m_bsum = bsum;
    }

    uint32_t m_qval;
    uint32_t m_rgsum;
    uint32_t m_bsum;
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

    void renderEX00(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame);
    void renderCCQ2(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame);

private:
    void rls(const Frame8 *frame);

    ColorBlob *m_cblob;
    Qqueue2 *m_qq;
    uint8_t *m_lut;

    ColorSignature m_signatures[NUM_SIGNATURES];
    float m_acqRange;
    float m_trackRange;
    float m_miny;

};

#endif // CBLOBMODULE_H
