#include "cccmodule.h"
#include "interpreter.h"
#include "renderer.h"
#include "blobs.h"
#include "qqueue.h"
#include "colorlut.h"

// declare module
MON_MODULE(CccModule);

CccModule::CccModule(Interpreter *interpreter) : MonModule(interpreter)
{
    m_crc = 0;
    m_qq = new Qqueue();
    m_lut = new uint8_t[CL_LUT_SIZE];
    m_blobs = new Blobs(m_qq, m_lut);

    m_interpreter->m_pixymonParameters->add("Cooked render mode", PT_INT8, 2,
        "Cooked video rendering mode, 0=boxes only, 1=filtered pixels only, 2=boxes and filtered pixels");
    m_renderMode = pixymonParameter("Cooked render mode").toUInt();
}

CccModule::~CccModule()
{
    delete m_blobs;
    delete [] m_lut;
    delete m_qq;
}

bool CccModule::render(uint32_t fourcc, const void *args[])
{
    if (fourcc==FOURCC('C','M','V','2'))
    {
        // uint8_t renderFlags, uint32_t sigLen, uint8_t *sigs, uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame
        // 0                    1                2              3               4                5                  6
        renderCMV2(*(uint8_t *)args[0], *(uint32_t *)args[1], (uint8_t *)args[2], *(uint16_t *)args[3], *(uint16_t *)args[4], *(uint32_t *)args[5], (uint8_t *)args[6]);
        return true;
    }
    return false;
}

bool CccModule::command(const QStringList &argv)
{
    return false;
}

void CccModule::paramChange()
{
    QVariant val;
    bool relut = false;

    if (pixyParameterChanged("Signature 1 range", &val))
    {
        m_blobs->m_clut.setSigRange(1, val.toFloat());
        relut = true;
    }
    if (pixyParameterChanged("Signature 2 range", &val))
    {
        m_blobs->m_clut.setSigRange(2, val.toFloat());
        relut = true;
    }
    if (pixyParameterChanged("Signature 3 range", &val))
    {
        m_blobs->m_clut.setSigRange(3, val.toFloat());
        relut = true;
    }
    if (pixyParameterChanged("Signature 4 range", &val))
    {
        m_blobs->m_clut.setSigRange(4, val.toFloat());
        relut = true;
    }
    if (pixyParameterChanged("Signature 5 range", &val))
    {
        m_blobs->m_clut.setSigRange(5, val.toFloat());
        relut = true;
    }
    if (pixyParameterChanged("Signature 6 range", &val))
    {
        m_blobs->m_clut.setSigRange(6, val.toFloat());
        relut = true;
    }
    if (pixyParameterChanged("Signature 7 range", &val))
    {
        m_blobs->m_clut.setSigRange(7, val.toFloat());
        relut = true;
    }
    if (pixyParameterChanged("Min brightness", &val))
    {
        m_blobs->m_clut.setMinBrightness(val.toFloat());
        relut = true;
    }

    if (relut)
        m_blobs->m_clut.generateLUT();

    uint16_t maxBlobs, maxBlobsPerSig;
    uint32_t minArea;
    ColorCodeMode ccMode;

    maxBlobs = pixyParameter("Max blocks").toUInt();
    maxBlobsPerSig = pixyParameter("Max blocks per signature").toUInt();
    minArea = pixyParameter("Min block area").toUInt();
    ccMode = (ColorCodeMode)pixyParameter("Color code mode").toUInt();
    m_blobs->setParams(maxBlobs, maxBlobsPerSig, minArea, ccMode);

    if (pixymonParameterChanged("Cooked render mode", &val))
        m_renderMode = val.toUInt();
}

void CccModule::handleLine(uint8_t *line, uint16_t width)
{
    uint32_t index, sig, sig2, usum, vsum, ysum;
    int32_t x, r, g1, g2, b, u, v, u0, v0;

    // new line
    m_qq->enqueue(Qval());
    x = 1;

next:
    usum = vsum = ysum = 0;
    r = line[x];
    g1 = line[x-1];
    g2 = line[x-width];
    b = line[x-width-1];
    u = r-g1;
    v = b-g2;
    ysum += r + (g1+g2)/2 + b;
    usum += u;
    vsum += v;

    u0 = u>>(9-CL_LUT_COMPONENT_SCALE);
    v0 = v>>(9-CL_LUT_COMPONENT_SCALE);
    u0 &= (1<<CL_LUT_COMPONENT_SCALE)-1;
    v0 &= (1<<CL_LUT_COMPONENT_SCALE)-1;
    index = (u0<<CL_LUT_COMPONENT_SCALE) | v0;
    sig = m_lut[index];

    x += 2;
    if (x>=width)
        return;

    if (sig==0)
        goto next;

    r = line[x];
    g1 = line[x-1];
    g2 = line[x-width];
    b = line[x-width-1];
    u = r-g1;
    v = b-g2;
    ysum += r + (g1+g2)/2 + b;
    usum += u;
    vsum += v;

    u0 = u>>(9-CL_LUT_COMPONENT_SCALE);
    v0 = v>>(9-CL_LUT_COMPONENT_SCALE);
    u0 &= (1<<CL_LUT_COMPONENT_SCALE)-1;
    v0 &=(1<<CL_LUT_COMPONENT_SCALE)-1;
    index = (u0<<CL_LUT_COMPONENT_SCALE) | v0;
    sig2 = m_lut[index];

    x += 2;
    if (x>=width)
        return;

    if (sig==sig2)
        goto save;

    goto next;

save:
    m_qq->enqueue(Qval(usum, vsum, ysum, (x/2<<3) | sig));
    x += 2;
    if (x>=width)
        return;
    goto next;
}


void CccModule::rls(const Frame8 *frame)
{
    uint32_t y;

    for (y=1; y<(uint32_t)frame->m_height; y+=2)
        handleLine(frame->m_pixels+y*frame->m_width, frame->m_width);

     // indicate end of frame
    m_qq->enqueue(Qval(0, 0, 0, 0xffff));
}

int CccModule::renderCMV2(uint8_t renderFlags, uint32_t sigLen, uint8_t *sigs, uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame)
{
    int i;
    uint32_t crc, numBlobs, numCCBlobs;
    BlobA *blobs;
    BlobB *ccBlobs;
    uint32_t numQvals, *qVals;

    // create checksum for color signatures
    crc = Chirp::calcCrc(sigs, sigLen);

    if (crc!=m_crc)
    { // update!
        m_crc = crc;
        memcpy(m_blobs->m_clut.m_signatures, sigs, sizeof(ColorLUT::m_signatures));
        for (i=1; i<=CL_NUM_SIGNATURES; i++)
            m_blobs->m_clut.updateSignature(i);
        m_blobs->m_clut.generateLUT();
    }

    Frame8 rawFrame(frame, width, height);
    rls(&rawFrame);
    m_blobs->blobify();
    m_blobs->getBlobs(&blobs, &numBlobs, &ccBlobs, &numCCBlobs);
    m_blobs->getRunlengths(&qVals, &numQvals);


    m_renderer->renderBA81(RENDER_FLAG_BLEND, width, height, frameLen, frame);
    if (m_renderMode==0)
        m_renderer->renderCCB2(RENDER_FLAG_BLEND | RENDER_FLAG_FLUSH, width/2, height/2, numBlobs*sizeof(BlobA)/sizeof(uint16_t), (uint16_t *)blobs, numCCBlobs*sizeof(BlobB)/sizeof(uint16_t), (uint16_t *)ccBlobs);
    else if (m_renderMode==1)
        m_renderer->renderCCQ1(RENDER_FLAG_BLEND | RENDER_FLAG_FLUSH, width/2, height/2, numQvals, qVals);
    else if (m_renderMode==2)
    {
        m_renderer->renderCCQ1(RENDER_FLAG_BLEND, width/2, height/2, numQvals, qVals);
        m_renderer->renderCCB2(RENDER_FLAG_BLEND | RENDER_FLAG_FLUSH, width/2, height/2, numBlobs*sizeof(BlobA)/sizeof(uint16_t), (uint16_t *)blobs, numCCBlobs*sizeof(BlobB)/sizeof(uint16_t), (uint16_t *)ccBlobs);
    }
    return 0;
}


