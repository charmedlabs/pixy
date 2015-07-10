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

#include <stdio.h>
#include "cccmodule.h"
#include "interpreter.h"
#include "renderer.h"
#include "blobs.h"
#include "qqueue.h"
#include "colorlut.h"
#include "calc.h"

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

    Parameter tp("Test", PT_INT8, 0, "Test help");
    tp.addRadioValue(RadioValue("description 0", 0));
    tp.addRadioValue(RadioValue("description 1", 1));
    tp.addRadioValue(RadioValue("description 2", 2));
    tp.addRadioValue(RadioValue("description 3", 3));

    m_interpreter->m_pixymonParameters->add(tp);

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
        renderCMV2(*(uint8_t *)args[0], *(uint16_t *)args[1], *(uint16_t *)args[2], *(uint32_t *)args[3], (uint8_t *)args[4]);
        return true;
    }
    else if (fourcc==FOURCC('C','C','Q','2'))
    {
        renderCCQ2(*(uint8_t *)args[0], *(uint16_t *)args[1], *(uint16_t *)args[2], *(uint32_t *)args[3], (uint8_t *)args[4]);
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
    int i;
    QVariant val;
    bool relut = false;
    uint32_t palette[CL_NUM_SIGNATURES];
    char id[128];
    uint32_t sigLen;
    uint8_t *sigData;
    QByteArray ba;

    // check to see if any signatures have changed
    for (i=0; i<CL_NUM_SIGNATURES; i++)
    {
        sprintf(id, "signature%d", i+1);
        if (pixyParameterChanged(id, &val))
        {
            ba = val.toByteArray();
            Chirp::deserialize((uint8_t *)ba.data(), val.toByteArray().size(), &sigLen, &sigData, END);
            if (sigLen==sizeof(ColorSignature))
            {
                memcpy(m_blobs->m_clut.m_signatures+i, sigData, sizeof(ColorSignature));
                relut = true;
            }
        }
    }
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
    if (pixyParameterChanged("Color code gain", &val))
    {
        m_blobs->m_clut.setCCGain(val.toFloat());
        relut = true;
    }

    if (relut)
    {
        m_blobs->m_clut.generateLUT();
        for (i=0; i<CL_NUM_SIGNATURES; i++)
            palette[i] = m_blobs->m_clut.m_signatures[i].m_rgb;
        m_renderer->setPalette(palette);
    }

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
    Qval newline;
    // new line
    m_qq->enqueue(&newline);
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
    Qval qval(usum, vsum, ysum, (x/2<<3) | sig);
    m_qq->enqueue(&qval);
    x += 2;
    if (x>=width)
        return;
    goto next;
}


void CccModule::rls(const Frame8 *frame)
{
    uint32_t y;
    Qval eof(0, 0, 0, 0xffff);

    for (y=1; y<(uint32_t)frame->m_height; y+=2)
        handleLine(frame->m_pixels+y*frame->m_width, frame->m_width);

    // indicate end of frame
    m_qq->enqueue(&eof);
}

int CccModule::renderCMV2(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame)
{
    uint32_t numBlobs, numCCBlobs;
    BlobA *blobs;
    BlobB *ccBlobs;
    uint32_t numQvals, *qVals;

    Frame8 rawFrame(frame, width, height);
    rls(&rawFrame);
    m_blobs->blobify();
    m_blobs->getBlobs(&blobs, &numBlobs, &ccBlobs, &numCCBlobs);
    m_blobs->getRunlengths(&qVals, &numQvals);

    // render different layers...
    // starting with the background
    m_renderer->renderBA81(RENDER_FLAG_BLEND, width, height, frameLen, frame);
    // then based on the renderMode, render/blend the different layers in a stack
    if (m_renderMode==0)
        m_renderer->renderCCB2(RENDER_FLAG_BLEND | renderFlags, width/2, height/2, numBlobs*sizeof(BlobA)/sizeof(uint16_t), (uint16_t *)blobs, numCCBlobs*sizeof(BlobB)/sizeof(uint16_t), (uint16_t *)ccBlobs);
    else if (m_renderMode==1)
        m_renderer->renderCCQ1(RENDER_FLAG_BLEND | renderFlags, width/2, height/2, numQvals, qVals);
    else if (m_renderMode==2)
    {
        m_renderer->renderCCQ1(RENDER_FLAG_BLEND, width/2, height/2, numQvals, qVals);
        m_renderer->renderCCB2(RENDER_FLAG_BLEND | renderFlags, width/2, height/2, numBlobs*sizeof(BlobA)/sizeof(uint16_t), (uint16_t *)blobs, numCCBlobs*sizeof(BlobB)/sizeof(uint16_t), (uint16_t *)ccBlobs);
    }
    return 0;
}

void CccModule::renderCCQ2(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame)
{
    uint32_t i;
    Qval *qval;
    uint32_t numQvals, *qVals;

    m_blobs->m_qq->flush();
    for (i=0; i<frameLen; i+=sizeof(Qval))
    {
        qval = (Qval *)(frame+i);
        m_blobs->m_qq->enqueue(qval);
    }
    m_blobs->runlengthAnalysis();
    m_blobs->getRunlengths(&qVals, &numQvals);
    m_renderer->renderCCQ1(renderFlags, width, height, numQvals, qVals);
}


