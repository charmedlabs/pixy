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

#include <QPainter>
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
    int i;

    m_crc = 0;
    m_qq = new Qqueue();
    m_lut = new uint8_t[CL_LUT_SIZE];
    m_blobs = new Blobs(m_qq, m_lut);

    Parameter rm("Cooked render mode", PT_INT8, 2, "Cooked video rendering mode.");
    rm.addRadioValue(RadioValue("Boxes only", 0));
    rm.addRadioValue(RadioValue("Filtered Pixels", 1));
    rm.addRadioValue(RadioValue("Boxes and filtered pixels", 2));
    m_interpreter->m_pixymonParameters->add(rm);

    m_renderMode = pixymonParameter("Cooked render mode").toUInt();

    for (i=0; i<CL_NUM_SIGNATURES; i++)
        m_palette[i] = Qt::black;
}

CccModule::~CccModule()
{
    delete m_blobs;
    delete [] m_lut;
    delete m_qq;
}

bool CccModule::render(uint32_t fourcc, const void *args[])
{
    if (fourcc==FOURCC('C', 'C', 'B', '1'))
    {
        renderCCB1(*(uint8_t *)args[0], *(uint16_t *)args[1], *(uint32_t *)args[2], *(uint32_t *)args[3], (uint16_t *)args[4]);
        return true;
    }
    else if (fourcc==FOURCC('C', 'C', 'B', '2'))
    {
        renderCCB2(*(uint8_t *)args[0], *(uint16_t *)args[1], *(uint32_t *)args[2], *(uint32_t *)args[3], (uint16_t *)args[4], *(uint32_t *)args[5], (uint16_t *)args[6]);
        return true;
    }
    else if (fourcc==FOURCC('C','M','V','2'))
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

uint16_t convert10to8(uint32_t signum)
{
    uint16_t res=0;
    uint32_t q;

    q = signum/10000;
    if (q)
    {
        res += q*8*8*8*8;
        signum -= q*10000;
    }
    q = signum/1000;
    if (q)
    {
        res += q*8*8*8;
        signum -= q*1000;
    }
    q = signum/100;
    if (q)
    {
        res += q*8*8;
        signum -= q*100;
    }
    q = signum/10;
    if (q)
    {
        res += q*8;
        signum -= q*10;
    }
    if (signum)
        res += signum;

    return res;
}

void CccModule::paramChange()
{
    int i;
    QVariant val;
    bool relut = false;
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
            m_palette[i] = m_blobs->m_clut.m_signatures[i].m_rgb;
        m_renderer->setPalette(m_palette);
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

    // create label dictionary
    Parameters &params = m_interpreter->m_pixyParameters.parameters();
    QStringList words;
    uint32_t signum;
    m_labels.clear();
    // go through all parameters and find labels
    for (i=0; i<params.length(); i++)
    {
        if (params[i].id().startsWith("Signature label"))
        {
            words = params[i].id().split(QRegExp("\\s+"));
            if (words.length()<3) // bogus!
                continue;
            signum = words[2].toUInt();
            m_labels.push_back(QPair<uint16_t, QString>(convert10to8(signum), params[i].value().toString().remove(QRegExp("^\\s+")))); // remove leading whitespace
        }
    }
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

QString CccModule::lookup(uint16_t signum)
{
    int i;

    for (i=0; i<m_labels.length(); i++)
    {
        if (m_labels[i].first==signum)
            return m_labels[i].second;
    }
    return "";
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
        renderCCB2(RENDER_FLAG_BLEND | renderFlags, width/2, height/2, numBlobs*sizeof(BlobA)/sizeof(uint16_t), (uint16_t *)blobs, numCCBlobs*sizeof(BlobB)/sizeof(uint16_t), (uint16_t *)ccBlobs);
    else if (m_renderMode==1)
        m_renderer->renderCCQ1(RENDER_FLAG_BLEND | renderFlags, width/2, height/2, numQvals, qVals);
    else if (m_renderMode==2)
    {
        m_renderer->renderCCQ1(RENDER_FLAG_BLEND, width/2, height/2, numQvals, qVals);
        renderCCB2(RENDER_FLAG_BLEND | renderFlags, width/2, height/2, numBlobs*sizeof(BlobA)/sizeof(uint16_t), (uint16_t *)blobs, numCCBlobs*sizeof(BlobB)/sizeof(uint16_t), (uint16_t *)ccBlobs);
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

int CccModule::renderCCB1(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t numBlobs, uint16_t *blobs)
{
    float scale = (float)m_renderer->m_video->activeWidth()/width;
    QImage img(width*scale, height*scale, QImage::Format_ARGB32);

    if (renderFlags&RENDER_FLAG_BLEND) // if we're blending, we should be transparent
        img.fill(0x00000000);
    else
        img.fill(0xff000000); // otherwise, we're just black

    numBlobs /= sizeof(BlobA)/sizeof(uint16_t);
    renderBlobsA(renderFlags&RENDER_FLAG_BLEND, &img, scale, (BlobA *)blobs, numBlobs);

    m_renderer->emitImage(img, renderFlags);

    return 0;
}

int CccModule::renderCCB2(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t numBlobs, uint16_t *blobs, uint32_t numCCBlobs, uint16_t *ccBlobs)
{
    float scale = (float)m_renderer->m_video->activeWidth()/width;
    QImage img(width*scale, height*scale, QImage::Format_ARGB32);

    if (renderFlags&RENDER_FLAG_BLEND) // if we're blending, we should be transparent
        img.fill(0x00000000);
    else
        img.fill(0xff000000); // otherwise, we're just black

    numBlobs /= sizeof(BlobA)/sizeof(uint16_t);
    numCCBlobs /= sizeof(BlobB)/sizeof(uint16_t);
    renderBlobsA(renderFlags&RENDER_FLAG_BLEND, &img, scale, (BlobA *)blobs, numBlobs);
    renderBlobsB(renderFlags&RENDER_FLAG_BLEND, &img, scale, (BlobB *)ccBlobs, numCCBlobs);

    m_renderer->emitImage(img, renderFlags);

    return 0;
}

void CccModule::renderBlobsA(bool blend, QImage *image, float scale, BlobA *blobs, uint32_t numBlobs)
{
    QPainter p;
    QString str;
    uint16_t left, right, top, bottom;
    uint i;

    p.begin(image);
    if (blend)
        p.setBrush(QBrush(QColor(0xff, 0xff, 0xff, 0x20)));
    p.setPen(QPen(QColor(0xff, 0xff, 0xff, 0xff)));

#ifdef __MACOS__
    QFont font("verdana", 18);
#else
    QFont font("verdana", 12);
#endif
    p.setFont(font);
    for (i=0; i<numBlobs; i++)
    {
        left = scale*blobs[i].m_left;
        right = scale*blobs[i].m_right;
        top = scale*blobs[i].m_top;
        bottom = scale*blobs[i].m_bottom;

        //DBG("%d %d %d %d", left, right, top, bottom);
        if (!blend)
            p.setBrush(QBrush(QRgb(m_palette[blobs[i].m_model-1])));
        p.drawRect(left, top, right-left, bottom-top);
        if (blobs[i].m_model)
        {
            if ((str=lookup(blobs[i].m_model))=="")
                str = str.sprintf("s=%d", blobs[i].m_model);
            p.setPen(QPen(QColor(0, 0, 0, 0xff)));
            p.drawText(left+1, top+1, str);
            p.setPen(QPen(QColor(0xff, 0xff, 0xff, 0xff)));
            p.drawText(left, top, str);
        }
    }
    p.end();
}


void CccModule::renderBlobsB(bool blend, QImage *image, float scale, BlobB *blobs, uint32_t numBlobs)
{
    QPainter p;
    QString str, modelStr;
    uint16_t left, right, top, bottom;
    uint i;

    p.begin(image);
    p.setBrush(QBrush(QColor(0xff, 0xff, 0xff, 0x40)));
    p.setPen(QPen(QColor(0xff, 0xff, 0xff, 0xff)));

#ifdef __MACOS__
    QFont font("verdana", 18);
#else
    QFont font("verdana", 12);
#endif
    p.setFont(font);
    for (i=0; i<numBlobs; i++)
    {
        left = scale*blobs[i].m_left;
        right = scale*blobs[i].m_right;
        top = scale*blobs[i].m_top;
        bottom = scale*blobs[i].m_bottom;

        //DBG("%d %d %d %d", left, right, top, bottom);
        p.drawRect(left, top, right-left, bottom-top);
        if (blobs[i].m_model)
        {
            if ((str=lookup(blobs[i].m_model))=="")
            {
                modelStr = QString::number(blobs[i].m_model, 8);
                str = "s=" + modelStr + ", " + QChar(0xa6, 0x03) + "=" + QString::number(blobs[i].m_angle);
            }
            else
                str += QString(", ") + QChar(0xa6, 0x03) + "=" + QString::number(blobs[i].m_angle);
            p.setPen(QPen(QColor(0, 0, 0, 0xff)));
            p.drawText(left+1, top+1, str);
            p.setPen(QPen(QColor(0xff, 0xff, 0xff, 0xff)));
            p.drawText(left, top, str);
        }
    }
    p.end();
}

