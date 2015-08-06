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

#include <QDebug>
#include "interpreter.h"
#include "renderer.h"
#include "cblobmodule.h"
#include "dataexport.h"

// declare module
//MON_MODULE(CBlobModule);


CBlobModule::CBlobModule(Interpreter *interpreter) : MonModule(interpreter)
{
    QStringList scriptlet;

    m_qq = new Qqueue2();
    m_lut = new uint8_t[LUT_SIZE];
    m_qvals = new uint32_t[320*200/3];
    m_numQvals = 0;
    m_cblob = new ColorBlob(m_lut);

    scriptlet << "cam_getFrame 0x21 0 0 320 200";
    scriptlet << "newsigPoint 1";
    //scriptlet << "runprogArg 8 100";
    m_interpreter->emitActionScriptlet("Create new signature point 1...", scriptlet);
    scriptlet.clear();
    scriptlet << "cam_getFrame 0x21 0 0 320 200";
    scriptlet << "newsigPoint 2";
    //scriptlet << "runprogArg 8 100";
    m_interpreter->emitActionScriptlet("Create new signature point 2...", scriptlet);
    scriptlet.clear();
    scriptlet << "cam_getFrame 0x21 0 0 320 200";
    scriptlet << "newsigPoint 3";
    //scriptlet << "runprogArg 8 100";
    m_interpreter->emitActionScriptlet("Create new signature point 3...", scriptlet);
    scriptlet.clear();
    scriptlet << "cam_getFrame 0x21 0 0 320 200";
    scriptlet << "newsigPoint 4";
    //scriptlet << "runprogArg 8 100";
    m_interpreter->emitActionScriptlet("Create new signature point 4...", scriptlet);
    scriptlet.clear();
    scriptlet << "cam_getFrame 0x21 0 0 320 200";
    scriptlet << "newsigPoint 5";
    //scriptlet << "runprogArg 8 100";
    m_interpreter->emitActionScriptlet("Create new signature point 5...", scriptlet);
    scriptlet.clear();
    scriptlet << "cam_getFrame 0x21 0 0 320 200";
    scriptlet << "newsigPoint 6";
    //scriptlet << "runprogArg 8 100";
    m_interpreter->emitActionScriptlet("Create new signature point 6...", scriptlet);
    scriptlet.clear();
    scriptlet << "cam_getFrame 0x21 0 0 320 200";
    scriptlet << "newsigPoint 7";
    //scriptlet << "runprogArg 8 100";
    m_interpreter->emitActionScriptlet("Create new signature point 7...", scriptlet);
    scriptlet.clear();
    scriptlet << "cam_getFrame 0x21 0 0 320 200";
    scriptlet << "newsigArea 1";
    scriptlet << "runprogArg 8 100";
    m_interpreter->emitActionScriptlet("Create new signature 1...", scriptlet);
    scriptlet.clear();
    scriptlet << "cam_getFrame 0x21 0 0 320 200";
    scriptlet << "newsigArea 2";
    scriptlet << "runprogArg 8 100";
    m_interpreter->emitActionScriptlet("Create new signature 2...", scriptlet);
    scriptlet.clear();
    scriptlet << "cam_getFrame 0x21 0 0 320 200";
    scriptlet << "newsigArea 3";
    scriptlet << "runprogArg 8 100";
    m_interpreter->emitActionScriptlet("Create new signature 3...", scriptlet);
    scriptlet.clear();
    scriptlet << "cam_getFrame 0x21 0 0 320 200";
    scriptlet << "newsigArea 4";
    scriptlet << "runprogArg 8 100";
    m_interpreter->emitActionScriptlet("Create new signature 4...", scriptlet);
    scriptlet.clear();
    scriptlet << "cam_getFrame 0x21 0 0 320 200";
    scriptlet << "newsigArea 5";
    scriptlet << "runprogArg 8 100";
    m_interpreter->emitActionScriptlet("Create new signature 5...", scriptlet);
    scriptlet.clear();
    scriptlet << "cam_getFrame 0x21 0 0 320 200";
    scriptlet << "newsigArea 6";
    scriptlet << "runprogArg 8 100";
    m_interpreter->emitActionScriptlet("Create new signature 6...", scriptlet);
    scriptlet.clear();
    scriptlet << "cam_getFrame 0x21 0 0 320 200";
    scriptlet << "newsigArea 7";
    scriptlet << "runprogArg 8 100";
    m_interpreter->emitActionScriptlet("Create new signature 7...", scriptlet);

    m_acqRange = DEFAULT_RANGE;
    m_trackRange = 1.0f;
    m_miny = DEFAULT_MINY;
    m_yfilter = true;
    m_fixedLength = true;
    m_maxDist = MAX_DIST;
    m_minRatio = MIN_RATIO;

    m_interpreter->m_pixymonParameters->addSlider("Range", m_acqRange, 0.0f, 10.0f, "The range for identifying the colors of a signature.", "CBA");
    m_interpreter->m_pixymonParameters->addSlider("Min Y", m_miny, 0.0f, 1.0f, "Minimum brightness for a signature.", "CBA");
    m_interpreter->m_pixymonParameters->addSlider("Max distance", m_maxDist, 100, 10000, "Maximum distance when growing region.", "CBA");
    m_interpreter->m_pixymonParameters->addSlider("Min ratio", m_minRatio, 0.1f, 1.0f, "Minimum ratio.", "CBA");
    m_interpreter->m_pixymonParameters->addCheckbox("Y filter", true, "Enable Y filtering", "CBA");
    m_interpreter->m_pixymonParameters->addCheckbox("Fixed length", true, "Enable fixed length", "CBA");
    m_interpreter->m_pixymonParameters->addCheckbox("Y exp", true, "y experiment", "CBA");
    memset(m_signatures, 0, sizeof(ColorSignature)*NUM_SIGNATURES);
    memset(m_runtimeSigs, 0, sizeof(RuntimeSignature)*NUM_SIGNATURES);
}

CBlobModule::~CBlobModule()
{
    delete [] m_lut;
    delete [] m_qvals;
}

bool CBlobModule::render(uint32_t fourcc, const void *args[])
{
    if (fourcc==FOURCC('E','X','0','0'))
    {
        renderEX00(*(uint8_t *)args[0], *(uint16_t *)args[1], *(uint16_t *)args[2], *(uint32_t *)args[3], (uint8_t *)args[4]);
        return true;
    }
    else if (fourcc==FOURCC('C','C','Q','2'))
    {
        renderCCQ2(*(uint8_t *)args[0], *(uint16_t *)args[1], *(uint16_t *)args[2], *(uint32_t *)args[3], (uint8_t *)args[4]);
        return true;
    }

    return false;
}


bool CBlobModule::command(const QStringList &argv)
{
    if (argv[0]=="newsigArea" || argv[0]=="newsigPoint")
    {
        uint8_t sig;
        bool point;

        point = argv[0]=="newsigPoint";

        if (argv.size()<2)
        {
            cprintf("error: missing signature");
            return true;
        }
        sig = argv[1].toUInt();
        if (sig<1 || sig>7)
        {
            cprintf("error: signature number out of range!");
            return true;
        }

        Frame8 *frame = m_interpreter->m_renderer->backgroundRaw();

        if (point)
        {
            Point16 point;
            Points points;
            m_interpreter->getSelection(&point);
            m_cblob->generateSignature(*frame, point, &points, &m_signatures[sig-1]);
            ColorSignature s;
            Points points2;
            m_cblob->generateSignature2(*frame, point, &points2, &s);
            if (m_signatures[sig-1].m_uMin==s.m_uMin &&
                    m_signatures[sig-1].m_uMean==s.m_uMean &&
                    m_signatures[sig-1].m_uMax==s.m_uMax &&
                    m_signatures[sig-1].m_vMin==s.m_vMin &&
                    m_signatures[sig-1].m_vMean==s.m_vMean &&
                    m_signatures[sig-1].m_vMax==s.m_vMax)
                qDebug("equal!");
            else
                qDebug("not equal!");

            m_interpreter->m_renderer->renderRects(points, GROW_INC);
        }
        else
        {
            RectA region;
            m_interpreter->getSelection(&region);

            m_interpreter->m_renderer->pixelsOut(region.m_xOffset, region.m_yOffset, region.m_width, region.m_height);
            m_cblob->generateSignature(*frame, region, &m_signatures[sig-1]);
            ColorSignature s;
            m_cblob->generateSignature2(*frame, region, &s);
            if (m_signatures[sig-1].m_uMin==s.m_uMin &&
                    m_signatures[sig-1].m_uMean==s.m_uMean &&
                    m_signatures[sig-1].m_uMax==s.m_uMax &&
                    m_signatures[sig-1].m_vMin==s.m_vMin &&
                    m_signatures[sig-1].m_vMean==s.m_vMean &&
                    m_signatures[sig-1].m_vMax==s.m_vMax)
                qDebug("equal!");
            else
                qDebug("not equal!");

        }

        updateSignatures();
        m_cblob->generateLUT(m_runtimeSigs);
        uploadLut();

        DataExport dx(m_interpreter->m_pixymonParameters->value("Document folder").toString(), "lut", ET_MATLAB);

        dx.startArray(1, "lut");

        for (int i=0; i<LUT_SIZE; i++)
            dx.addElement(m_lut[i]);

        return true;
    }
    return false;
}

void CBlobModule::paramChange()
{
    m_acqRange = m_interpreter->m_pixymonParameters->value("Range").toFloat();
    m_miny = m_interpreter->m_pixymonParameters->value("Min Y").toFloat();
    m_minRatio = m_interpreter->m_pixymonParameters->value("Min ratio").toFloat();
    m_maxDist = m_interpreter->m_pixymonParameters->value("Max distance").toUInt();
    m_yfilter = m_interpreter->m_pixymonParameters->value("Y filter").toBool();
    m_fixedLength = m_interpreter->m_pixymonParameters->value("Fixed length").toBool();
    m_yexp = m_interpreter->m_pixymonParameters->value("Y exp").toBool();
    m_cblob->setParameters(m_acqRange, m_yexp ? 0.0f : m_miny, m_maxDist, m_minRatio);
    updateSignatures();
    m_cblob->generateLUT(m_runtimeSigs);
}

int CBlobModule::uploadLut()
{
    return 0;
    uint32_t i, sum;
    uint32_t responseInt;

    for (i=0, sum=0; i<LUT_SIZE; i++)
        sum += m_lut[i];
    qDebug() << sum;
    ChirpProc setmem = m_interpreter->m_chirp->getProc("cc_setMemory");
    for (i=0; i<LUT_SIZE; i+=0x100)
        m_interpreter->m_chirp->callSync(setmem, UINT32(0x10080000+0x12000-LUT_SIZE+i), UINTS8(0x100, m_lut+i), END_OUT_ARGS, &responseInt, END_IN_ARGS);

    return 0;
}

void CBlobModule::handleLine(uint8_t *line, uint16_t width)
{
    uint32_t index, sig, sig2, usum, vsum, ysum;
    int32_t x, r, g1, g2, b, u, v, u0, v0;

    // new line
    m_qq->enqueue(Qval2());
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

    u0 = u>>(9-LUT_COMPONENT_SCALE);
    v0 = v>>(9-LUT_COMPONENT_SCALE);
    u0 &= (1<<LUT_COMPONENT_SCALE)-1;
    v0 &= (1<<LUT_COMPONENT_SCALE)-1;
    index = (u0<<LUT_COMPONENT_SCALE) | v0;
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

    u0 = u>>(9-LUT_COMPONENT_SCALE);
    v0 = v>>(9-LUT_COMPONENT_SCALE);
    u0 &= (1<<LUT_COMPONENT_SCALE)-1;
    v0 &=(1<<LUT_COMPONENT_SCALE)-1;
    index = (u0<<LUT_COMPONENT_SCALE) | v0;
    sig2 = m_lut[index];

    x += 2;
    if (x>=width)
        return;

    if (sig==sig2)
        goto save;

    goto next;

save:
    m_qq->enqueue(Qval2(usum, vsum, ysum, (x/2<<3) | sig));
    x += 2;
    if (x>=width)
        return;
    goto next;
}


void CBlobModule::rls(const Frame8 *frame)
{
    uint32_t y;

    for (y=1; y<(uint32_t)frame->m_height; y+=2)
        handleLine(frame->m_pixels+y*frame->m_width, frame->m_width);

     // indicate end of frame
    m_qq->enqueue(Qval2(0, 0, 0, 0xffff));
}

#define m_minArea        25
#define m_acqCount       3
#define m_reacqWindow    15

BlobA m_pb;


void CBlobModule::processBlobs(BlobA *blobs, uint32_t *numBlobs)
{
    uint32_t i, area;

    if (*numBlobs>0)
    {
      m_pb = blobs[0];
      if (m_pb.m_left>m_reacqWindow)
          m_pb.m_left -= m_reacqWindow;
      else
          m_pb.m_left = 0;

      if (m_pb.m_top>m_reacqWindow)
          m_pb.m_top -= m_reacqWindow;
      else
          m_pb.m_top = 0;

      m_pb.m_right += m_reacqWindow;
      if (m_pb.m_right>320)
          m_pb.m_right = 320;

      m_pb.m_bottom += m_reacqWindow;
      if (m_pb.m_bottom>200)
          m_pb.m_bottom = 200;

    }
#if 0
    for (i=0; i<*numBlobs; i++)
    {
        area = (blobs[i].m_bottom - blobs[i].m_top + 1)*(blobs[i].m_right - blobs[i].m_left);
       // if (area>=m_minArea)

    }
#endif
}

void CBlobModule::renderEX00(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame)
{
    uint32_t numBlobs, numCCBlobs;
    BlobA *blobs;
    BlobB *ccBlobs;
    Frame8 frame8(frame, width, height);
    rls(&frame8);
    rla();
    m_blobs.blobify();
    m_blobs.getBlobs(&blobs, &numBlobs, &ccBlobs, &numCCBlobs);
    processBlobs(blobs, &numBlobs);
    m_interpreter->m_renderer->renderBA81(RENDER_FLAG_BLEND, width, height, frameLen, frame);
    m_interpreter->m_renderer->renderCCQ1(RENDER_FLAG_BLEND, width/2, height/2, m_numQvals, m_qvals);
    //m_interpreter->m_renderer->renderCCB2(RENDER_FLAG_BLEND | RENDER_FLAG_FLUSH, width/2, height/2, numBlobs*sizeof(BlobA)/sizeof(uint16_t), (uint16_t *)blobs, numCCBlobs*sizeof(BlobB)/sizeof(uint16_t), (uint16_t *)ccBlobs);
}

void CBlobModule::renderCCQ2(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame)
{
    qDebug("ccq2 %d", frameLen);
#if 1
    rla(frame, frameLen);
#if 0
    m_blobs.blobify();
    m_blobs.getBlobs(&blobs, &numBlobs, &ccBlobs, &numCCBlobs);
    processBlobs(blobs, &numBlobs);
#endif
    m_interpreter->m_renderer->renderCCQ1(renderFlags, width, height, m_numQvals, m_qvals);
//    m_interpreter->m_renderer->renderCCB2(renderFlags, width/2, height/2, numBlobs*sizeof(BlobA)/sizeof(uint16_t), (uint16_t *)blobs, numCCBlobs*sizeof(BlobB)/sizeof(uint16_t), (uint16_t *)ccBlobs);
#endif
}

void CBlobModule::handleSegment(uint8_t signature, uint16_t row, uint16_t startCol, uint16_t length, bool blobs)
{
    uint32_t qval;

    qval = signature;
    qval |= startCol<<3;
    qval |= length<<12;

    if (blobs)
        m_blobs.addSegment(signature, row, startCol, startCol+length);
    m_qvals[m_numQvals++] = qval;
}

void CBlobModule::updateSignatures()
{
    int signature;
    float c;

    for (signature=0; signature<NUM_SIGNATURES; signature++)
    {
        //c = ((float)m_signatures[signature].m_uMax + m_signatures[signature].m_uMin)/2.0f;
        c = m_signatures[signature].m_uMean;
        m_runtimeSigs[signature].m_uMin = c + (m_signatures[signature].m_uMin - c)*m_acqRange;
        m_runtimeSigs[signature].m_uMax = c + (m_signatures[signature].m_uMax - c)*m_acqRange;
        //c = ((float)m_signatures[signature].m_vMax + m_signatures[signature].m_vMin)/2.0f;
        c = m_signatures[signature].m_vMean;
        m_runtimeSigs[signature].m_vMin = c + (m_signatures[signature].m_vMin - c)*m_acqRange;
        m_runtimeSigs[signature].m_vMax = c + (m_signatures[signature].m_vMax - c)*m_acqRange;
    }
#if 0
    for (signature=0; signature<NUM_SIGNATURES; signature++)
    {
        uint32_t responseInt;

        ChirpProc sigBounds = m_interpreter->m_chirp->getProc("cc_setSigBounds");
        m_interpreter->m_chirp->callSync(sigBounds,
                                         INT8(signature),
                                         INT16(m_runtimeSigs[signature].m_uMin),
                                         INT16(m_runtimeSigs[signature].m_uMax),
                                         INT16(m_signatures[signature].m_uMean),
                                         INT16(m_runtimeSigs[signature].m_vMin),
                                         INT16(m_runtimeSigs[signature].m_vMax),
                                         INT16(m_signatures[signature].m_vMean),
                                         END_OUT_ARGS, &responseInt, END_IN_ARGS);
    }
#endif
}

void CBlobModule::rla()
{
    int32_t row;
    uint32_t i, startCol, sig, prevSig, prevStartCol, segmentStartCol, segmentEndCol, segmentSig=0;
    bool merge;
    Qval2 qval;
    int32_t u, v, c, miny;
    qlonglong m_usum, m_vsum;
    uint32_t m_numuv;

    m_usum = m_vsum = m_numuv = 0;
    miny = 3*((1<<8)-1)*m_miny;

    m_numQvals = 0;
    for (i=0, row=-1; m_qq->dequeue(&qval); i++)
    {
        if (qval.m_col==0xffff)
        {
            m_qvals[m_numQvals++] = 0xffffffff;
            m_blobs.endFrame();
            continue;
        }
        if (qval.m_col==0)
        {
            prevStartCol = 0xffff;
            prevSig = 0;
            if (segmentSig)
            {
                handleSegment(segmentSig, row, segmentStartCol-2, segmentEndCol - segmentStartCol+2);
                segmentSig = 0;
            }
            row++;
            m_qvals[m_numQvals++] = 0;
            continue;
        }

        sig = qval.m_col&0x07;
        qval.m_col >>= 3;
        startCol = qval.m_col;

        u = qval.m_u;
        v = qval.m_v;

        u <<= LUT_ENTRY_SCALE;
        v <<= LUT_ENTRY_SCALE;
        c = qval.m_y;
        if (c==0)
            c = 1;
        u /= c;
        v /= c;

        //   yexp min  add
        //     1   1    1
        //     1   0    0
        //     0   x    1

        if (!m_yfilter || (!(m_yexp && c<miny) &&
                m_runtimeSigs[sig-1].m_uMin<u && u<m_runtimeSigs[sig-1].m_uMax && m_runtimeSigs[sig-1].m_vMin<v && v<m_runtimeSigs[sig-1].m_vMax))
        {
            if (m_pb.m_left<startCol && startCol<m_pb.m_right && m_pb.m_top<row && row<m_pb.m_bottom)
            {
                m_numuv++;
                m_usum += u;
                m_vsum += v;
            }


            merge = startCol-prevStartCol<=4 && prevSig==sig;
            if (segmentSig==0 && merge)
            {
                segmentSig = sig;
                segmentStartCol = prevStartCol;
            }
            else if (segmentSig!=0 && (segmentSig!=sig || !merge))
            {
                handleSegment(segmentSig, row, segmentStartCol-2, segmentEndCol - segmentStartCol+2);
                segmentSig = 0;
            }

            if (segmentSig!=0 && merge)
                segmentEndCol = startCol;
            else if (segmentSig==0 && !merge)
                handleSegment(sig, row, startCol-2, 2);
            prevSig = sig;
            prevStartCol = startCol;
        }
        else if (segmentSig!=0)
        {
            handleSegment(segmentSig, row, segmentStartCol-2, segmentEndCol - segmentStartCol+2);
            segmentSig = 0;
        }

    }
    if (m_numuv && m_fixedLength)
    {
        int32_t uavg, vavg;
        int32_t urange, vrange;

        urange = (m_runtimeSigs[0].m_uMax - m_runtimeSigs[0].m_uMin);
        vrange = (m_runtimeSigs[0].m_vMax - m_runtimeSigs[0].m_vMin);
        uavg = m_usum/m_numuv;
        vavg = m_vsum/m_numuv;

        m_runtimeSigs[0].m_uMax = uavg + urange/2;
        m_runtimeSigs[0].m_uMin = uavg - urange/2;
        m_runtimeSigs[0].m_vMax = vavg + vrange/2;
        m_runtimeSigs[0].m_vMin = vavg - vrange/2;

        qDebug("*** %d %d", uavg, vavg);
    }
}

void CBlobModule::rla(uint8_t *qmem, uint32_t qmemSize)
{
    int32_t row;
    uint32_t i, startCol, sig, prevSig, prevStartCol, segmentStartCol, segmentEndCol, segmentSig=0;
    bool merge;
    Qval3 *qval;
    int32_t u, v, c;
    qlonglong m_usum, m_vsum;
    uint32_t m_numuv;

    m_usum = m_vsum = m_numuv = 0;

    m_numQvals = 0;
    for (i=0, row=-1; i<qmemSize; i+=sizeof(Qval3))
    {
        qval = (Qval3 *)(qmem+i);
        if (qval->m_col==0xffff)
        {
#if 0
            m_qvals[m_numQvals++] = 0xffffffff;
            m_blobs.endFrame();
#endif
            continue;
        }
        if (qval->m_col==0)
        {
            prevStartCol = 0xffff;
            prevSig = 0;
            if (segmentSig)
            {
                handleSegment(segmentSig, row, segmentStartCol-1, segmentEndCol - segmentStartCol+1, false);
                segmentSig = 0;
            }
            row++;
            m_qvals[m_numQvals++] = 0;
            continue;
        }

        sig = qval->m_col&0x07;
        qval->m_col >>= 3;
        startCol = qval->m_col;

        u = qval->m_u;
        v = qval->m_v;

        u <<= LUT_ENTRY_SCALE;
        v <<= LUT_ENTRY_SCALE;
        c = qval->m_y;
        if (c==0)
            c = 1;
        u /= c;
        v /= c;

        if (!m_yfilter ||
                (m_runtimeSigs[sig-1].m_uMin<u && u<m_runtimeSigs[sig-1].m_uMax && m_runtimeSigs[sig-1].m_vMin<v && v<m_runtimeSigs[sig-1].m_vMax))
        {
            if (m_pb.m_left<startCol && startCol<m_pb.m_right && m_pb.m_top<row && row<m_pb.m_bottom)
            {
                m_numuv++;
                m_usum += u;
                m_vsum += v;
            }


            merge = startCol-prevStartCol<=4 && prevSig==sig;
            if (segmentSig==0 && merge)
            {
                segmentSig = sig;
                segmentStartCol = prevStartCol;
            }
            else if (segmentSig!=0 && (segmentSig!=sig || !merge))
            {
                handleSegment(segmentSig, row, segmentStartCol-1, segmentEndCol - segmentStartCol+1, false);
                segmentSig = 0;
            }

            if (segmentSig!=0 && merge)
                segmentEndCol = startCol;
            else if (segmentSig==0 && !merge)
                handleSegment(sig, row, startCol-1, 2, false);
            prevSig = sig;
            prevStartCol = startCol;
        }
        else if (segmentSig!=0)
        {
            handleSegment(segmentSig, row, segmentStartCol-1, segmentEndCol - segmentStartCol+1, false);
            segmentSig = 0;
        }
    }
}


Qqueue2::Qqueue2()
{
#ifdef PIXY
    m_fields = (QqueueFields2 *)QQ_LOC;
#else
    m_fields = (QqueueFields2 *)(new uint8_t[QQ_SIZE]);
#endif
    memset((void *)m_fields, 0, sizeof(QqueueFields2));
}

Qqueue2::~Qqueue2()
{
#ifdef PIXY
#else
    delete [] m_fields;
#endif
}

uint32_t Qqueue2::dequeue(Qval2 *val)
{
    uint16_t len = m_fields->produced - m_fields->consumed;
    if (len)
    {
        *val = m_fields->data[m_fields->readIndex++];
        m_fields->consumed++;
        if (m_fields->readIndex==QQ_MEM_SIZE)
            m_fields->readIndex = 0;
        return 1;
    }
    return 0;
}

int Qqueue2::enqueue(const Qval2 &val)
{
    uint16_t len = m_fields->produced - m_fields->consumed;
    uint16_t freeLen = 	QQ_MEM_SIZE-len;
    if (freeLen>0)
    {
        m_fields->data[m_fields->writeIndex++] = val;
        m_fields->produced++;
        if (m_fields->writeIndex==QQ_MEM_SIZE)
            m_fields->writeIndex = 0;
        return 1;
    }
    return 0;

}


uint32_t Qqueue2::readAll(Qval2 *mem, uint32_t size)
{
    uint16_t len = m_fields->produced - m_fields->consumed;
    uint16_t i, j;

    for (i=0, j=m_fields->readIndex; i<len && i<size; i++)
    {
        mem[i] = m_fields->data[j++];
        if (j==QQ_MEM_SIZE)
            j = 0;
    }
    // flush the rest
    m_fields->consumed += len;
    m_fields->readIndex += len;
    if (m_fields->readIndex>=QQ_MEM_SIZE)
        m_fields->readIndex -= QQ_MEM_SIZE;

    return i;
}

void Qqueue2::flush()
{
    uint16_t len = m_fields->produced - m_fields->consumed;

    m_fields->consumed += len;
    m_fields->readIndex += len;
    if (m_fields->readIndex>=QQ_MEM_SIZE)
        m_fields->readIndex -= QQ_MEM_SIZE;
}




