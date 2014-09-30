#include <QDebug>
#include "interpreter.h"
#include "renderer.h"
#include "cblobmodule.h"
#include "dataexport.h"

// declare module
MON_MODULE(CBlobModule);


CBlobModule::CBlobModule(Interpreter *interpreter) : MonModule(interpreter)
{
    QStringList scriptlet;

    m_qq = new Qqueue2();
    m_lut = new uint8_t[LUT_SIZE];
    m_qvals = new uint32_t[320*200/3];
    m_numQvals = 0;
    m_cblob = new ColorBlob(m_lut);

    scriptlet << "cam_getFrame 0x21 0 0 320 200";
    scriptlet << "newsig 1";
    scriptlet << "runprogArg 8 100";
    m_interpreter->emit actionScriptlet("Create new signature 1...", scriptlet);
    scriptlet.clear();
    scriptlet << "cam_getFrame 0x21 0 0 320 200";
    scriptlet << "newsig 2";
    scriptlet << "runprogArg 8 100";
    m_interpreter->emit actionScriptlet("Create new signature 2...", scriptlet);
    scriptlet.clear();
    scriptlet << "cam_getFrame 0x21 0 0 320 200";
    scriptlet << "newsig 3";
    scriptlet << "runprogArg 8 100";
    m_interpreter->emit actionScriptlet("Create new signature 3...", scriptlet);
    scriptlet.clear();
    scriptlet << "cam_getFrame 0x21 0 0 320 200";
    scriptlet << "newsig 4";
    scriptlet << "runprogArg 8 100";
    m_interpreter->emit actionScriptlet("Create new signature 4...", scriptlet);
    scriptlet.clear();
    scriptlet << "cam_getFrame 0x21 0 0 320 200";
    scriptlet << "newsig 5";
    scriptlet << "runprogArg 8 100";
    m_interpreter->emit actionScriptlet("Create new signature 5...", scriptlet);
    scriptlet.clear();
    scriptlet << "cam_getFrame 0x21 0 0 320 200";
    scriptlet << "newsig 6";
    scriptlet << "runprogArg 8 100";
    m_interpreter->emit actionScriptlet("Create new signature 6...", scriptlet);
    scriptlet.clear();
    scriptlet << "cam_getFrame 0x21 0 0 320 200";
    scriptlet << "newsig 7";
    scriptlet << "runprogArg 8 100";
    m_interpreter->emit actionScriptlet("Create new signature 7...", scriptlet);


    m_acqRange = DEFAULT_RANGE;
    m_trackRange = 1.0f;
    m_miny = DEFAULT_MINY;
    m_yfilter = true;
    m_fixedLength = true;

    m_interpreter->m_pixymonParameters->addSlider("Range", m_acqRange, 0.0f, 10.0f, this, "The range for identifying the colors of a signature.", "CBA");
    m_interpreter->m_pixymonParameters->addSlider("Min Y", m_miny, 0.0f, 0.5f, this, "Minimum brightness for a signature.", "CBA");
    m_interpreter->m_pixymonParameters->addBool("Y filter", true, this, "Enable Y filtering", "CBA");
    m_interpreter->m_pixymonParameters->addBool("Fixed length", true, this, "Enable fixed length", "CBA");
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
    return false;
}

bool CBlobModule::command(const QStringList &argv)
{
    if (argv.size()==2 && argv[0]=="newsig")
    {
        uint8_t sig = argv[1].toUInt();
        if (sig<1 || sig>7)
        {
            cprintf("Signature number out of range!");
            return true;
        }

        RectA region;
        m_interpreter->getSelection(VideoWidget::REGION, &region);

        Frame8 *frame = m_interpreter->m_renderer->backgroundRaw();
        m_interpreter->m_renderer->pixelsOut(region.m_xOffset, region.m_yOffset, region.m_width, region.m_height);
        m_cblob->generateSignature(frame, &region, &m_signatures[sig-1]);

        updateSignatures();
        m_cblob->generateLUT(m_runtimeSigs);

        DataExport dx(m_interpreter->m_pixymonParameters->value("Document folder")->toString(), "lut", ET_MATLAB);

        dx.startArray(1, "lut");

        for (int i=0; i<LUT_SIZE; i++)
            dx.addElement(m_lut[i]);

        return true;
    }
    return false;
}

void CBlobModule::paramChange()
{
    m_acqRange = m_interpreter->m_pixymonParameters->value("Range")->toFloat();
    m_miny = m_interpreter->m_pixymonParameters->value("Min Y")->toFloat();
    m_cblob->setParameters(m_acqRange, m_miny);
    m_yfilter = m_interpreter->m_pixymonParameters->value("Y filter")->toBool();
    m_fixedLength = m_interpreter->m_pixymonParameters->value("Fixed length")->toBool();

    m_cblob->generateLUT(m_runtimeSigs);
    updateSignatures();
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

    m_interpreter->m_renderer->renderBA81(0, width, height, frameLen, frame);
    m_interpreter->m_renderer->renderCCQ1(0, width/2, height/2, m_numQvals, m_qvals);
    m_interpreter->m_renderer->renderCCB2(renderFlags, width/2, height/2, numBlobs*sizeof(BlobA)/sizeof(uint16_t), (uint16_t *)blobs, numCCBlobs*sizeof(BlobB)/sizeof(uint16_t), (uint16_t *)ccBlobs);
}

void CBlobModule::handleSegment(uint8_t signature, uint16_t row, uint16_t startCol, uint16_t length)
{
    uint32_t qval;

    qval = signature;
    qval |= startCol<<3;
    qval |= length<<12;

    m_blobs.addSegment(signature, row, startCol, startCol+length);

    m_qvals[m_numQvals++] = qval;
}

void CBlobModule::updateSignatures()
{
    int signature;
    float c;

    for (signature=0; signature<NUM_SIGNATURES; signature++)
    {
        c = ((float)m_signatures[signature].m_uMax + m_signatures[signature].m_uMin)/2.0f;
        m_runtimeSigs[signature].m_uMin = c + (m_signatures[signature].m_uMin - c)*m_acqRange;
        m_runtimeSigs[signature].m_uMax = c + (m_signatures[signature].m_uMax - c)*m_acqRange;
        c = ((float)m_signatures[signature].m_vMax + m_signatures[signature].m_vMin)/2.0f;
        m_runtimeSigs[signature].m_vMin = c + (m_signatures[signature].m_vMin - c)*m_acqRange;
        m_runtimeSigs[signature].m_vMax = c + (m_signatures[signature].m_vMax - c)*m_acqRange;
    }
}

void CBlobModule::rla()
{
    int32_t row;
    uint32_t i, startCol, sig, prevSig, prevStartCol, segmentStartCol, segmentEndCol, segmentSig=0;
    bool merge;
    Qval2 qval;
    int32_t u, v, c;

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

        if (!m_yfilter ||
                (m_runtimeSigs[sig-1].m_uMin<u && u<m_runtimeSigs[sig-1].m_uMax && m_runtimeSigs[sig-1].m_vMin<v && v<m_runtimeSigs[sig-1].m_vMax))
        {
            merge = startCol-prevStartCol<=6 && prevSig==sig;
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




