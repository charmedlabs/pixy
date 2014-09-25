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
    m_cblob = new ColorBlob(m_lut);

    scriptlet << "cam_getFrame 0x21 0 0 320 200";
    scriptlet << "cb_region";
    scriptlet << "runprogArg 8 100";
    m_interpreter->emit actionScriptlet("Create new signature...", scriptlet);

    m_acqRange = DEFAULT_RANGE;
    m_trackRange = 1.0f;
    m_miny = DEFAULT_MINY;
    m_yfilter = true;
    m_fixedLength = true;

    m_interpreter->m_pixymonParameters->add("Range", PT_FLT32, m_acqRange, "The range for identifying the colors of a signature.", "CBA");
    m_interpreter->m_pixymonParameters->add("Min Y", PT_FLT32, m_miny, "Minimum brightness for a signature.", "CBA");
    m_interpreter->m_pixymonParameters->addBool("Y filter", true, this, "Enable Y filtering", "CBA");
    m_interpreter->m_pixymonParameters->addBool("Fixed length", true, this, "Enable fixed length", "CBA");

    memset(m_signatures, 0, sizeof(ColorSignature)*NUM_SIGNATURES);
}

CBlobModule::~CBlobModule()
{
    delete [] m_lut;
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
    if (argv[0]=="cb_region")
    {
        RectA region;
        m_interpreter->getSelection(VideoWidget::REGION, &region);

        Frame8 *frame = m_interpreter->m_renderer->backgroundRaw();
        m_interpreter->m_renderer->pixelsOut(region.m_xOffset, region.m_yOffset, region.m_width, region.m_height);
        m_cblob->generateSignature(frame, &region, &m_signatures[0]);

        m_cblob->generateLUT(&m_signatures[0], 1);

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
    m_cblob->generateLUT(&m_signatures[0], 1);
    m_yfilter = m_interpreter->m_pixymonParameters->value("Y filter")->toBool();
    m_fixedLength = m_interpreter->m_pixymonParameters->value("Fixed length")->toBool();
}

#define _LENGTH  2

void CBlobModule::rls(const Frame8 *frame)
{
    uint32_t x, y, index, sig, startCol, prevSig, qval, rgsum, bsum, count, length;
    int32_t r, g1, g2, b, u, v, u0, v0;
    bool segment;

    for (y=1; y<(uint32_t)frame->m_height; y+=2)
    {
        // new lime
        m_qq->enqueue(Qval2(0, 0, 0));

        segment = false;
        prevSig = 0;
        startCol = 0;
        for (x=1; x<(uint32_t)frame->m_width; x+=2)
        {
            r = frame->m_pixels[y*frame->m_width + x];
            g1 = frame->m_pixels[y*frame->m_width + x - 1];
            g2 = frame->m_pixels[y*frame->m_width - frame->m_width + x];
            b = frame->m_pixels[y*frame->m_width - frame->m_width + x - 1];
            u = r-g1;
            v = b-g2;

            u0 = u>>(9-LUT_COMPONENT_SCALE);
            v0 = v>>(9-LUT_COMPONENT_SCALE);
            u0 &= (1<<LUT_COMPONENT_SCALE)-1;
            v0 &=(1<<LUT_COMPONENT_SCALE)-1;
            index = (u0<<LUT_COMPONENT_SCALE) | v0;
            sig = m_lut[index];

            if (segment==false && sig)
            {
                startCol = x/2;
                rgsum = bsum = 0;
                segment = true;
                count = 0;
            }
            else if (segment && (sig!=prevSig || (m_fixedLength && count==_LENGTH)))
            {
                length = x/2-startCol;
                if (m_fixedLength==false || length==_LENGTH)
                {
                    qval = prevSig;
                    qval |= startCol<<7;
                    qval |= length<<(7+9);
                    m_qq->enqueue(Qval2(qval, rgsum, bsum));
                }
                segment = false;
                if (count!=length)
                    qDebug("*** %d %d", count, x/2-startCol);
            }
            if (segment)
            {
                rgsum += (r<<16) | g1;
                bsum += b;
                count++;
            }
            prevSig = sig;
        }

        if (segment)
        {
            qval = prevSig;
            qval |= startCol<<7;
            qval |= (x/2-startCol)<<(7+9);
            m_qq->enqueue(Qval2(qval, rgsum, bsum));
        }

    }
    // indicate end of frame
    m_qq->enqueue(Qval2(0xffffffff, 0, 0));
}

void CBlobModule::renderEX00(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame)
{
    Frame8 frame8(frame, width, height);
    rls(&frame8);
    //cprintf("qvals %d", m_qq->m_fields->produced-m_qq->m_fields->consumed);
    //m_qq->flush();

    m_interpreter->m_renderer->renderBA81(0, width, height, frameLen, frame);
    renderCCQ2(RENDER_FLAG_FLUSH, width, height, frameLen, frame);

}

void CBlobModule::renderCCQ2(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame)
{
#if 0
    int32_t row;
    uint32_t i, startCol, length;
    uint8_t model;
    QImage img(width/2, height/2, QImage::Format_ARGB32);
    unsigned int palette[] =
    {0x00000000, // 0 no model (transparent)
     0x80ff0000, // 1 red
     0x80ff4000, // 2 orange
     0x80ffff00, // 3 yellow
     0x8000ff00, // 4 green
     0x8000ffff, // 5 cyan
     0x800000ff, // 6 blue
     0x80ff00ff  // 7 violet
    };

    static int f = 0;

    // if we're a background frame, set alpha to 1.0
    if (m_interpreter->m_renderer->firstFrame())
    {
        for (i=0; i<sizeof(palette)/sizeof(unsigned int); i++)
            palette[i] |= 0xff000000;
    }

    img.fill(palette[0]);

    uint32_t x, y, index, sig;
    int32_t r, g1, g2, b, u, v, u0, v0, c;

    for (y=1; y<height; y+=2)
    {
        for (x=1; x<width; x+=2)
        {
            r = frame[y*width + x];
            g1 = frame[y*width + x - 1];
            g2 = frame[y*width - width + x];
            b = frame[y*width - width + x - 1];
            u = r-g1;
            v = b-g2;

            u0 = u>>(9-LUT_COMPONENT_SCALE);
            v0 = v>>(9-LUT_COMPONENT_SCALE);
            u0 &= (1<<LUT_COMPONENT_SCALE)-1;
            v0 &=(1<<LUT_COMPONENT_SCALE)-1;
            index = (u0<<LUT_COMPONENT_SCALE) | v0;
            sig = m_lut[index];

            if (sig)
            {
                sig--;
                u <<= LUT_ENTRY_SCALE;
                v <<= LUT_ENTRY_SCALE;
                c = r+g1+b;
                if (c==0)
                    c = 1;
                u /= c;
                v /= c;
#if 0
                img.setPixel(x/2, y/2, palette[sig+1]);
#else
                float c, umin, umax, vmin, vmax;
                // scale up
                c = ((float)m_signatures[sig].m_uMax + m_signatures[sig].m_uMin)/2.0f;
                umin = c + (m_signatures[sig].m_uMin - c)*m_acqRange;
                umax = c + (m_signatures[sig].m_uMax - c)*m_acqRange;
                c = ((float)m_signatures[sig].m_vMax + m_signatures[sig].m_vMin)/2.0f;
                vmin = c + (m_signatures[sig].m_vMin - c)*m_acqRange;
                vmax = c + (m_signatures[sig].m_vMax - c)*m_acqRange;


                if (umin<u && u<umax &&
                        vmin<v && v<vmax)
                    img.setPixel(x/2, y/2, palette[sig+1]);
#endif
            }
        }
    }
#if 0
    for (i=0, row=-1; i<numVals; i++)
    {
        if (qVals[i]==0xffffffff)
            continue;
        if (qVals[i]==0)
        {
            row++;
            continue;
        }
        model = qVals[i]&0x07;
        qVals[i] >>= 3;
        startCol = qVals[i]&0x1ff;
        qVals[i] >>= 9;
        length = qVals[i]&0x1ff;
        handleRL(&img, palette[model], row, startCol, length);
    }
#endif
    m_interpreter->m_renderer->emitImage(img);
    if (renderFlags&RENDER_FLAG_FLUSH)
        m_interpreter->m_renderer->emitFlushImage();
#else
    int32_t row;
    uint32_t i, startCol, length, sig, prevStartCol;
    QImage img(width/2, height/2, QImage::Format_ARGB32);
    Qval2 qval;
    unsigned int palette[] =
    {0x00000000, // 0 no model (transparent)
     0x80ff0000, // 1 red
     0x80ff4000, // 2 orange
     0x80ffff00, // 3 yellow
     0x8000ff00, // 4 green
     0x8000ffff, // 5 cyan
     0x800000ff, // 6 blue
     0x80ff00ff  // 7 violet
    };

    // if we're a background frame, set alpha to 1.0
    if (m_interpreter->m_renderer->firstFrame())
    {
        for (i=0; i<sizeof(palette)/sizeof(unsigned int); i++)
            palette[i] |= 0xff000000;
    }

    img.fill(palette[0]);
    int32_t r, g, b, u, v, c;
    for (i=0, row=-1; m_qq->dequeue(&qval); i++)
    {
        if (qval.m_qval==0xffffffff)
            continue;
        if (qval.m_qval==0)
        {
            row++;
            prevStartCol = 0xffff;
            continue;
        }

        sig = qval.m_qval&((1<<7)-1);
        qval.m_qval >>= 7;
        startCol = qval.m_qval&((1<<9)-1);
        qval.m_qval >>= 9;
        length = qval.m_qval;

        b = qval.m_bsum/length;
        g = (qval.m_rgsum&((1<<16)-1))/length;
        r = (qval.m_rgsum>>16)/length;
        u = r-g;
        v = b-g;

        u <<= LUT_ENTRY_SCALE;
        v <<= LUT_ENTRY_SCALE;
        c = r+g+b;
        if (c==0)
            c = 1;
        u /= c;
        v /= c;
#define GAIN 1.0f
        float c, umin, umax, vmin, vmax;
        // scale up
        c = ((float)m_signatures[sig-1].m_uMax + m_signatures[sig-1].m_uMin)/2.0f;
        umin = c + (m_signatures[sig-1].m_uMin - c)*m_acqRange*GAIN;
        umax = c + (m_signatures[sig-1].m_uMax - c)*m_acqRange*GAIN;
        c = ((float)m_signatures[sig-1].m_vMax + m_signatures[sig-1].m_vMin)/2.0f;
        vmin = c + (m_signatures[sig-1].m_vMin - c)*m_acqRange*GAIN;
        vmax = c + (m_signatures[sig-1].m_vMax - c)*m_acqRange*GAIN;

        if (m_yfilter==false || (umin<u && u<umax && vmin<v && v<vmax))
        {
            if (m_fixedLength)
            {
                if (startCol==prevStartCol+_LENGTH+1)
                    m_interpreter->m_renderer->renderRL(&img, palette[sig], row, startCol-1, length+1);
                //m_interpreter->m_renderer->renderRL(&img, palette[sig], row, prevStartCol, length*2+1);
                else if (startCol==prevStartCol+_LENGTH+2)
                    m_interpreter->m_renderer->renderRL(&img, palette[sig], row, startCol-2, length+2);
                else
                    m_interpreter->m_renderer->renderRL(&img, palette[sig], row, startCol, length);
                prevStartCol = startCol;
            }
            else
                m_interpreter->m_renderer->renderRL(&img, palette[sig], row, startCol, length);
        }
    }

    m_interpreter->m_renderer->emitImage(img);
    if (renderFlags&RENDER_FLAG_FLUSH)
        m_interpreter->m_renderer->emitFlushImage();

#endif
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




