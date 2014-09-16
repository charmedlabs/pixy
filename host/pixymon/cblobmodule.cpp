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

    m_qq = new Qqueue();
    m_lut = new uint8_t[LUT_SIZE];
    m_cblob = new ColorBlob(m_lut);

    scriptlet << "cam_getFrame 0x21 0 0 320 200";
    scriptlet << "cb_region";
    scriptlet << "runprogArg 8 100";
    m_interpreter->emit actionScriptlet("Create new signature...", scriptlet);

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

        ColorSignature sig;

        Frame8 *frame = m_interpreter->m_renderer->backgroundRaw();
        m_interpreter->m_renderer->pixelsOut(region.m_xOffset, region.m_yOffset, region.m_width, region.m_height);
        m_cblob->generateSignature(frame, &region, &sig);
        qDebug("sig: %d %d %d %d", sig.m_uMin, sig.m_uMax, sig.m_vMin, sig.m_vMax);

        m_cblob->generateLUT(&sig, 1);
        m_signatures[0] = sig;


        DataExport dx(m_interpreter->m_pixymonParameters->value("Document folder")->toString(), "lut", ET_MATLAB);

        dx.startArray(1, "lut");

        for (int i=0; i<LUT_SIZE; i++)
            dx.addElement(m_lut[i]);

        return true;
    }
    return false;
}

void CBlobModule::rls(const Frame8 *frame)
{
    uint32_t x, y, count, index, startCol, model, lutVal;
    int32_t r, g1, g2, b;
    int32_t c1, c2;
    bool stateIn, stateOut;
    uint32_t prevModel=0;

    for (y=1; y<(uint32_t)frame->m_height; y+=2)
    {
        // new lime
        m_qq->enqueue(0);

        stateIn = stateOut = false;
        count = 0;
        prevModel = 0;
        startCol = 0;
        for (x=1; x<(uint32_t)frame->m_width; x+=2)
        {
            r = frame->m_pixels[y*frame->m_width + x];
            g1 = frame->m_pixels[y*frame->m_width + x - 1];
            g2 = frame->m_pixels[y*frame->m_width - frame->m_width + x];
            b = frame->m_pixels[y*frame->m_width - frame->m_width + x - 1];
            c2 = r-g1;
            c1 = b-g2;
            c1 >>= 1;
            c2 >>= 1;
            index = ((uint8_t)c2<<8) | (uint8_t)c1;
            lutVal = m_lut[index];

            model = lutVal&0x07;

            if (model && prevModel==0)
            {
                startCol = x/2;
            }
            if ((model && prevModel && model!=prevModel) ||
                    (model==0 && prevModel))
            {
                model = prevModel;
                model |= startCol<<3;
                model |= (x/2-startCol)<<12;
                m_qq->enqueue(model);
                model = 0;
                startCol = 0;
            }
            prevModel = model;
        }

        if (startCol)
        {
            model = prevModel;
            model |= startCol<<3;
            model |= (x/2-startCol)<<12;
            m_qq->enqueue(model);
            model = 0;
        }

    }
    // indicate end of frame
    m_qq->enqueue(0xffffffff);
}

void CBlobModule::renderEX00(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame)
{
    Frame8 frame8(frame, width, height);
    //rls(&frame8);

    m_interpreter->m_renderer->renderBA81(0, width, height, frameLen, frame);
    renderCCQ2(RENDER_FLAG_FLUSH, width, height, frameLen, frame);

}

void CBlobModule::renderCCQ2(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame)
{
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

    // if we're a background frame, set alpha to 1.0
    if (m_interpreter->m_renderer->firstFrame())
    {
        for (i=0; i<sizeof(palette)/sizeof(unsigned int); i++)
            palette[i] |= 0xff000000;
    }

    img.fill(palette[0]);

    uint32_t x, y, index;
    int32_t r, g1, g2, b, u, v, u0, v0;
    uint8_t sig;

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
                u /= r+g1+b;
                v /= r+g1+b;
#if 0
                img.setPixel(x/2, y/2, palette[sig+1]);
#else
                float c, umin, umax, vmin, vmax, trackRange = 2.0f;
                // scale up
                c = ((float)m_signatures[sig].m_uMax + m_signatures[sig].m_uMin)/2.0f;
                umin = c + (m_signatures[sig].m_uMin - c)*trackRange;
                umax = c + (m_signatures[sig].m_uMax - c)*trackRange;
                c = ((float)m_signatures[sig].m_vMax + m_signatures[sig].m_vMin)/2.0f;
                vmin = c + (m_signatures[sig].m_vMin - c)*trackRange;
                vmax = c + (m_signatures[sig].m_vMax - c)*trackRange;


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
}



