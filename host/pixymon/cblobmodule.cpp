#include <QDebug>
#include "interpreter.h"
#include "renderer.h"
#include "cblobmodule.h"

// declare module
MON_MODULE(CBlobModule);


CBlobModule::CBlobModule(Interpreter *interpreter) : MonModule(interpreter)
{
    m_qVals = new uint32_t[MAX_QVALS];
    m_lut = new uint8_t[LUT_SIZE];
    m_cblob = new ColorBlob(m_lut);
    m_numqVals = 0;
}

CBlobModule::~CBlobModule()
{
    delete [] m_qVals;
    delete [] m_lut;
}

void CBlobModule::selection(int x0, int y0, int width, int height)
{
    ColorSignature sig;

    Frame8 *frame = m_interpreter->m_renderer->backgroundRaw();
    RectA region(x0, y0, width, height);
    m_interpreter->m_renderer->pixelsOut(x0, y0, width, height);
    m_cblob->generateSignature(frame, &region, &sig);
    qDebug("sig: %d %d %d %d", sig.m_uMin, sig.m_uMax, sig.m_vMin, sig.m_vMax);

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
        m_interpreter->emitVideoInput(this, VideoWidget::REGION);
        return true;
    }
    return false;
}

void CBlobModule::rls(const Frame8 *frame)
{
}

void CBlobModule::renderEX00(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame)
{
    Frame8 frame8(frame, width, height);
    rls(&frame8);

}


