#include <QPainter>
#include <vector>
#include "fdmodule.h"
#include "interpreter.h"
#include "renderer.h"

// declare module
MON_MODULE(FdModule);

FdModule::FdModule(Interpreter *interpreter) : MonModule(interpreter), m_cascade("../pixymon/lbpcascade_frontalface.xml")
{
    QStringList scriptlet;

    scriptlet << "runprogArg 8 101";
    m_interpreter->emitActionScriptlet("Run face detection", scriptlet);

    m_scaleFactor = 1.3f;
    m_stepSize = 2.0f;

    m_interpreter->m_pixymonParameters->addSlider("Scale factor", m_scaleFactor, 0.0f, 10.0f, "Sets the scale factor", "Face Detection");
    m_interpreter->m_pixymonParameters->addSlider("Step size", m_stepSize, 0.0f, 10.0f, "Sets the step size", "Face Detection");
}

FdModule::~FdModule()
{
}

bool FdModule::render(uint32_t fourcc, const void *args[])
{
    if (fourcc==FOURCC('E','X','0','1'))
    {
        renderEX01(*(uint8_t *)args[0], *(uint16_t *)args[1], *(uint16_t *)args[2], *(uint32_t *)args[3], (uint8_t *)args[4]);
        return true;
    }
    return false;
}

bool FdModule::command(const QStringList &argv)
{
#if 0 // insert your command here
    if (argv[0]=="someCommand")
    {
        return true;
    }
#endif
    return false;
}

void FdModule::paramChange()
{
    m_scaleFactor = pixymonParameter("Scale factor").toFloat();
    m_stepSize = pixymonParameter("Step size").toFloat();
}

void FdModule::renderEX01(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame)
{
    QPainter p;
    QImage img(width, height, QImage::Format_ARGB32);
    img.fill(0x00000000);
    // Run face detection
    std::vector<detectionLocation> faces = m_cascade.detectMultiScale(*m_renderer->backgroundImage(), m_scaleFactor, m_stepSize);

    // Draw detected faces
    p.begin(&img);
    for (std::vector<detectionLocation>::const_iterator detectionLoc = faces.begin(); detectionLoc != faces.end(); ++detectionLoc)
    {
        p.drawRect(detectionLoc->locationX, detectionLoc->locationY, detectionLoc->height, detectionLoc->width);
//        p.drawText(detectionLoc->locationX, detectionLoc->locationY, "face here");
    }
    p.end();

    m_renderer->renderBA81(RENDER_FLAG_BLEND, width, height, frameLen, frame);
    m_renderer->emit image(img, RENDER_FLAG_BLEND | RENDER_FLAG_FLUSH);
}

