#include <QDebug>
#include "cblobmodule.h"


MON_MODULE(CBlobModule);


CBlobModule::CBlobModule(Interpreter *interpreter) : MonModule(interpreter)
{
}

CBlobModule::~CBlobModule()
{
    qDebug("hello");
}

void CBlobModule::selection(int x0, int y0, int width, int height)
{
}

bool CBlobModule::render(uint32_t fourcc, void *args[])
{
    return false;
}

bool CBlobModule::command(QStringList argv)
{
    return false;
}

