#include <stdarg.h>
#include "monmodule.h"
#include "interpreter.h"


NewMonModuleFunc MonModuleUtil::m_modules[MAX_MONMODULES];
unsigned int MonModuleUtil::m_index = 0;
Interpreter *MonModuleUtil::m_interpreter = NULL;



MonModule::MonModule(Interpreter *interpreter)
{
    m_interpreter = interpreter;
    MonModuleUtil::m_interpreter = interpreter;

}

MonModule::~MonModule()
{
}

bool MonModule::render(uint32_t fourcc, const void *args[])
{
    return false;
}

bool MonModule::command(const QStringList &argv)
{
    return false;
}

void MonModule::paramChange()
{
}

void cprintf(const char *format, ...)
{
    char buffer[256];
    va_list args;
    va_start(args, format);
    vsnprintf(buffer, 256, format, args);
    MonModuleUtil::m_interpreter->cprintf(buffer);
    va_end(args);
}
