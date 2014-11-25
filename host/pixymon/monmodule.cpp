#include <stdarg.h>
#include "monmodule.h"
#include "parameters.h"
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

bool MonModule::pixyParameterChanged(const QString &id, QVariant *val)
{
    Parameter *param = pixyParameter(id);
    if (param && val)
        *val = param->value();
    if (param && param->dirty())
        return true;
    return false;
}

bool MonModule::pixymonParameterChanged(const QString &id, QVariant *val)
{
    Parameter *param = pixymonParameter(id);
    if (param && val)
        *val = param->value();
    if (param && param->dirty())
        return true;
    return false;
}

Parameter *MonModule::pixyParameter(const QString &id)
{
    return m_interpreter->m_pixyParameters.parameter(id);
}

Parameter *MonModule::pixymonParameter(const QString &id)
{
    return m_interpreter->m_pixymonParameters->parameter(id);
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



