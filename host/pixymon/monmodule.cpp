#include "monmodule.h"


NewMonModuleFunc MonModuleUtil::m_modules[MAX_MONMODULES];
unsigned int MonModuleUtil::m_index = 0;



MonModule::MonModule(Interpreter *interpreter)
{
    m_interpreter = interpreter;
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


