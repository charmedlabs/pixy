#ifndef _DEBUG2_H
#define _DEBUG2_H

#include <QDebug>
#include "monmodule.h"

#define DBG(...)            if (g_debug) qDebug(__VA_ARGS__)
#define DBGL(level, ...)    if (g_debug>=level) qDebug(__VA_ARGS__)
#define DBGE(n, ...)        if (g_debug==n) qDebug(__VA_ARGS__)

class Debug : public MonModule
{
public:
    Debug(Interpreter *interpreter);

    virtual void paramChange();
};

extern uint g_debug;

#endif
