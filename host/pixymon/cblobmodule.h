#ifndef CBLOBMODULE_H
#define CBLOBMODULE_H

#include "monmodule.h"

class CBlobModule : public MonModule
{
public:
    CBlobModule(Interpreter *interpreter);
    ~CBlobModule();

    virtual void selection(int x0, int y0, int width, int height);
    virtual bool render(uint32_t fourcc, void *args[]);
    virtual bool command(QStringList argv);
};

#endif // CBLOBMODULE_H
