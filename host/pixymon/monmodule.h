//
// begin license header
//
// This file is part of Pixy CMUcam5 or "Pixy" for short
//
// All Pixy source code is provided under the terms of the
// GNU General Public License v2 (http://www.gnu.org/licenses/gpl-2.0.html).
// Those wishing to use Pixy source code, software and/or
// technologies under different licensing terms should contact us at
// cmucam@cs.cmu.edu. Such licensing terms are available for
// all portions of the Pixy codebase presented here.
//
// end license header
//

#ifndef MONMODULE_H
#define MONMODULE_H

#include <QString>
#include <QStringList>
#include <QList>
#include <inttypes.h>

#define MAX_MONMODULES 0x40 // this should be way more than neeeded....

class MonModule;
class Interpreter;
class Parameter;
class Renderer;

typedef MonModule *(*NewMonModuleFunc)(Interpreter *);
typedef QList <MonModule *> MonModules;


#define MON_MODULE(module) \
    MonModule *newFunc ## module(Interpreter *interpreter) {\
        return new module(interpreter); } \
    MonModuleUtil g_register ## module(newFunc ## module);


class MonModule
{
public:
    MonModule(Interpreter *interpreter);
    virtual ~MonModule();

    virtual bool render(uint32_t fourcc, const void *args[]);
    virtual bool command(const QStringList &argv);
    virtual void paramChange();

protected:
    bool pixyParameterChanged(const QString &id, QVariant *val=NULL);
    bool pixymonParameterChanged(const QString &id, QVariant *val=NULL);
    QVariant pixyParameter(const QString &id);
    QVariant pixymonParameter(const QString &id);

    Interpreter *m_interpreter;
    Renderer *m_renderer;
};


struct MonModuleUtil
{
    MonModuleUtil(NewMonModuleFunc func)
    {
        m_modules[m_index++] = func;
    }

    static void createModules(MonModules *modules, Interpreter *interpreter)
    {
        for (uint i=0; i<m_index; i++)
            modules->push_back((*m_modules[i])(interpreter));
    }

    static void destroyModules(MonModules *modules)
    {
        for (int i=0; i<modules->size(); i++)
            delete (*modules)[i];
    }

    static NewMonModuleFunc m_modules[MAX_MONMODULES];
    static uint m_index;
    static Interpreter *m_interpreter;
};

void cprintf(const char *format, ...);



#endif // MONMODULE_H
