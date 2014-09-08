#ifndef MONMODULE_H
#define MONMODULE_H

#include <QString>
#include <QStringList>
#include <QList>
#include <inttypes.h>

#define MAX_MONMODULES 0x40 // this should be way more than neeeded....

class MonModule;
class Interpreter;

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

    virtual void selection(int x0, int y0, int width, int height);
    virtual bool render(uint32_t fourcc, void *args[]);
    virtual bool command(QStringList argv);

protected:
    Interpreter *m_interpreter;
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
};



#endif // MONMODULE_H
