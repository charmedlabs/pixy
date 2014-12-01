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
