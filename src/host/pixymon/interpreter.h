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

#ifndef INTERPRETER_H
#define INTERPRETER_H
#include <QThread>
#include <QWaitCondition>
#include <QMutex>
#include <QStringList>
#include <QColor>
#include <QVariant>
#include <QList>
#include <vector>
#include <utility>
#include "pixytypes.h"
#include "chirpmon.h"
#include "videowidget.h"
#include "connectevent.h"
#include "disconnectevent.h"
#include "usblink.h"
#include "monparameterdb.h"

#define PROMPT  ">"
#define RUN_POLL_PERIOD_SLOW   500 // msecs
#define RUN_POLL_PERIOD_FAST   10  // msecs
#define CD_GENERAL             "General"

class ConsoleWidget;
class Renderer;
class MonModule;

enum CommandType {STOP, RUN, STOP_LOCAL, RUN_LOCAL, LOAD_PARAMS, SAVE_PARAMS, UPDATE_PARAM, CLOSE};

struct Command
{
    Command(CommandType type, const QVariant &arg0=0, const QVariant &arg1=0)
    {
        m_type = type;
        m_arg0 = arg0;
        m_arg1 = arg1;
    }

    // need to implement this for QList::removeAll()
    bool operator==(const Command &rhs)
    {
        return m_type==rhs.m_type;
    }

    CommandType m_type;
    QVariant m_arg0;
    QVariant m_arg1;

};

typedef QList<Command> CommandQueue;
typedef std::pair<QString,QString> Arg;
typedef std::vector<Arg> ArgList;


class Interpreter : public QThread
{
    Q_OBJECT

public:
    Interpreter(ConsoleWidget *console, VideoWidget *video, MonParameterDB *data, const QString &initScript="");
    ~Interpreter();

    // local program business
    int beginLocalProgram();
    int endLocalProgram();
    int clearLocalProgram();

    // "remote" program business
    void runOrStopProgram(bool local=false);
    uint programRunning();

    void execute(QString command);
    void execute(QStringList commandList);
    void loadParams();
    void saveParams(bool reject=false);
    void updateParam();
    int saveImage(const QString &filename);
    void printHelp();

    void close();
    void unwait();

    uint16_t *getVersion();
    QString getVersionType();

    void getSelection(RectA *region);
    void getSelection(Point16 *point);
    void cprintf(const char *format, ...);

    void emitActionScriptlet(QString action, QStringList scriptlet);

    ChirpMon *m_chirp;
    Renderer *m_renderer;
    ParameterDB m_pixyParameters;
    MonParameterDB *m_pixymonParameters;

    friend class ChirpMon;
    friend class Renderer;

signals:
    void runState(int state);
    void textOut(QString text, QColor color=Qt::black);
    void error(QString text);
    void consoleCommand(QString text);
    void prompt(QString text);
    void videoInput(VideoWidget::InputMode mode);
    void enableConsole(bool enable);
    void connected(Device device, bool state);
    void actionScriptlet(int index, QString action, QStringList scriptlet);
    void actionScriptlet(QString action, QStringList scriptlet);
    void parameter(QString id, QByteArray data);
    void paramLoaded();
    void version(ushort major, ushort minor, ushort build, QString type);

public slots:

private slots:
    void controlKey(Qt::Key key);
    void command(const QString &command);
    void handleSelection(int x0, int y0, int width, int height);

protected:
    virtual void run();

private:
    void handleHelp();
    void handleCall(const QStringList &argv);
    void listProgram();
    int call(const QStringList &argv, bool interactive=false);
    void handleResponse(const void *args[]);
    void handleData(const void *args[]);

    int addProgram(ChirpCallData data);
    int addProgram(const QStringList &argv);
    void handleLocalProgram();

    void getRunning();
    int sendRun();
    int sendStop();
    int sendGetAction(int index);
    void queueCommand(CommandType type, const QVariant &arg0=0, const QVariant &arg1=0);
    void handlePendingCommand();

    void prompt();

    QString extractProperty(const QString &tag, QString *desc);
    void handleProperties(const uint8_t *argList, Parameter *parameter, QString *desc);
    void handleSaveParams(bool reject);
    void handlePixySaveParams(bool shadow);
    void handleLoadParams(); // load from Pixy
    void handleUpdateParam();
    void sendMonModulesParamChange();

    QStringList getSections(const QString &id, const QString &string);
    int getArgs(const ProcInfo *info, ArgList *argList);
    QString printProc(const ProcInfo *info,  int level=0);
    QString printArgType(uint8_t *type, int &index);
    QString printArgType(uint8_t type, uint32_t flags);
    QStringList parseScriptlet(const QString &scriptlet);

    void augmentProcInfo(ProcInfo *info);

    ConsoleWidget *m_console;
    VideoWidget *m_video;

    USBLink m_link;

    // for thread
    QMutex m_mutexProg;
    QMutex m_mutexInput;
    QMutex m_mutexQueue;
    QWaitCondition m_waitInput;
    CommandQueue m_commandQueue;

    unsigned int m_pc;
    ChirpProc m_exec_run;
    ChirpProc m_exec_running;
    ChirpProc m_exec_stop;
    ChirpProc m_exec_get_action;
    ChirpProc m_get_param;
    ChirpProc m_getAll_param;
    ChirpProc m_set_param;
    ChirpProc m_reload_params;
    ChirpProc m_set_shadow_param;
    ChirpProc m_reset_shadows;

    // for program
    bool m_programming;
    bool m_localProgramRunning;
    bool m_waiting;
    bool m_run;
    bool m_fastPoll;
    bool m_notified;
    int m_running;

    std::vector<ChirpCallData> m_program;
    std::vector<QStringList> m_programText;

    QString m_command;
    QString m_print;
    Qt::Key m_key;
    QStringList m_argv; // executed on Pixy
    QStringList m_argvHost;  // executed on host
    QStringList m_commandList;
    RectA m_selection;

    QList <MonModule *> m_modules;

    uint8_t m_argTypes[0x100];
    uint16_t m_version[3];
    QString m_versionType;
    QString m_initScript;
};


#endif // INTERPRETER_H
