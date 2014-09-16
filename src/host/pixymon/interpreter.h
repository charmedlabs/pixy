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
#include <vector>
#include <queue>
#include <utility>
#include "chirpmon.h"
#include "videowidget.h"
#include "connectevent.h"
#include "disconnectevent.h"
#include "usblink.h"
#include "parameters.h"

#define PROMPT  ">"
#define RUN_POLL_PERIOD_SLOW   500 // msecs
#define RUN_POLL_PERIOD_FAST   10  // msecs
#define CD_GENERAL             "General"

class ConsoleWidget;
class Renderer;

enum CommandType {STOP, RUN, GET_ACTION, LOAD_PARAMS, SAVE_PARAMS};

typedef std::pair<CommandType,QVariant> Command;
typedef std::queue<Command> CommandQueue;

typedef std::pair<QString,QString> Arg;
typedef std::vector<Arg> ArgList;


class Interpreter : public QThread
{
    Q_OBJECT

public:
    Interpreter(ConsoleWidget *console, VideoWidget *video, ParameterDB *data);
    ~Interpreter();

    // local program business
    int beginLocalProgram();
    int endLocalProgram();
    int runLocalProgram();
    int clearLocalProgram();

    // "remote" program business
    void runOrStopProgram();
    uint programRunning();

    void execute(const QString &command);
    void execute(QStringList commandList);
    void getAction(int index);
    void loadParams();
    void saveParams();
    int saveImage(const QString &filename);
    void printHelp();

    void close();
    void unwait();

    uint16_t *getVersion();

    ChirpMon *m_chirp;
    ParameterDB m_pixyParameters;
    ParameterDB *m_pixymonParameters;

    friend class ChirpMon;

signals:
    void runState(uint state);
    void textOut(QString text, QColor color=Qt::black);
    void error(QString text);
    void prompt(QString text);
    void videoInput(VideoWidget::InputMode mode);
    void enableConsole(bool enable);
    void connected(Device device, bool state);
    void actionScriptlet(int index, QString action, QStringList scriptlet);
    void parameter(QString id, QByteArray data);
    void paramLoaded();
    void paramChange();

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
    void handleResponse(void *args[]);
    void handleData(void *args[]);

    int addProgram(ChirpCallData data);
    int addProgram(const QStringList &argv);
    int execute();

    void getRunning();
    int sendRun();
    int sendStop();
    int sendGetAction(int index);
    void queueCommand(CommandType type, QVariant arg=QVariant(0));
    void handlePendingCommand();

    void prompt();

    void handleSaveParams(); // save to Pixy
    void handleLoadParams(); // load from Pixy

    QStringList getSections(const QString &id, const QString &string);
    int getArgs(const ProcInfo *info, ArgList *argList);
    QString printProc(const ProcInfo *info,  int level=0);
    QString printArgType(uint8_t *type, int &index);
    QString printArgType(uint8_t type, uint32_t flags);

    void augmentProcInfo(ProcInfo *info);

    ConsoleWidget *m_console;
    VideoWidget *m_video;
    Renderer *m_renderer;

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

    // for program
    bool m_programming;
    bool m_localProgramRunning;
    bool m_waiting;
    bool m_run;
    bool m_fastPoll;
    bool m_notified;
    bool m_paramDirty;
    int m_running;

    std::vector<ChirpCallData> m_program;
    std::vector<QStringList> m_programText;

    QString m_command;
    QString m_externalCommand;
    QString m_print;
    Qt::Key m_key;
    uint32_t m_rcount;
    QStringList m_argv; // executed on Pixy
    QStringList m_argvHost;  // executed on host
    QStringList m_commandList;

    uint8_t m_argTypes[0x100];
    uint16_t m_version[3];
};


#endif // INTERPRETER_H
