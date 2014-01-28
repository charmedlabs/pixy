#ifndef INTERPRETER_H
#define INTERPRETER_H
#include <QThread>
#include <QWaitCondition>
#include <QMutex>
#include <QStringList>
#include <QColor>
#include <vector>
#include <utility>
#include "chirpmon.h"
#include "videowidget.h"
#include "connectevent.h"
#include "disconnectevent.h"
#include "usblink.h"

#define PROMPT  ">"
#define RUN_POLL_PERIOD_SLOW   500 // msecs
#define RUN_POLL_PERIOD_FAST   50  // msecs

class ConsoleWidget;
class Renderer;

typedef std::pair<QString,QString> Arg;
typedef std::vector<Arg> ArgList;

class Interpreter : public QThread
{
    Q_OBJECT

public:
    Interpreter(ConsoleWidget *console, VideoWidget *video);
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
    void printHelp();

    void close();
    void unwait();

    ChirpMon *m_chirp;

    // experimental
    ConsoleWidget *m_console;
    VideoWidget *m_video;
    Renderer *m_renderer;

    friend class ChirpMon;

signals:
    void runState(uint state);
    void textOut(QString text, QColor color=Qt::black);
    void error(QString text);
    void prompt(QString text);
    void videoInput(VideoWidget::InputMode mode);
    void enableConsole(bool enable);
    void connected(Device device, bool state);

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
    void handlePendingCommand();

    void prompt();
    QStringList getSections(const QString &id, const QString &string);
    int getArgs(const ProcInfo *info, ArgList *argList);
    QString printProc(const ProcInfo *info,  int level=0);
    QString printArgType(uint8_t *type, int &index);
    void augmentProcInfo(ProcInfo *info);

    USBLink m_link;

    // for thread
    QMutex m_mutexProg;
    QMutex m_mutexInput;
    QWaitCondition m_waitInput;
    enum {NONE, STOP, RUN} m_pendingCommand;

    unsigned int m_pc;
    ChirpProc m_exec_run;
    ChirpProc m_exec_running;
    ChirpProc m_exec_stop;

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
    QString m_externalCommand;
    QString m_print;
    Qt::Key m_key;
    uint32_t m_rcount;
    QStringList m_argv;
    uint8_t m_argTypes[0x100];
};


#endif // INTERPRETER_H
