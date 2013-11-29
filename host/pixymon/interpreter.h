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
#include "blobs.h"
#include "clut.h"
#include "connectevent.h"
#include "disconnectevent.h"
#include "usblink.h"

#define PROMPT  ">"

class VideoWidget;
class ConsoleWidget;
class Renderer;
class MainWindow;

typedef std::pair<QString,QString> Arg;
typedef std::vector<Arg> ArgList;

class Interpreter : public QThread
{
    Q_OBJECT

public:
    Interpreter(ConsoleWidget *console, VideoWidget *video, MainWindow *main);
    ~Interpreter();
    int beginProgram();
    int endProgram();
    int runProgram();
    int runRemoteProgram();
    int stopProgram();
    int clearProgram();
    bool programRunning()
    {
        return m_programRunning || m_remoteProgramRunning;
    }

    int call(const QString &command);
    void printHelp();

    ChirpMon *m_chirp;

    // experimental
    ConsoleWidget *m_console;
    VideoWidget *m_video;
    Renderer *m_renderer;
    MainWindow *m_main;

    friend class ChirpMon;
    friend class DisconnectEvent;

signals:
    void runState(bool state);
    void textOut(QString text, QColor color=Qt::black);
    void error(QString text);
    void prompt(QString text);
    void videoPrompt(uint type);
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
    void handleHelp(const QStringList &argv);
    void handleCall(const QStringList &argv);
    void listProgram();
    int call(const QStringList &argv, bool interactive=false);
    void handleResponse(void *args[]);
    void handleData(void *args[]);
    int addProgram(ChirpCallData data);
    int addProgram(const QStringList &argv);
    int execute();
    bool checkRemoteProgram();
    int stopRemoteProgram();

    int getRunning();
    int sendRun();
    int sendStop();

    void prompt();
    QStringList getSections(const QString &id, const QString &string);
    int getArgs(const ProcInfo *info, ArgList *argList);
    QString printProc(const ProcInfo *info,  int level=0);
    QString printArgType(uint8_t *type, int &index);
    void augmentProcInfo(ProcInfo *info);

    // experimental
    int uploadLut();
    int loadLut(const QString &filename, int model);

    void getStats(int x0, int y0, int width, int height);
    void writeFrame();
    void fileOut(const QString &name, int *data, unsigned int len, unsigned int pitch=1, int p1=0, int p2=0);

    // DEBUG
#if 0
    void fileOutDebug(uint8_t *data);
#endif

    unsigned int fileIn(const QString &name, char *data, unsigned int size);

    USBLink m_link;

    uint8_t *m_lut;

    uint8_t m_tempLut[LUT_SIZE];

    // for thread
    QMutex m_mutexProg;
    QMutex m_mutexInput;
    QWaitCondition m_waitInput;

    unsigned int m_pc;
    ChirpProc m_exec_run;
    ChirpProc m_exec_running;
    ChirpProc m_exec_stop;

    // for program
    bool m_programming;
    bool m_programRunning;
    bool m_remoteProgramRunning;
    bool m_init;
    std::vector<ChirpCallData> m_program;
    std::vector<QStringList> m_programText;

    DisconnectEvent *m_disconnect;
    QString m_command;
    QString m_print;
    Qt::Key m_key;
    uint32_t m_rcount;
    QStringList m_argv;
    uint8_t m_argTypes[0x100];
};


#endif // INTERPRETER_H
