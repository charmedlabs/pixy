#ifndef INTERPRETER_H
#define INTERPRETER_H
#include <QThread>
#include <QWaitCondition>
#include <QMutex>
#include <QStringList>
#include <vector>
#include <utility>
#include "chirpmon.h"
#include "blobs.h"
#include "clut.h"
#include "connectevent.h"

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
    int resumeProgram();
    int stopProgram();
    int clearProgram();
    bool programRunning()
    {
        return m_programRunning;
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

signals:
    void runState(bool state);
    void textOut(const QString &text);
    void error(const QString &text);
    void prompt(const QString &text);
    void videoPrompt(uint type);
    void enableConsole(bool enable);
    void connected(ConnectEvent::Device device, bool state);

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
    int handleResponse(void *args[]);
    int addProgram(ChirpCallData data);
    int addProgram(const QStringList &argv);
    int execute();
    bool checkRemoteProgram();
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
    uint8_t *m_lut;

    uint8_t m_tempLut[LUT_SIZE];

    // for thread
    QMutex m_mutex;
    QMutex m_mutexInput;
    QWaitCondition m_waitInput;

    unsigned int m_pc;

    // for program
    bool m_programming;
    bool m_programRunning;
    bool m_remoteProgramRunning;
    std::vector<ChirpCallData> m_program;
    std::vector<QStringList> m_programText;

    QString m_command;
    Qt::Key m_key;
    uint32_t m_rcount;
    QStringList m_argv;
    uint8_t m_argTypes[0x100];
};


#endif // INTERPRETER_H
