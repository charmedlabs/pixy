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

#include <stdexcept>
#include <QMessageBox>
#include <QFile>
#include "debug.h"
#include <QTime>
#include <stdarg.h>
#include "interpreter.h"
#include "disconnectevent.h"
#include "videowidget.h"
#include "console.h"
#include "mainwindow.h"
#include "renderer.h"
#include "sleeper.h"
#include "pixymon.h"
#include "monmodule.h"

QString printType(uint32_t val, bool parens=false);

Interpreter::Interpreter(ConsoleWidget *console, VideoWidget *video, MonParameterDB *data, const QString &initScript) :
    m_mutexProg(QMutex::Recursive)
{
    m_initScript = initScript;
    m_initScript.remove(QRegExp("^\\s+"));  // remove initial whitespace
    m_console = console;
    m_video = video;
    m_pixymonParameters = data;
    m_pc = 0;
    m_programming = false;
    m_localProgramRunning = false;
    m_waiting = false;
    m_fastPoll = true;
    m_notified = false;
    m_running = -1; // set to bogus value to force update
    m_chirp = NULL;

    m_renderer = new Renderer(m_video, this);

    connect(m_console, SIGNAL(textLine(QString)), this, SLOT(command(QString)));
    connect(m_console, SIGNAL(controlKey(Qt::Key)), this, SLOT(controlKey(Qt::Key)));
    connect(this, SIGNAL(textOut(QString, QColor)), m_console, SLOT(print(QString, QColor)));
    connect(this, SIGNAL(error(QString)), m_console, SLOT(error(QString)));
    connect(this, SIGNAL(enableConsole(bool)), m_console, SLOT(acceptInput(bool)));
    connect(this, SIGNAL(prompt(QString)), m_console, SLOT(prompt(QString)));
    connect(this, SIGNAL(consoleCommand(QString)), m_console, SLOT(command(QString)));
    connect(this, SIGNAL(videoInput(VideoWidget::InputMode)), m_video, SLOT(acceptInput(VideoWidget::InputMode)));
    connect(m_video, SIGNAL(selection(int,int,int,int)), this, SLOT(handleSelection(int,int,int,int)));

    prompt();

    m_run = true;
}

Interpreter::~Interpreter()
{
    DBG("destroying interpreter...");
    close();
    wait();
    clearLocalProgram();
    MonModuleUtil::destroyModules(&m_modules);
    if (m_chirp)
        delete m_chirp;
    DBG("done");
}

void Interpreter::close()
{
    m_localProgramRunning = false;
    m_console->m_mutexPrint.lock();
    m_console->m_waitPrint.wakeAll();
    m_console->m_mutexPrint.unlock();
    unwait(); // if we're waiting for input, unhang ourselves

    m_run = false;

    // save any parameters
    m_pixymonParameters->save();
}

void Interpreter::handleLocalProgram()
{
    int res;

    if (m_program.size()>0 && m_localProgramRunning)
    {
        if (m_pc>=m_program.size())
            m_pc = 0;
        res = m_chirp->execute(m_program[m_pc]);
        if (res<0)
            queueCommand(STOP_LOCAL);
        m_pc++;
    }
}

QString Interpreter::printArgType(uint8_t type, uint32_t flags)
{
    switch(type)
    {
    case CRP_INT8:
        return flags&PRM_FLAG_SIGNED ? "INT8" : "UINT8";
    case CRP_INT16:
        return flags&PRM_FLAG_SIGNED ? "INT16" : "UINT16";
    case CRP_INT32:
        return flags&PRM_FLAG_SIGNED ? "INT32" : "UINT32";
    case CRP_FLT32:
        return "FLOAT32";
    case CRP_STRING:
        return "CSTRING";
    default:
        return "?";
    }
}

QString Interpreter::printArgType(uint8_t *type, int &index)
{
    if (*type==CRP_TYPE_HINT)
    {
        int n = strlen((char *)type);
        QString print = "HINT(";
        if (n>4)
        {
            print += printType(FOURCC(type[1], type[2], type[3], type[4]), false) + ") ";
            index += 4;
        }
        else
        {
            index += n-1;
            print += "error)";
        }
        return print;
    }
    else
        return printArgType(*type, PRM_FLAG_SIGNED);
}

QString Interpreter::printProc(const ProcInfo *info, int level)
{
    ArgList list;
    QString print;
    QStringList sections;
    int i, j;

    print = QString(info->procName) + "(";
    if (getArgs(info, &list)<0)
        return "";
    for (i=0; i<(int)list.size(); i++)
    {
        if (i>0)
            print +=  ", ";
	j = i;
        print += printArgType(&info->argTypes[i], i) + " " + list[j].first;
    }
    print += ")\n";

    if (level>0)
    {
        sections = getSections("", info->procInfo);
        if (sections.size()>0)
            print += sections[0] + "\n";
        print += "Parameters:\n";
        if (list.size()==0)
            print += "<NONE>\n";
        for (i=0; i<(int)list.size(); i++)
        {
            print += "   " + list[i].first + ": ";
            print += list[i].second + "\n";
        }
        sections = getSections("@r", info->procInfo);
        print += "Returns:\n";
        for (i=0; i<sections.size(); i++)
            print += "   " + sections[i] + "\n";
    }

    return print;
}

void Interpreter::printHelp()
{
    ProcInfo info;
    ChirpProc p;

    for (p=0; true; p++)
    {
        if (m_chirp->getProcInfo(p, &info)<0)
            break;
        emit textOut(QString::number(p) + ": " + printProc(&info));
    }
}

QStringList Interpreter::parseScriptlet(const QString &scriptlet)
{
    if (scriptlet.contains(QString("\\") + "n")) // this is comming from the commandline -- we are looking for
        // the backslash followed by the n character, which the c-compiler will always substitute CR character
        return scriptlet.split(QString("\\") + "n", QString::SkipEmptyParts);
    else
        return scriptlet.split(QRegExp("[\\n]"), QString::SkipEmptyParts);
}

QStringList Interpreter::getSections(const QString &id, const QString &string)
{
    int i;
    QStringList sections, words;
    QString section;

    if(id=="")
    {
        sections << string.section('@', 0, 0);
        return sections;
    }

    for (i=1; true; i++)
    {
        section = string.section('@', i, i, QString::SectionIncludeLeadingSep);
        if (section=="@")
            break;
        words = section.split(QRegExp("\\s+"));
        if (words[0].contains(id))
        {
            section.remove(words[0]); // remove id
            section.remove(QRegExp("^\\s+")); // remove leading whitespace
            sections << section;
        }
    }

    return sections;
}

int Interpreter::getArgs(const ProcInfo *info, ArgList *argList)
{
    int i;
    QString label, desc, procInfo(info->procInfo);
    QStringList sections, words;

    if (info->argTypes==NULL)
        return -1;

    sections = getSections("@p", procInfo);
    for (i=0; info->argTypes[i]; i++)
    {
        if (i<sections.size())
        {
            words = sections[i].split(QRegExp("\\s+"));
            label = words[0];
            desc = sections[i];
            desc.remove(QRegExp("^\\s+")); // remove leading whitespace
            desc.remove(QRegExp("^" + label)); // remove id
            desc.remove(QRegExp("^\\s+")); // remove leading whitespace
            argList->push_back(Arg(label, desc));
            // if argument is a type hint, we need to skip the 4-byte (32-bit) int, error if we run out of args
            if (info->argTypes[i]==CRP_TYPE_HINT &&
                    (!info->argTypes[++i] || !info->argTypes[++i] || !info->argTypes[++i] || !info->argTypes[++i]))
                return -1;
        }
        else
            argList->push_back(Arg(QString("arg")+QString::number(i), ""));
    }
    return 0;
}


QString printType(uint32_t val, bool parens)
{
    QString res;
    QChar a, b, c, d;
    uint32_t val2 = val;

    a = (QChar)(val2&0xff);
    val2 >>= 8;
    b = (QChar)(val2&0xff);
    val2 >>= 8;
    c = (QChar)(val2&0xff);
    val2 >>= 8;
    d = (QChar)(val2&0xff);

    if (a.isPrint() && b.isPrint() && c.isPrint() && d.isPrint())
    {
        if (parens)
            res = QString("FOURCC(") + a + b + c + d + ")";
        else
            res = QString(a) + b + c + d;
    }
    else
    {
        if (parens)
            res = "HTYPE(0x" + QString::number((uint)val, 16) + ")";
        else
            res = "0x" + QString::number((uint)val, 16);
    }

    return res;
}

void Interpreter::handleResponse(const void *args[])
{
    // strip off response, add to print string
    m_print = "response: " +
            QString::number(*(int *)args[0]) + " (0x" + QString::number((uint)*(uint *)args[0], 16) + ") ";

    // render rest of response, if present
    handleData(args+1);
}

void Interpreter::handleData(const void *args[])
{
    int i;
    uint8_t type;
    QColor color = CW_DEFAULT_COLOR;

    if (args[0])
    {
        type = Chirp::getType(args[0]);
        if (type==CRP_TYPE_HINT)
        {
            if (g_debug)
                m_print += printType(*(uint32_t *)args[0]) + " data\n";
            if (*(uint32_t *)args[0]==FOURCC('E','V','T','1'))
            {
                uint32_t event = *(uint32_t *)args[1];
                if (event==EVT_PARAM_CHANGE)
                    loadParams();
            }
            else
            {
                // check modules, see if they handle the fourcc
                for (i=0; i<m_modules.size(); i++)
                {
                    if (m_modules[i]->render(*(uint32_t *)args[0], args+1))
                        break;
                }
            }
        }
        else if (type==CRP_HSTRING)
        {
            m_print +=  (char *)args[0];
            color = Qt::blue;
        }
        else
            DBG("unknown type: %d", type);
    }
    if (m_print.size()>0 && !m_print.endsWith('\n'))
        m_print += '\n';


    // wait queue business keeps worker thread from getting too far ahead of gui thread
    // (when this happens, things can get sluggish.)
    // update: this causes the worker thread to block.  For example, bringing up a file
    // dialog will cause the gui to block and a block in the gui causes ths console to block
    // the console blocks and the xdata console print blocks, blocking the worker thread.
    // Removing this fixes the issue.  But it's now possible for the console prints to queue
    // up if they are coming in too fast, causing gui sluggishness.  Limit size of queue?
    // Need some kind of throttling mechanism -- putting sleeps in the worker thread?  Or
    // a call somewhere to processevents?
#if 0
    if (m_localProgramRunning || m_running)
        m_console->m_mutexPrint.lock();
#endif
    if (m_print.size()>0)
    {
        emit textOut(m_print, color);
        m_print = "";
    }
#if 0
    if (m_localProgramRunning || m_running)
    {
        m_console->m_waitPrint.wait(&m_console->m_mutexPrint);
        m_console->m_mutexPrint.unlock();
    }
#endif
}

int Interpreter::addProgram(ChirpCallData data)
{
    QMutexLocker locker(&m_mutexProg);

    m_program.push_back(data);

    return 0;
}

int Interpreter::addProgram(const QStringList &argv)
{
    QMutexLocker locker(&m_mutexProg);

    m_programText.push_back(argv);

    return 0;
}


void Interpreter::getRunning()
{
    int res, running;

    res = m_chirp->callSync(m_exec_running, END_OUT_ARGS, &running, END_IN_ARGS);
    DBG("running %d %d", res, running);
    if (res<0 && !m_notified)
    {
        running = false;
        m_notified = true;
        emit connected(PIXY, false);
    }
    // emit state if we've changed
    if (m_running!=running)
    {
        if (running==2)
            m_fastPoll = true;
        else
            m_fastPoll = false;
        m_running = running;
        emit runState(running);
        emit enableConsole(!running);
    }
}

int Interpreter::sendRun()
{
    int res, response;

    m_fastPoll = true;

    res = m_chirp->callSync(m_exec_run, END_OUT_ARGS, &response, END_IN_ARGS);
    if (res<0)
        return res;
    return response;
}

int Interpreter::sendStop()
{
    int res, response;

    m_fastPoll = true;

    res = m_chirp->callSync(m_exec_stop, END_OUT_ARGS, &response, END_IN_ARGS);
    if (res<0)
        return res;
    return response;
}


int Interpreter::sendGetAction(int index)
{
    int res, response;
    char *action, *scriptlet;
    QStringList scriptlet2;
    QString action2;

    res = m_chirp->callSync(m_exec_get_action, UINT16(index), END_OUT_ARGS, &response, &action, &scriptlet, END_IN_ARGS);

    if (res<0)
        return res;

    if (response<0)
        return response;

    action2 = QString(action);
    scriptlet2 = parseScriptlet(scriptlet);

    emit actionScriptlet(action2, scriptlet2);
    return response;
}


void Interpreter::handlePendingCommand()
{
    QMutexLocker locker(&m_mutexQueue);
    if (m_commandQueue.empty())
        return;
    const Command command = m_commandQueue.front();
    m_commandQueue.pop_front();
    locker.unlock();

    switch (command.m_type)
    {
    case STOP:
        sendStop();
        break;

    case RUN:
        sendRun();
        break;

    case STOP_LOCAL:
        m_localProgramRunning = false;
        emit runState(false);
        break;

    case RUN_LOCAL:
        if (m_program.size()>0)
        {
            m_localProgramRunning = true;
            emit runState(true);
            emit enableConsole(false);
            m_pc = 0;
        }
        break;

    case LOAD_PARAMS:
        handleLoadParams();
        break;

    case SAVE_PARAMS:
        handleSaveParams(command.m_arg0.toBool());
        break;

    case UPDATE_PARAM:
        handleUpdateParam();
        break;

    case CLOSE:
        emit runState(-1);
        break;
    }
}


void Interpreter::queueCommand(CommandType type, const QVariant &arg0, const QVariant &arg1)
{
    Command command(type, arg0, arg1);
    m_mutexQueue.lock();
    // we only want one type of each command queued up at a time,
    // otherwise we might "wind up".  And we can't block, or the gui will either deadlock
    // or become sluggish, so instead remove all of this type of command before adding.
    m_commandQueue.removeAll(command);
    m_commandQueue.push_back(command);
    m_mutexQueue.unlock();
}


int Interpreter::saveImage(const QString &filename)
{
    return m_renderer->saveImage(filename);
}

void Interpreter::run()
{
    int res;
    QTime time;
    QString paramScriptlet;

    // init
    try
    {
        int i;
        ChirpProc versionProc, versionType;
        uint16_t *ver;
        char *type;
        uint32_t verLen, responseInt;

        if (m_link.open()<0)
            throw std::runtime_error("Unable to open USB device.");
        m_chirp = new ChirpMon(this, &m_link);        

        // get version and compare
        versionProc = m_chirp->getProc("version");
        if (versionProc<0)
            throw std::runtime_error("Can't get firmware version.");
        res = m_chirp->callSync(versionProc, END_OUT_ARGS, &responseInt, &verLen, &ver, END_IN_ARGS);
        if (res<0)
            throw std::runtime_error("Can't get firmware version.");
        memcpy(m_version, ver, 3*sizeof(uint16_t));
        if (m_version[0]!=VER_MAJOR || m_version[1]>VER_MINOR)
        {
            char buf[0x100];
            sprintf(buf, "This Pixy's firmware version (%d.%d.%d) is not compatible with this PixyMon version (%d.%d.%d).",
                    m_version[0], m_version[1], m_version[2], VER_MAJOR, VER_MINOR, VER_BUILD);
            throw std::runtime_error(buf);
        }
        versionType = m_chirp->getProc("versionType");
        if (versionType>=0)
        {
            res = m_chirp->callSync(versionType, END_OUT_ARGS, &responseInt, &type, END_IN_ARGS);
            if (res==0 && responseInt==0)
                m_versionType = type;
        }
        else
            m_versionType = "general";
        emit version(m_version[0], m_version[1], m_version[2], m_versionType);

        m_exec_run = m_chirp->getProc("run");
        m_exec_running = m_chirp->getProc("running");
        m_exec_stop = m_chirp->getProc("stop");
        m_exec_get_action = m_chirp->getProc("getAction");
        m_get_param = m_chirp->getProc("prm_get");
        m_getAll_param = m_chirp->getProc("prm_getAll");
        m_set_param = m_chirp->getProc("prm_set");
        m_reload_params = m_chirp->getProc("prm_reload");
        m_set_shadow_param = m_chirp->getProc("prm_setShadow");
        m_reset_shadows = m_chirp->getProc("prm_resetShadows");

        if (m_exec_run<0 || m_exec_running<0 || m_exec_stop<0 || m_exec_get_action<0 ||
                m_get_param<0 || m_getAll_param<0 || m_set_param<0 || m_reload_params<0 ||
                m_set_shadow_param<0 || m_reset_shadows<0)
            throw std::runtime_error("Hmm... missing procedures.");

        // create pixymon modules
        m_modules.push_back(m_renderer); // add renderer to monmodule list so we can send it updates, etc
        MonModuleUtil::createModules(&m_modules, this);
        // reload any parameters that the mon modules might have created
        m_pixymonParameters->load();
        // notify mon modules of parameter change
        sendMonModulesParamChange();
        // load debug
        m_pixymonParameters->clean();

        // get all actions
        for (i=0; sendGetAction(i)>=0; i++);
    }
    catch (std::runtime_error &exception)
    {
        emit error(QString(exception.what()) + '\n');
        return;
    }
    DBG("*** init done");

    time.start();
    getRunning();
    paramScriptlet = m_pixymonParameters->value("Pixy start command").toString();
    paramScriptlet.remove(QRegExp("^\\s+")); // remove initial whitespace
    handleLoadParams(); // load params upon initialization
    if (m_initScript!="")
        execute(parseScriptlet(m_initScript));
    else if (paramScriptlet!="")
        execute(paramScriptlet);


    while(m_run)
    {
        // poll to see if we're still connected
        if (!m_programming &&
                ((m_fastPoll && time.elapsed()>RUN_POLL_PERIOD_FAST) ||
                (!m_fastPoll && time.elapsed()>RUN_POLL_PERIOD_SLOW)))
        {
            getRunning();
            time.start();
        }
        // service chirps -- but if we're running a local program it just slows things down
        else if (!m_localProgramRunning)
        {
            m_chirp->service(false);
            msleep(1); // give config thread time to run
        }
        handlePendingCommand();
        handleLocalProgram();
        if (!m_running && !m_localProgramRunning)
        {
            emit enableConsole(true);
            Sleeper::msleep(10);
            if (m_mutexProg.tryLock())
            {
                if (m_argv.size())
                {
                    if (m_argv[0]=="help")
                        handleHelp();
                    else
                    {
                        res = call(m_argv, true);
                        if (res<0)
                        {
                            if (m_programming)
                            {
                                endLocalProgram();
                                clearLocalProgram();
                            }
                            m_commandList.clear(); // abort our little scriptlet
                        }
                    }
                    m_argv.clear();
                    // check quickly to see if we're running after this command
                    if (!m_programming)
                        getRunning();
                    // is there another command in our little scriptlet?
                    if (m_commandList.size())
                    {
                        execute(m_commandList[0]);
                        m_commandList.removeFirst();
                    }
                }
                m_mutexProg.unlock();
            }
        }
    }
    DBG("worker thead exiting");
}


int Interpreter::beginLocalProgram()
{
    if (m_programming)
        return -1;
    m_program.clear();
    m_programming = true;
    return 0;
}

int Interpreter::endLocalProgram()
{
    if (!m_programming)
        return -1;
    m_pc = 0;
    m_programming = false;
    return 0;
}

void Interpreter::runOrStopProgram(bool local)
{
    unwait(); // unhang ourselves if we're waiting
    if (local)
    {
        if (m_running)
            queueCommand(STOP);
        else if (m_localProgramRunning)
            queueCommand(STOP_LOCAL);
        else
            queueCommand(RUN_LOCAL);
    }
    else
    {
        if (m_localProgramRunning)
            queueCommand(STOP_LOCAL);
        else if (m_running==false)
            queueCommand(RUN);
        else if (m_running==true)
            queueCommand(STOP);
        // note m_running has 3 states...
    }
}

uint Interpreter::programRunning()
{
    if (m_localProgramRunning)
        return m_localProgramRunning;
    if (m_running>0)
        return m_running;
    return false;
}


int Interpreter::clearLocalProgram()
{
    QMutexLocker locker(&m_mutexProg);
    uint i;

    for (i=0; i<m_program.size(); i++)
        delete [] m_program[i].m_buf;
    m_program.clear();
    m_programText.clear();

    return 0;
}

void Interpreter::listProgram()
{
    QMutexLocker locker(&m_mutexProg);
    QString print;
    unsigned int i;
    int j;

    for (i=0; i<m_programText.size(); i++)
    {
        if (m_programText[i].size()<1)
            continue;

        print += QString::number(i+1) + ": " + m_programText[i][0] + " ";
        for (j=1; j<m_programText[i].size(); j++)
        {
            if (j>1)
                print += ", ";
            print += m_programText[i][j];
        }
        print += "\n";
     }

    emit textOut(print);
}

void Interpreter::prompt()
{
    if (m_programming)
        emit prompt(QString("prog") + PROMPT);
    else
        emit prompt(PROMPT);
}

void Interpreter::command(const QString &command)
{
    QMutexLocker locker(&m_mutexInput);

    if (m_localProgramRunning)
        return;

    QStringList words = command.split(QRegExp("[\\s(),\\t]"), QString::SkipEmptyParts);

    if (m_waiting)
    {
        m_command = command;
        m_command.remove(QRegExp("[(),\\t]"));
        m_key = (Qt::Key)0;
        m_selection = RectA(0, 0, 0, 0);
        m_waitInput.wakeAll();
        goto end;
    }

    if (words.size()==0)
        goto end;

    if (words[0]=="do")
    {
        clearLocalProgram();
        beginLocalProgram();
    }
    else if (words[0]=="done")
    {
        endLocalProgram();
        locker.unlock();
        runOrStopProgram(true);
        locker.relock();
    }
    else if (words[0]=="list")
        listProgram();
    else if (words[0].left(4)=="cont")
    {
        locker.unlock();
        runOrStopProgram(true);
        locker.relock();
    }
    else if (words[0]=="close")
        queueCommand(CLOSE);
    else
        handleCall(words);
end:
    prompt();
}

void Interpreter::controlKey(Qt::Key key)
{
    m_command = "";
    m_key = key;
    m_selection = RectA(0, 0, 0, 0);
    m_waitInput.wakeAll();
    if (m_programming)
        endLocalProgram();
}


void Interpreter::handleHelp()
{
    ChirpProc proc;
    ProcInfo info;

    if (m_argv.size()==1)
        printHelp();
    else if (m_argv.size()>1)
    {
        if ((proc=m_chirp->getProc(m_argv[1].toLocal8Bit()))>=0 && m_chirp->getProcInfo(proc, &info)>=0)
            emit textOut(printProc(&info, 1));
        else
            emit error("can't find procedure.\n");
    }
}


void Interpreter::handleCall(const QStringList &argv)
{
    m_mutexProg.lock();
    m_argv = argv;
    m_mutexProg.unlock();
}

void Interpreter::execute(QString command)
{
    if (m_running==true)
        queueCommand(STOP);
    if (m_localProgramRunning)
        queueCommand(STOP_LOCAL);

    command.remove(QRegExp("^\\s+")); // remove leading whitespace
    if (command!="")
        emit consoleCommand(command);
}

void Interpreter::execute(QStringList commandList)
{
    if (commandList.size()==0)
        return;
    execute(commandList[0]);
    commandList.removeFirst();
    if (commandList.size()>0)
    {
        m_mutexProg.lock();
        m_commandList = commandList;
        m_mutexProg.unlock();
    }
}


void Interpreter::loadParams()
{
    queueCommand(LOAD_PARAMS);
}

void Interpreter::saveParams(bool reject)
{
    queueCommand(SAVE_PARAMS, reject);
}

void Interpreter::handleSelection(int x0, int y0, int width, int height)
{
    m_mutexInput.lock();
    m_command = QString::number(x0) + " " + QString::number(y0) +  " " + QString::number(width) +  " " + QString::number(height) + "\n";
    textOut(m_command);
    m_selection = RectA(x0, y0, width, height);
    m_key = (Qt::Key)0;
    m_waitInput.wakeAll();
    m_mutexInput.unlock();
}

void Interpreter::unwait()
{
    QMutexLocker locker(&m_mutexInput);
    if (m_waiting)
    {
        m_selection = RectA(0, 0, 0, 0);
        m_key = Qt::Key_Escape;
        m_waitInput.wakeAll();
        emit videoInput(VideoWidget::NONE);
    }
}

uint16_t *Interpreter::getVersion()
{
    return m_version;
}

QString Interpreter::getVersionType()
{
    return m_versionType;
}

int Interpreter::call(const QStringList &argv, bool interactive)
{
    ChirpProc proc;
    ProcInfo info;
    void *args[10];
    int i, j, k, n, base, res=-1;
    bool ok;
    uint type;
    ArgList list;
    const char *cstrings[11];

    memset(cstrings, 0, sizeof(cstrings));

    // not allowed
    if (argv.size()<1)
        goto end;

    // check modules to see if they handle this command, if so, skip to end
    emit enableConsole(false);
    for (i=0; i<m_modules.size(); i++)
    {
        if (m_modules[i]->command(argv))
        {
            res = 0;
            goto end;
        }
    }

    // a procedure needs extension info (arg info, etc) in order for us to call...
    if ((proc=m_chirp->getProc(argv[0].toLocal8Bit()))>=0 &&
            m_chirp->getProcInfo(proc, &info)>=0)
    {
        memset(args, 0, sizeof(args)); // zero args
        getArgs(&info, &list);
        n = strlen((char *)info.argTypes);

        // if we have fewer args than required...
        if ((int)list.size()>argv.size()-1)
        {
            // if we're interactive, ask for values
            if (interactive && argv.size()>0)
            {
                QStringList cargv = argv;
                QString pstring, pstring2;
                for (i=cargv.size()-1; i<(int)list.size(); i++)
                {
                    if (info.argTypes[i]==CRP_TYPE_HINT)
                    {
                        if (n>i+4)
                        {
                            type = *(uint *)&info.argTypes[i+1];
                            if (type==FOURCC('R','E','G','1'))
                            {
                                emit videoInput(VideoWidget::REGION);
                                pstring2 = "(select region with mouse)";
                            }
                            if (type==FOURCC('P','N','T','1'))
                            {
                                emit videoInput(VideoWidget::POINT);
                                pstring2 = "(select point with mouse)";
                            }
                        }
                    }
                    k = i;
                    pstring = printArgType(&info.argTypes[i], i) + " " + list[k].first +
                            (list[k].second=="" ? "?" : " (" + list[k].second + ")?") + " " + pstring2;

                    emit enableConsole(true);
                    emit prompt(pstring);
                    m_mutexInput.lock();
                    m_waiting = true;
                    m_waitInput.wait(&m_mutexInput);
                    m_waiting = false;
                    m_mutexInput.unlock();
                    emit prompt(PROMPT);
                    emit enableConsole(false);

                    if (m_key==Qt::Key_Escape)
                        goto end;
                    cargv << m_command.split(QRegExp("\\s+"));
                }
                // call ourselves again, now that we have all the args
                res = call(cargv, true);
                goto end;
            }
            else
            {
                emit error("too few arguments.\n");
                goto end;
            }
        }


        augmentProcInfo(&info);
        // if we have all the args we need, parse, put in args array
        for (i=0, j=0; m_argTypes[i]; i++)
        {
            if (argv.size()>i+1)
            {
                if (m_argTypes[i]==CRP_INT8 || m_argTypes[i]==CRP_INT16 || m_argTypes[i]==CRP_INT32)
                {
                    if (argv[i+1].left(2)=="0x")
                        base = 16;
                    else
                        base = 10;
                    args[i] = (void *)argv[i+1].toInt(&ok, base);
                    if (!ok)
                    {
                        emit error("argument didn't parse.\n");
                        goto end;
                    }
                }
                else if (m_argTypes[i]==CRP_STRING)
                {
                    char *cstr = new char[128];
                    QByteArray ba = argv[i+1].toUtf8();
                    const char *csstr = ba.constData();
                    strcpy(cstr, csstr);
                    cstrings[j] = cstr;
                    args[i] = (void *)cstrings[j];
                    j++;
                }
                else
                {
                    // deal with non-integer types
                    goto end;
                }
            }
        }
#if 0
        // print helpful chirp argument string
        if (interactive && argv.size()>1)
        {
            QString callString = "Chirp arguments for " + argv[0] +
                    " (ChirpProc=" + QString::number(proc) + "): ";
            for (i=1; i<argv.size(); i++)
            {
                if (i>1)
                    callString += ", ";
                j = i;
                callString += printArgType(&m_argTypes[i-1], i) + "(" + argv[j] + ")";
            }
            emit textOut(callString + "\n");
        }
#endif

        // make chirp call
        res = m_chirp->callAsync(proc, m_argTypes[0], args[0], m_argTypes[1], args[1], m_argTypes[2], args[2],
                m_argTypes[3], args[3], m_argTypes[4], args[4], m_argTypes[5], args[5], m_argTypes[6], args[6],
                m_argTypes[7], args[7], m_argTypes[8], args[8], m_argTypes[9], args[9], END_OUT_ARGS);

        // check for cable disconnect
        if (res<0 && !m_notified) //res==LIBUSB_ERROR_PIPE)
        {
            m_notified = true;
            emit connected(PIXY, false);
            goto end;
        }
        // get response if we're not programming, save text if we are
        if (m_programming)
            addProgram(argv);
        else
            m_chirp->serviceChirp();
    }
    else
    {
        emit error("procedure unsupported.\n");
        goto end;
    }

    res = 0;

    end:
    for (i=0; cstrings[i]; i++)
        delete [] cstrings[i];
    return res;
}

void Interpreter::augmentProcInfo(ProcInfo *info)
{
    int i, n;
    uint type;
    uint8_t types[0x100];

    n = strlen((char *)info->argTypes);
    memcpy(m_argTypes, info->argTypes, n+1);
    for (i=0; m_argTypes[i]; i++)
    {
        if (m_argTypes[i]==CRP_TYPE_HINT)
        {
            if (n>i+4)
            {
                type = *(uint *)&m_argTypes[i+1];
                memcpy(types, &m_argTypes[i+5], n-(i+5)+1);
                if (type==FOURCC('R','E','G','1'))
                {
                    m_argTypes[i++] = CRP_UINT16;
                    m_argTypes[i++] = CRP_UINT16;
                    m_argTypes[i++] = CRP_UINT16;
                    m_argTypes[i++] = CRP_UINT16;
                }
                else if (type==FOURCC('P','N','T','1'))
                {
                    m_argTypes[i++] = CRP_UINT16;
                    m_argTypes[i++] = CRP_UINT16;
                }
                memcpy(&m_argTypes[i], types, strlen((char *)types)+1);
                i += strlen((char *)types);
            }
        }
    }
}

QString Interpreter::extractProperty(const QString &tag, QString *desc)
{
    QString property;
    QStringList words = desc->split(QRegExp("\\s+"));

    int i = words.indexOf(tag);
    if (i>=0 && words.size()>i+1)
    {
        property = words[i+1];
        *desc = desc->remove(QRegExp(tag + "\\s+" + property + "\\s*")); // remove from description

        return property;
    }
    return "";
}

void Interpreter::handleProperties(const uint8_t *argList, Parameter *parameter, QString *desc)
{
    QString property;
    QStringList halves;
    int val;
    bool ok;

    if ((property=extractProperty("@c", desc))!="")
    {
        property = property.replace('_', ' '); // make it look prettier
        parameter->setProperty(PP_CATEGORY, property);
    }

    if ((property=extractProperty("@m", desc))!="")
    {
        if (argList[0]==CRP_FLT32)
            parameter->setProperty(PP_MIN, property.toFloat());
        else
            parameter->setProperty(PP_MIN, property.toInt());
    }

    if ((property=extractProperty("@M", desc))!="")
    {
        if (argList[0]==CRP_FLT32)
            parameter->setProperty(PP_MAX, property.toFloat());
        else
            parameter->setProperty(PP_MAX, property.toInt());
    }

    while(1)
    {
        if ((property=extractProperty("@s", desc))=="")
            break;
        halves = property.split('=', QString::SkipEmptyParts);
        if (halves.length()<2)
            continue;  // bogus!
        val = halves[0].toInt(&ok);
        if (!ok)
            continue; // bogus also!
        halves[1] = halves[1].replace('_', ' '); // make it look prettier
        parameter->addRadioValue(RadioValue(halves[1], val));
    }
}


void Interpreter::handleLoadParams()
{
    DBG("loading...");
    uint i;
    char *id, *desc;
    uint32_t len;
    uint32_t flags;
    int response, res;
    uint8_t *data, *argList;
    int running;

    // if we're running, stop so this doesn't take too long....
    // (ie it would proceed with 1 property to returned frame, which could take 1 second or 2)
    running = m_running;
    if (running==1) // only if we're running and not in forced state (running==2)
    {
        sendStop();
        while(m_running) // poll for stop
            getRunning();
    }
    for (i=0; true; i++)
    {
        QString category;

        res = m_chirp->callSync(m_getAll_param, UINT16(i), END_OUT_ARGS, &response, &flags, &argList, &id, &desc, &len, &data, END_IN_ARGS);
        if (res<0)
            break;

        if (response<0)
            break;

        QString sdesc(desc);
        Parameter parameter(id, (PType)argList[0]);
        parameter.setProperty(PP_FLAGS, flags);
        handleProperties(argList, &parameter, &sdesc);
        parameter.setHelp(sdesc);

        // deal with param category

        if (strlen((char *)argList)>1)
        {
            QByteArray a((char *)data, len);
            parameter.set(a);
        }
        else
        {
            if (argList[0]==CRP_INT8 || argList[0]==CRP_INT16 || argList[0]==CRP_INT32)
            {
                int32_t val = 0;
                Chirp::deserialize(data, len, &val, END);
                parameter.set(val);
            }
            else if (argList[0]==CRP_FLT32)
            {
                float val;
                Chirp::deserialize(data, len, &val, END);
                parameter.set(val);
            }
            else if (argList[0]==CRP_STRING)
            {
                QString string((char *)data+1); // skip first byte (type)
                parameter.set(string);
            }
            else // not sure what to do with it, so we'll save it as binary
            {
                QByteArray a((char *)data, len);
                parameter.set(a);
            }
        }
        // it's changed! (ie, it's been loaded)
        parameter.setDirty(true);
        m_pixyParameters.add(parameter);
    }

    // if we're running, we've stopped, now resume
    if (running==1)
    {
        sendRun();
        m_fastPoll = false; // turn off fast polling...
    }

    DBG("loaded");
    emit paramLoaded();
    sendMonModulesParamChange();
    m_pixyParameters.clean();

}

void Interpreter::handlePixySaveParams(bool shadow)
{
    int i, res, response;
    QVariant var;
    bool send, reload=false;
    Parameters &pixyParameters = m_pixyParameters.parameters();

    for (i=0; i<pixyParameters.size(); i++)
    {
        uint8_t buf[0x100];

        send = false;

        m_pixyParameters.mutex()->lock();
        if (pixyParameters[i].dirty() && (!shadow || pixyParameters[i].shadow()))
        {
            var = pixyParameters[i].value();
            pixyParameters[i].setDirty(false);
            send = true;
        }
        m_pixyParameters.mutex()->unlock();

        if (send)
        {
            int len;
            QByteArray str = pixyParameters[i].id().toUtf8();
            const char *id = str.constData();
            PType type = pixyParameters[i].type();

            if (type==PT_INT8 || type==PT_INT16 || type==PT_INT32)
            {
                int val = var.toInt();
                len = Chirp::serialize(NULL, buf, 0x100, type, val, END);
            }
            else if (type==PT_FLT32)
            {
                float val = var.toFloat();
                len = Chirp::serialize(NULL, buf, 0x100, type, val, END);
            }
            else if (type==PT_STRING)
            {
                QByteArray baVal = pixyParameters[i].value().toString().toUtf8();
                const char *val = baVal.constData();
                len = Chirp::serialize(NULL, buf, 0x100, type, val, END);
            }
            else if (type==PT_INTS8)
            {
                QByteArray a = var.toByteArray();
                len = a.size();
                memcpy(buf, a.constData(), len);
            }
            else
                continue; // don't know what to do!

            if (shadow)
                // note, this might fail if a parameter isn't designated a shadow parameter in the firmware
                // but that's ok... not all parameters are shadow-able
                res = m_chirp->callSync(m_set_shadow_param, STRING(id), UINTS8(len, buf), END_OUT_ARGS, &response, END_IN_ARGS);
            else
            {
                res = m_chirp->callSync(m_set_param, STRING(id), UINTS8(len, buf), END_OUT_ARGS, &response, END_IN_ARGS);
                reload = true;
            }
            if (res<0)
            {
                emit error("There was a problem setting a parameter.\n");
                break;
            }
        }
    }
    // reload parameters if we're changed any
    if (reload)
         m_chirp->callSync(m_reload_params, END_OUT_ARGS, &response, END_IN_ARGS);
}

void Interpreter::handleSaveParams(bool reject)
{
    bool running;
    int32_t response;

    // if we're running, stop so this doesn't take too long....
    // (ie it would proceed with 1 property to returned frame, which could take 1 second or 2)
    running = m_running;
    if (running==1) // only if we're running and not in forced state (running==2)
    {
        sendStop();
        while(m_running) // poll for stop
            getRunning();
    }

    // notify monmodules
    sendMonModulesParamChange();
    // save them in pixy
    if (!reject)
        handlePixySaveParams(false);

    // reset the shadow parameters because we've saved all params
    m_chirp->callSync(m_reset_shadows, END_OUT_ARGS, &response, END_IN_ARGS);


    // if we're running, we've stopped, now resume
    if (running==1)
    {
        sendRun();
        m_fastPoll = false; // turn off fast polling...
    }
}

void Interpreter::getSelection(RectA *region)
{
    emit videoInput(VideoWidget::REGION);

    m_mutexInput.lock();
    m_waiting = true;
    m_waitInput.wait(&m_mutexInput);
    m_waiting = false;
    *region = m_selection;
    m_mutexInput.unlock();
}


void Interpreter::getSelection(Point16 *point)
{
    emit videoInput(VideoWidget::POINT);

    m_mutexInput.lock();
    m_waiting = true;
    m_waitInput.wait(&m_mutexInput);
    m_waiting = false;
    point->m_x = m_selection.m_xOffset;
    point->m_y = m_selection.m_yOffset;
    m_mutexInput.unlock();
}

void Interpreter::sendMonModulesParamChange()
{
    for (int i=0; i<m_modules.size(); i++)
        m_modules[i]->paramChange();
}

void Interpreter::cprintf(const char *format, ...)
{
    char buffer[256];
    va_list args;
    va_start (args, format);
    vsprintf (buffer, format, args);
    emit textOut(buffer, Qt::blue);
}

void Interpreter::updateParam()
{
    queueCommand(UPDATE_PARAM);
}

void Interpreter::handleUpdateParam()
{
    // use pixyParameters mutex as mutex between the worker thead in interpreter
    // (here) and the configdialog. If we try to lock both mutexes
    // (pixymonParameters and pixyParameters) we can get into a double mutex deadlock
    // (as a rule, never lock more than 1 mutex at a time)
    m_pixyParameters.mutex()->lock();
    sendMonModulesParamChange();
    m_pixymonParameters->clean();
    m_pixyParameters.mutex()->unlock();

    handlePixySaveParams(true);
}

void Interpreter::emitActionScriptlet(QString action, QStringList scriptlet)
{
    emit actionScriptlet(action, scriptlet);
}
