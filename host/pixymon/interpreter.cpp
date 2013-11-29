#include <stdexcept>
#include <QMessageBox>
#include <QFile>
#include <QDebug>
#include "interpreter.h"
#include "disconnectevent.h"
#include "videowidget.h"
#include "console.h"
#include "mainwindow.h"
#include "renderer.h"
#include "calc.h"

QString printType(uint32_t val, bool parens=false);

Interpreter::Interpreter(ConsoleWidget *console, VideoWidget *video, MainWindow *main)
{
    m_console = console;
    m_video = video;
    m_main = main;
    m_pc = 0;
    m_programming = false;
    m_programRunning = false;
    m_remoteProgramRunning = false;
    m_rcount = 0;
    m_init = true;
    m_exit = false;
    m_chirp = NULL;
    m_disconnect = NULL;

#if 0
    ChirpProc proc, procGet, procGetInfo, procGetAll;
    uint8_t buf[0x40];
    proc = m_chirp->getProc("prm_set");
    procGet = m_chirp->getProc("prm_get");
    procGetInfo = m_chirp->getProc("prm_getInfo");
    procGetAll = m_chirp->getProc("prm_getAll");

    uint32_t dead, baad;
    uint16_t beef;
    uint8_t ab;
    uint8_t *buf2;
    char *desc;
    char *id;
    int response, len, res;
    len = m_chirp->serialize(false, buf, 0x40, UINT32(0xa1b2c3d4), UINT16(0xc1ab), UINT8(0xca), UINT32(0xea12345), END);
    res = m_chirp->callSync(proc, STRING("hello"), UINTS8(len, buf), END_OUT_ARGS, &response, END_IN_ARGS);
    res = m_chirp->callSync(procGet, STRING("hello"), END_OUT_ARGS, &response, &len, &buf2, END_IN_ARGS);
    res = Chirp::deserialize(buf2, len, &dead, &beef, &ab, &baad, END);
    res = m_chirp->callSync(procGetInfo, STRING("hello"), END_OUT_ARGS, &response, &desc, END_IN_ARGS);
    res = m_chirp->callSync(procGetAll, UINT16(0), END_OUT_ARGS, &response, &id, &desc, &len, &buf2, END_IN_ARGS);
    res = m_chirp->callSync(procGetAll, UINT16(1), END_OUT_ARGS, &response, &id, &desc, &len, &buf2, END_IN_ARGS);
    res = m_chirp->callSync(procGetAll, UINT16(2), END_OUT_ARGS, &response, &id, &desc, &len, &buf2, END_IN_ARGS);
    res = m_chirp->callSync(procGetAll, UINT16(3), END_OUT_ARGS, &response, &id, &desc, &len, &buf2, END_IN_ARGS);

#endif

    m_renderer = new Renderer(m_video);
    m_lut = m_renderer->m_blobs.getLut();

    connect(m_console, SIGNAL(textLine(QString)), this, SLOT(command(QString)));
    connect(m_console, SIGNAL(controlKey(Qt::Key)), this, SLOT(controlKey(Qt::Key)));
    connect(this, SIGNAL(textOut(QString, QColor)), m_console, SLOT(print(QString, QColor)));
    connect(this, SIGNAL(error(QString)), m_console, SLOT(error(QString)));
    connect(this, SIGNAL(enableConsole(bool)), m_console, SLOT(acceptInput(bool)));
    connect(this, SIGNAL(prompt(QString)), m_console, SLOT(prompt(QString)));
    connect(this, SIGNAL(videoPrompt(uint)), m_video, SLOT(acceptInput(uint)));
    connect(m_video, SIGNAL(selection(int,int,int,int)), this, SLOT(handleSelection(int,int,int,int)));
    // we necessarily want to execute in the gui thread, so queue
    connect(this, SIGNAL(connected(Device,bool)), m_main, SLOT(handleConnected(Device,bool)), Qt::QueuedConnection);

    start();
}

Interpreter::~Interpreter()
{
    m_exit = true;
    stopProgram();
    m_console->m_mutexPrint.lock();
    m_console->m_waitPrint.wakeAll();
    m_console->m_mutexPrint.unlock();
    wait();
    clearProgram();
    if (m_disconnect)
        delete m_disconnect;
    if (m_chirp)
        delete m_chirp;
}

int Interpreter::execute()
{
    int res;

    while(1)
    {
        for (; m_pc<m_program.size(); m_pc++)
        {
            if (!m_programRunning)
            {
                prompt();
                return 0;
            }
            res = m_chirp->execute(m_program[m_pc]);
            if (res<0)
                return res;
        }
        m_pc = 0;
    }
}


QString Interpreter::printArgType(uint8_t *type, int &index)
{
    if (*type==CRP_INT8)
        return "INT8";
    else if (*type==CRP_INT16)
        return "INT16";
    else if (*type==CRP_INT32)
        return "INT32";
    else if (*type==CRP_TYPE_HINT)
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
        return "?";
}

QString Interpreter::printProc(const ProcInfo *info, int level)
{
    ArgList list;
    QString print;
    QStringList sections;
    int i;

    print = QString(info->procName) + "(";
    if (getArgs(info, &list)<0)
        return "";
    for (i=0; i<(int)list.size(); i++)
    {
        if (i>0)
            print +=  ", ";
        print += printArgType(&info->argTypes[i], i) + " " + list[i].first;
    }
    print += ")\n";

    if (level>0)
    {
        sections = getSections("", info->procInfo);
        if (sections.size()>0)
            print += sections[0] + "\n";
        print += "Parameters:\n";
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
    QMutexLocker locker(&m_chirp->m_mutex);
    ChirpProc p;

    for (p=0; true; p++)
    {
        if (m_chirp->getProcInfo(p, &info)<0)
            break;
        emit textOut(QString::number(p) + ": " + printProc(&info));
    }
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

void Interpreter::handleResponse(void *args[])
{
    // strip off response, add to print string
    m_print = "response " + QString::number(m_rcount++) + ": " +
            QString::number(*(int *)args[0]) + " (0x" + QString::number((uint)*(uint *)args[0], 16) + ") ";

    // render rest of response, if present
    handleData(args+1);
}

void Interpreter::handleData(void *args[])
{
    int i;
    uint8_t type;
    QColor color = CW_DEFAULT_COLOR;

    for(i=0; args[i]; i++)
    {
        type = Chirp::getType(args[i]);
        if (type==CRP_TYPE_HINT)
        {
            m_print += printType(*(uint32_t *)args[i]) + " frame data\n";
            m_renderer->render(*(uint32_t *)args[i], &args[i+1]);
        }
        else if (type==CRP_HSTRING)
        {
            m_print +=  (char *)args[i];
            color = Qt::blue;
        }
    }
    if (m_print.right(1)!="\n")
        m_print += "\n";

    // wait queue business keeps worker thread from getting too far ahead of gui thread
    // (when this happens, things can get sluggish.)
    if (m_programRunning || m_remoteProgramRunning)
        m_console->m_mutexPrint.lock();
    emit textOut(m_print, color);
    m_print = "";
    if (m_programRunning || m_remoteProgramRunning)
    {
        m_console->m_waitPrint.wait(&m_console->m_mutexPrint);
        m_console->m_mutexPrint.unlock();
    }
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

bool Interpreter::checkRemoteProgram()
{
    int res;

    res = getRunning();
    if (res<0)
        return false;

    m_remoteProgramRunning = (bool)res;

    emit runState(m_remoteProgramRunning);
    emit enableConsole(!m_remoteProgramRunning);

    return m_remoteProgramRunning;
}

int Interpreter::stopRemoteProgram()
{
    int i, res;

    res = sendStop();
    if (res<0)
        return -1;

    // poll for 500ms for program to stop
    for (i=0; i<10; i++)
    {
        res = getRunning();
        if (res<0)
            return -1;
        if (res==false)
            return 0;
        msleep(50);
    }
    return -1;
}

int Interpreter::getRunning()
{
    int res, response;
    QMutexLocker locker(&m_chirp->m_mutex);

    res = m_chirp->callSync(m_exec_running, END_OUT_ARGS, &response, END_IN_ARGS);
    if (res<0)
        return res;
    return response ? true : false;
}

int Interpreter::sendRun()
{
    int res, response;
    QMutexLocker locker(&m_chirp->m_mutex);

    res = m_chirp->callSync(m_exec_run, STRING(""), END_OUT_ARGS, &response, END_IN_ARGS);
    if (res<0)
        return res;
    return response;
}

int Interpreter::sendStop()
{
    int res, response;
    QMutexLocker locker(&m_chirp->m_mutex);

    res = m_chirp->callSync(m_exec_stop, END_OUT_ARGS, &response, END_IN_ARGS);
    if (res<0)
        return res;
    return response;
}

void Interpreter::run()
{
    int res;

    if (m_init)
    {
        try
        {
            if (m_link.open()<0)
                throw std::runtime_error("Unable to open USB device.");
            m_chirp = new ChirpMon(this, &m_link);
            m_exec_run = m_chirp->getProc("run");
            m_exec_running = m_chirp->getProc("running");
            m_exec_stop = m_chirp->getProc("stop");
            if (m_exec_run<0 || m_exec_running<0 || m_exec_stop<0)
                throw std::runtime_error("Communication error with Pixy.");
            m_disconnect = new DisconnectEvent(this);
        }
        catch (std::runtime_error &exception)
        {
            emit error(QString(exception.what()));
            return;
        }
        checkRemoteProgram(); // get initial state (is program running or not?)
        m_init = false;
        qDebug() << "*** init done";
    }

    if (m_remoteProgramRunning)
    {
        while(m_remoteProgramRunning)
        {
            m_chirp->m_mutex.lock();
            m_chirp->service(false);
            m_chirp->m_mutex.unlock();
        }
        if (!m_exit)
        {
            stopRemoteProgram();
            prompt();
        }
    }
    else if (m_programRunning)
    {

        res = execute();
        // check for cable disconnect
        if (res) //==LIBUSB_ERROR_PIPE)
        {
            emit connected(PIXY, false);
            return;
        }
    }
    else
    {
        res = call(m_argv, true);

        if (res<0 && m_programming)
        {
            endProgram();
            clearProgram();
        }
        prompt();
    }
}

int Interpreter::beginProgram()
{
    if (m_programming)
        return -1;
    m_programming = true;
    return 0;
}

int Interpreter::endProgram()
{
    if (!m_programming)
        return -1;
    m_pc = 0;
    m_programming = false;
    return 0;
}

int Interpreter::runProgram()
{
    QMutexLocker locker(&m_mutexProg);

    if (m_programRunning || m_program.size()==0)
        return -1;

    m_programRunning = true;

    // start thread
    start();

    m_console->emptyLine(); // don't want to start printing on line with prompt
    emit runState(true);
    emit enableConsole(false);

    return 0;
}

int Interpreter::runRemoteProgram()
{
    int res;

    res = sendRun();
    if (res<0)
        return -1;

    m_remoteProgramRunning = true;

    // start thread
    start();

    m_console->emptyLine(); // don't want to start printing on line with prompt
    emit runState(true);
    emit enableConsole(false);

    return 0;
}



int Interpreter::stopProgram()
{
    if (!m_programRunning && !m_remoteProgramRunning)
        return -1;
    m_programRunning = false;
    m_remoteProgramRunning = false;

    emit runState(false);
    emit enableConsole(true);

    return 0;
}

int Interpreter::clearProgram()
{
    QMutexLocker locker(&m_mutexProg);
    unsigned int i;

    for (i=0; i<m_program.size() && m_programRunning; i++)
    {
        ChirpCallData data = m_program[i];
        delete [] data.m_buf;
    }
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
        emit prompt("prog" + QString::number(m_program.size()+1) + PROMPT);
    else
        emit prompt(PROMPT);
}

void Interpreter::command(const QString &command)
{
    int res;

    if (m_programRunning)
        return;

    if (isRunning())
    {
        QString command2 = command;
        command2.remove(QRegExp("[(),\\t]"));
        m_command = command2;
        m_key = (Qt::Key)0;
        m_waitInput.wakeAll();
        return;
    }

    QStringList words = command.split(QRegExp("[\\s(),\\t]"), QString::SkipEmptyParts);

    if (words.size()==0)
        goto end;

    if (words[0]=="help")
        handleHelp(words);
    else if (words[0]=="do")
    {
        clearProgram();
        beginProgram();
    }
    else if (words[0]=="done")
    {
        endProgram();
        runProgram();
        return;
    }
    else if (words[0]=="list")
        listProgram();
    else if (words[0].left(4)=="cont")
    {
        runProgram();
        return;
    }
    else if (words[0]=="load")
    {
        if (words.size()==1)
            res = loadLut("lut", 1);
        else if (words.size()==2)
            res = loadLut(words[1], 1);
        else if (words.size()==3)
            res = loadLut(words[1], words[2].toInt());
        if (res<0)
            emit textOut("There was an error (bad model number of filename.)\n");
    }
    else if (words[0]=="clear")
    {
        int i;
        for (i=0; i<LUT_SIZE; i++)
            m_lut[i] = 0;
    }
    else if (words[0]=="save")
        writeFrame();
    else if (words[0]=="upload")
        uploadLut();
    else if (words[0]=="rendermode")
    {
        if (words.size()>1)
            m_renderer->setMode(words[1].toInt());
        else
            emit textOut("Missing mode parameter.\n");
    }
    else if (words[0]=="set")
    {
        if (words.size()==3)
        {
            int model;
            words[1].remove(QRegExp("[\\s\\D]+"));
            m_renderer->m_blobs.setLabel(words[1], words[2]);
        }
    }
    else
    {
        handleCall(words);
        return; // don't print prompt
    }

    end:
    prompt();
}

void Interpreter::controlKey(Qt::Key key)
{
    if (isRunning())
    {
        m_command = "";
        m_key = key;
        m_waitInput.wakeAll();
        return;
    }
}


void Interpreter::handleHelp(const QStringList &argv)
{
    ChirpProc proc;
    ProcInfo info;
    QMutexLocker locker(&m_chirp->m_mutex);

    if (argv.size()==1)
        printHelp();
    else if (argv.size()>1)
    {
        if ((proc=m_chirp->getProc(argv[1].toLocal8Bit()))>=0 && m_chirp->getProcInfo(proc, &info)>=0)
            emit textOut(printProc(&info, 1));
        else
            emit error("can't find procedure.\n");
    }
}

int Interpreter::call(const QString &command)
{
    int res;
    QStringList argv = command.split(QRegExp("[\\s(),\\t]"), QString::SkipEmptyParts);

    if (argv.size()==0)
        res = -1;
    else
        res = call(argv);

    if (res<0 && m_programming)
    {
        endProgram();
        clearProgram();
    }

    return res;
}

void Interpreter::handleCall(const QStringList &argv)
{
    m_argv = argv;
    start();
}

int Interpreter::uploadLut()
{
    uint32_t i, sum;
    uint32_t responseInt;
    QMutexLocker locker(&m_chirp->m_mutex);

    for (i=0, sum=0; i<LUT_SIZE; i++)
        sum += m_lut[i];
    qDebug() << sum;
    ChirpProc setmem = m_chirp->getProc("cc_setMemory");
    for (i=0; i<LUT_SIZE; i+=0x100)
        m_chirp->callSync(setmem, UINT32(0x10010000+i), UINTS8(0x100, m_lut+i), END_OUT_ARGS, &responseInt, END_IN_ARGS);

    return 0;
}

int Interpreter::loadLut(const QString &filename, int model)
{
    int i;
    int model0;

    if (model<1 || model>7)
        return -2;

    // DEBUG
#if 0
    uint8_t testLut[LUT_SIZE];
    if (fileIn("lut.bin", (char *)testLut, LUT_SIZE)==0)
        return -1;
    for (int i = 0; i < LUT_SIZE; i++) {
        if (testLut[i] != m_tempLut[i])
            printf("Nope...\n");
    }
#endif

    for (i=0; i<LUT_SIZE; i++)
    {
#if 0
        model0 = m_lut[i]&0x07;
        if (model0==0)
            model0 = 8;
        if (m_tempLut[i] && model<=model0)
            m_lut[i] = (m_tempLut[i]&~0x07) | model;
#endif
        if (m_tempLut[i])
        {
            m_lut[i] = model;
        }
    }
    return 0;
}

int compareUnsigned(const void *a, const void *b)
{
  return ( *(uint8_t *)a - *(uint8_t *)b );
}

int compareSigned(const void *a, const void *b)
{
  return ( *(int8_t *)a - *(int8_t *)b );
}


void minmax(int &min, int &max, int x)
{
    if (x<min)
        min = x;
    if (x>max)
        max = x;
}

#define WIDTH 320
#define HEIGHT 200
#define SATMAX8(v, a)   v > 0xff - a ? 0xff : v + a
#define SATMIN8(v, a)   v < a ? 0 : v - a

void Interpreter::writeFrame()
{
    uint32_t pixels[0x10000];

    int i, j, k;
     uint r, g, b;
    uint8_t h, s, v, c;

    uint8_t *frame = m_renderer->m_frameData;

    for (k=0, i=1; i<HEIGHT; i+=2)
    {
        for (j=1; j<WIDTH; j+=2, k+=3)
        {
            r = frame[i*WIDTH + j];
            g = frame[i*WIDTH - WIDTH + j];
            b = frame[i*WIDTH - WIDTH + j - 1];
            pixels[k] = r;
            pixels[k+1] = g;
            pixels[k+2] = b;
        }
    }

    fileOut("frame", (int *)pixels, k, 3, WIDTH/2, HEIGHT/2);

}

void Interpreter::getStats(int x0, int y0, int width, int height)
{
    uint8_t list[0x10000];
    uint32_t pixels[0x10000];

    int i, j, k;
    int havg;
    uint r, g, b;
    uint8_t h, s, v, c;
    uint8_t hmin, hmax, median, hlb, hub;
    int smin = 0xff, smax = 0, vmin = 0xff, vmax = 0, cmin = 0xff, cmax = 0;
    int8_t hmin2, hmax2;
    int16_t hmin16, hmax16;
    int diff, diff2;

    uint8_t *frame = m_renderer->m_frameData;

    x0 |= 0x01;
    y0 |= 0x01;

    hmin = 0xff;
    hmin2 = 0x7f;
    hmax = 0x00;
    hmax2 = 0x00;

    for (k=0, i=y0; i<y0+height; i+=2)
    {
        for (j=x0; j<x0+width; j+=2, k+=3)
        {
            r = frame[i*WIDTH + j];
            g = frame[i*WIDTH - WIDTH + j];
            b = frame[i*WIDTH - WIDTH + j - 1];
            pixels[k] = r;
            pixels[k+1] = g;
            pixels[k+2] = b;
        }
    }

    // DEBUG
#if 0
    fileOut("pixels", (int *)pixels, k, 3);
#endif

    CLUT::generateFromImgSample(pixels, k, m_tempLut);

    // DEBUG
#if 0
    fileOutDebug(m_tempLut);
#endif

    k = 0;
    havg = 0;

    for (i=y0; i<y0+height; i+=2)
    {
        for (j=x0; j<x0+width; j+=2)
        {
            r = frame[i*WIDTH + j];
            g = frame[i*WIDTH - WIDTH + j];
            b = frame[i*WIDTH - WIDTH + j - 1];
            qDebug() << r << " " << g << " " << b;
            r >>= 3; r <<= 3; g >>= 3; g <<= 3; b >>= 3; b <<= 3;

            hsvc(r, g, b, &h, &s, &v, &c);
            if (s<80 || v<80)
                continue;
            list[k++] = h;
            havg += h;

            if (h<hmin)
                hmin = h;
            if ((int8_t)h<hmin2)
                hmin2 = (int8_t)h;
            if (h>hmax)
                hmax = h;
            if ((int8_t)h>hmax2)
                hmax2 = (int8_t)h;

            minmax(smin, smax, s);
            minmax(vmin, vmax, v);
            minmax(cmin, cmax, c);
            //qDebug() << r << " " << g << " " << b;
        }
    }
    diff = hmax - hmin;
    diff2 = hmax2 - hmin2;

    if (diff<=diff2)
    {
        qsort(list, k, sizeof(uint8_t), compareUnsigned);
        hmin16 = hmin;
        hmax16 = hmax;
    }
    else
    {
        diff = diff2;
        hmin16 = hmin2;
        hmax16 = hmax2;
        qsort(list, k, sizeof(uint8_t), compareSigned);
    }
    if (k)
        havg /= k;
    median = list[k/2];
    hlb = list[k/20];
    hub = list[k - k/20 - 1];
    diff = diff < diff2 ? diff : diff2;

    qDebug() << hmin16 << " " << hmax16;
    cmax = 0xff;
    cmin = 0;
#if 0
    smax = 0xff; //SATMAX8(smax, 20);
    vmax = 0xff; //SATMAX8(vmax , 20);
    cmax = 0xff; //SATMAX8(cmax, 20);
    smin = SATMIN8(smin, 10);
    vmin = SATMIN8(vmin, 20);
    m_renderer->setFilter(hlb, (hlb+hub)/2, hub, smin, smax, vmin, vmax, cmin, cmax);
#endif
#if 1
    smax = SATMAX8(smax, 5);
    vmax = SATMAX8(vmax , 5);
    smin = SATMIN8(smin, 5);
    vmin = SATMIN8(vmin, 5);

    //m_renderer->setFilter(hmin16, (hmin16+hmax16)/2, hmax16, smin, smax, vmin, vmax, cmin, cmax);

#endif


    //qDebug() << diff << " " << hmin << " " << hmin2 << " " << hmax << " " << hmax2;
    //qDebug() << "median: " << median << "hlb: " << hlb << "hub: " << hub;
}

unsigned int Interpreter::fileIn(const QString &name, char *data, unsigned int size)
{
    QFile file("matlab\\" + name);
    if (!file.open(QIODevice::ReadOnly))
        return 0;

    return file.read(data, size);
}

void Interpreter::fileOut(const QString &name, int *data, unsigned int len, unsigned int pitch, int p1, int p2)
{
    unsigned int i, j;
    static int index = 1;
    QString str, namex;
    namex = QString("%1%2").arg(name).arg(index, 2, 10, QLatin1Char('0'));
    QFile file("matlab\\" + namex + ".m");
    if (!file.open(QIODevice::WriteOnly | QIODevice::Text))
        return;

    QTextStream out(&file);

    out << "function [A, b, c]=" << namex << "()\n\n";
    out << "A=[";
    for (i=0; i<len;)
    {
        if (i>0)
            out << ",\n";
        for (j=0; j<pitch; j++)
        {
            if (j>0)
                out << ", ";
            out << str.sprintf("%d", data[i++]);
        }
    }
    out << "];\n";
    out << "b=" << QString::number(p1) << ";\n";
    out << "c=" << QString::number(p2) << ";\n";

    index++;
}

// DEBUG
#if 0
void Interpreter::fileOutDebug(uint8_t *data)
{
    QFile file("pixi.txt");
    if (!file.open(QIODevice::WriteOnly | QIODevice::Text))
        return;

    QTextStream out(&file);

    for (int i = 0; i < 256; i++)
    {
        for (int j = 0; j < 256; j++)
        {
            out << data[(i*256)+j];
        }
        //out << "\n";
    }
}
#endif

void Interpreter::handleSelection(int x0, int y0, int width, int height)
{
#if 0
    m_command = QString::number(x0) + " " + QString::number(y0) +  " " + QString::number(width) +  " " + QString::number(height);
    m_waitInput.wakeAll();
#endif
#if 0
    uint8_t lut[0x8000];
    int i;
    for (i=0; i<0x8000; i++)
        lut[i] = i;
    uploadLut(lut);
#endif
    getStats(x0, y0, width, height);
}

int Interpreter::call(const QStringList &argv, bool interactive)
{
    ChirpProc proc;
    ProcInfo info;
    int args[20];
    int i, j, n, base, res;
    bool ok;
    ArgList list;
    QMutexLocker locker(&m_chirp->m_mutex);

    // not allowed
    if (argv.size()<1)
        return -1;

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
                QString pstring;
                for (i=cargv.size()-1; i<(int)list.size(); i++)
                {
                    if (info.argTypes[i]==CRP_TYPE_HINT)
                    {
                        if (n>i+4)
                        {
                            pstring += "(select region)";
                            emit videoPrompt(*(uint *)&info.argTypes[i+1]);
                            emit enableConsole(false);
                        }

                    }
                    pstring = printArgType(&info.argTypes[i], i) + " " + list[i].first +
                            (list[i].second=="" ? "?" : " (" + list[i].second + ")?");

                    emit prompt(pstring);
                    m_mutexInput.lock();
                    m_waitInput.wait(&m_mutexInput);
                    m_mutexInput.unlock();

                    emit enableConsole(true);

                    if (m_key==Qt::Key_Escape)
                        return -1;
                    cargv << m_command.split(QRegExp("\\s+"))[0];
                }
                // call ourselves again, now that we have all the args
                return call(cargv, true);
            }
            else
            {
                emit error("too few arguments.\n");
                return -1;
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
                    args[j++] = m_argTypes[i];
                    if (argv[i+1].left(2)=="0x")
                        base = 16;
                    else
                        base = 10;
                    args[j++] = argv[i+1].toInt(&ok, base);
                    if (!ok)
                    {
                        emit error("argument didn't parse.\n");
                        return -1;
                    }
                }
#if 0
                else if (m_argTypes[i]==CRP_STRING)
                {
                    args[j++] = m_argTypes[i];
                    // string goes where?  can't cast pointer to int...
                }
#endif
                else
                {
                    // deal with non-integer types
                    return -1;
                }
            }
        }

        // print helpful chirp argument string
        if (interactive && argv.size()>1)
        {
            QString callString = "Chirp arguments for " + argv[0] +
                    " (ChirpProc=" + QString::number(proc) + "): ";
            for (i=1; i<argv.size(); i++)
            {
                if (i>1)
                    callString += ", ";
                callString += printArgType(&m_argTypes[i-1], i) + "(" + argv[i] + ")";
            }
            emit textOut(callString + "\n");
        }

        // make chirp call
        res = m_chirp->callAsync(proc, args[0], args[1], args[2], args[3], args[4], args[5], args[6],
                           args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15],
                           args[16], args[17], args[18], args[19], END_OUT_ARGS);

        // check for cable disconnect
        if (res) //res==LIBUSB_ERROR_PIPE)
        {
            emit connected(PIXY, false);
            return res;
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
        return -1;
    }

    return 0;
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
                //else if (type==FOURCC('F','O','O','1'))

                memcpy(&m_argTypes[i], types, strlen((char *)types)+1);
                i += strlen((char *)types);
            }
        }
    }
}

