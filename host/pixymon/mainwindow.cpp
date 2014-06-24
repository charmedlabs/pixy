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
#include <QDebug>
#include <QMessageBox>
#include <QFileDialog>
#include <QMetaType>
#include <QSettings>
#include "mainwindow.h"
#include "videowidget.h"
#include "console.h"
#include "interpreter.h"
#include "chirpmon.h"
#include "dfu.h"
#include "flash.h"
#include "ui_mainwindow.h"
#include "configdialog.h"
#include "sleeper.h"
#include "aboutdialog.h"

extern ChirpProc c_grabFrame;

MainWindow::MainWindow(int argc, char *argv[], QWidget *parent) :
    QMainWindow(parent),
    m_ui(new Ui::MainWindow)
{
    parseCommandline(argc, argv);

    qRegisterMetaType<Device>("Device");

    m_ui->setupUi(this);
    setWindowTitle(PIXYMON_TITLE);

    m_interpreter = 0;
    m_flash = NULL;
    m_pixyConnected = false;
    m_pixyDFUConnected = false;
    m_exitting = false;
    m_configDialog = NULL;
    m_initScriptExecuted = false;

    m_settings = new QSettings(QSettings::NativeFormat, QSettings::UserScope, PIXYMON_COMPANY, PIXYMON_TITLE);
    m_console = new ConsoleWidget(this);
    m_video = new VideoWidget(this);

    m_ui->imageLayout->addWidget(m_video);
    m_ui->imageLayout->addWidget(m_console);

    m_ui->toolBar->addAction(m_ui->actionPlay_Pause);

    m_ui->actionRaw_video->setIcon(QIcon(":/icons/icons/raw.png"));
    m_ui->toolBar->addAction(m_ui->actionRaw_video);

    m_ui->actionCooked_video->setIcon(QIcon(":/icons/icons/cooked.png"));
    m_ui->toolBar->addAction(m_ui->actionCooked_video);

    m_ui->actionConfigure->setIcon(QIcon(":/icons/icons/config.png"));
    m_ui->toolBar->addAction(m_ui->actionConfigure);

    updateButtons();

    // start looking for devices
    m_connect = new ConnectEvent(this);
    if (m_connect->getConnected()==NONE)
        m_console->error("No Pixy devices have been detected.\n");

    QStringList list;
    list << "runprog 0";
    addAction("Run default program", list);
    list.clear();
    list << "runprog 2";
    addAction("Run pan/tilt demo", list);
    list.clear();
    list << "cam_getFrame 0x21 0 0 320 200";
    list << "cc_setSigRegion 0 1";
    list << "runprogArg 8 1";
    addAction("Set signature 1...", list);
    list.clear();
    list << "cam_getFrame 0x21 0 0 320 200";
    list << "cc_setSigRegion 0 2";
    list << "runprogArg 8 1";
    addAction("Set signature 2...", list);
    list.clear();
    list << "cam_getFrame 0x21 0 0 320 200";
    list << "cc_setSigRegion 0 3";
    list << "runprogArg 8 1";
    addAction("Set signature 3...", list);
    list.clear();
    list << "cam_getFrame 0x21 0 0 320 200";
    list << "cc_setSigRegion 0 4";
    list << "runprogArg 8 1";
    addAction("Set signature 4...", list);
    list.clear();
    list << "cam_getFrame 0x21 0 0 320 200";
    list << "cc_setSigRegion 0 5";
    list << "runprogArg 8 1";
    addAction("Set signature 5...", list);
    list.clear();
    list << "cam_getFrame 0x21 0 0 320 200";
    list << "cc_setSigRegion 0 6";
    list << "runprogArg 8 1";
    addAction("Set signature 6...", list);
    list.clear();
    list << "cam_getFrame 0x21 0 0 320 200";
    list << "cc_setSigRegion 0 7";
    list << "runprogArg 8 1";
    addAction("Set signature 7...", list);

    list.clear();
    list << "cam_getFrame 0x21 0 0 320 200";
    list << "cc_setSigRegion 1 1";
    list << "runprogArg 8 1";
    addAction("Set CC signature 1...", list);
    list.clear();
    list << "cam_getFrame 0x21 0 0 320 200";
    list << "cc_setSigRegion 1 2";
    list << "runprogArg 8 1";
    addAction("Set CC signature 2...", list);
    list.clear();
    list << "cam_getFrame 0x21 0 0 320 200";
    list << "cc_setSigRegion 1 3";
    list << "runprogArg 8 1";
    addAction("Set CC signature 3...", list);
    list.clear();
    list << "cam_getFrame 0x21 0 0 320 200";
    list << "cc_setSigRegion 1 4";
    list << "runprogArg 8 1";
    addAction("Set CC signature 4...", list);
    list.clear();
    list << "cam_getFrame 0x21 0 0 320 200";
    list << "cc_setSigRegion 1 5";
    list << "runprogArg 8 1";
    addAction("Set CC signature 5...", list);
    list.clear();
    list << "cam_getFrame 0x21 0 0 320 200";
    list << "cc_setSigRegion 1 6";
    list << "runprogArg 8 1";
    addAction("Set CC signature 6...", list);
    list.clear();
    list << "cam_getFrame 0x21 0 0 320 200";
    list << "cc_setSigRegion 1 7";
    list << "runprogArg 8 1";
    addAction("Set CC signature 7...", list);


    list.clear();
    list << "cc_clearSig";
    list << "run";
    addAction("Clear signature...", list);
    list.clear();
    list << "cc_clearAllSig";
    list << "run";
    addAction("Clear all signatures", list);
    list.clear();
    list << "prm_restore";
    list << "run";
    addAction("Restore default parameters", list);
}

MainWindow::~MainWindow()
{
    if (m_connect)
        delete m_connect;

    qDebug("deleting mainWindow");
    // we don't delete any of the widgets because the parent deletes it's children upon deletion

    delete m_settings;
}

void MainWindow::parseCommandline(int argc, char *argv[])
{
    int i;

    // when updating to Qt 5, we can use QCommandLineParser
    for (i=1; i<argc; i++)
    {
        if (!strcmp("-firmware", argv[i]) && i+1<argc)
        {
            i++;
            m_firmwareFile = argv[i];
            m_firmwareFile.remove(QRegExp("[\"']"));
        }
        else if (!strcmp("-initscript", argv[i]) && i+1<argc)
        {
            i++;
            QString script = argv[i];
            script.remove(QRegExp("[\"']"));
            m_initScript = script.split(QRegExp("[\\\\]"));
        }

    }
}

void MainWindow::updateButtons()
{
    uint runstate = 0;

    if (m_interpreter && (runstate=m_interpreter->programRunning()))
    {
        m_ui->actionPlay_Pause->setIcon(QIcon(":/icons/icons/stop.png"));
        m_ui->actionPlay_Pause->setToolTip("Stop program");
    }
    else
    {
        m_ui->actionPlay_Pause->setIcon(QIcon(":/icons/icons/play.png"));
        m_ui->actionPlay_Pause->setToolTip("Run program");
    }

    if (m_pixyDFUConnected && m_pixyConnected) // we're in programming mode
    {
        m_ui->actionPlay_Pause->setEnabled(false);
        m_ui->actionRaw_video->setEnabled(false);
        m_ui->actionCooked_video->setEnabled(false);
        m_ui->actionConfigure->setEnabled(false);
        m_ui->actionProgram->setEnabled(true);
        setEnabledActions(false);
    }
    else if (runstate==2) // we're in forced runstate
    {
        m_ui->actionPlay_Pause->setEnabled(false);
        m_ui->actionRaw_video->setEnabled(false);
        m_ui->actionCooked_video->setEnabled(false);
        m_ui->actionConfigure->setEnabled(true);
        m_ui->actionProgram->setEnabled(false);
        setEnabledActions(false);
    }
    else if (runstate) // runstate==1
    {
        m_ui->actionPlay_Pause->setEnabled(true);
        m_ui->actionRaw_video->setEnabled(true);
        m_ui->actionCooked_video->setEnabled(true);
        m_ui->actionConfigure->setEnabled(true);
        m_ui->actionProgram->setEnabled(false);
        setEnabledActions(true);
    }
    else if (m_pixyConnected) // runstate==0
    {
        m_ui->actionPlay_Pause->setEnabled(true);
        m_ui->actionRaw_video->setEnabled(true);
        m_ui->actionCooked_video->setEnabled(true);
        m_ui->actionConfigure->setEnabled(true);
        m_ui->actionProgram->setEnabled(true);
        setEnabledActions(true);
    }
    else // nothing connected
    {
        m_ui->actionPlay_Pause->setEnabled(false);
        m_ui->actionRaw_video->setEnabled(false);
        m_ui->actionCooked_video->setEnabled(false);
        m_ui->actionConfigure->setEnabled(false);
        m_ui->actionProgram->setEnabled(false);
        setEnabledActions(false);
    }

    if (m_configDialog)
        m_ui->actionConfigure->setEnabled(false);
}

void MainWindow::close()
{
    QCoreApplication::exit();
}

void MainWindow::closeEvent(QCloseEvent *event)
{
    // delete interpreter
    if (m_configDialog)
    {
        m_exitting = true;
        event->ignore(); // ignore event
        qDebug("closing dialog");
        m_configDialog->close();
    }
    else if (m_interpreter)
    {
        m_exitting = true;
        event->ignore(); // ignore event
        qDebug("closing interpreter");
        m_interpreter->close();
    }
    // let gui thread run some more, wait for finished() event
}

void MainWindow::handleRunState(uint state)
{
    updateButtons();
    if (state==true && m_initScript.size()>0 && !m_initScriptExecuted)
    {
        m_interpreter->execute(m_initScript);
        m_initScriptExecuted = true;
    }
}

void MainWindow::connectPixyDFU(bool state)
{
    if (state) // connect
    {
        m_console->print("Pixy programming state detected.\n");
        try
        {
            Dfu *dfu;
            dfu = new Dfu();
            QString path = QFileInfo(QCoreApplication::applicationFilePath()).absolutePath();
            QString file = QFileInfo(path, "pixyflash.bin.hdr").absoluteFilePath();
            dfu->download(file);
            m_pixyDFUConnected = true;
            delete dfu;
            Sleeper::msleep(1000);
            m_connect = new ConnectEvent(this);
        }
        catch (std::runtime_error &exception)
        {
            m_pixyDFUConnected = false;
            m_console->error(QString(exception.what())+"\n");
        }
    }
    else // disconnect
    {
        m_console->error("Pixy DFU has stopped working.\n");
        // if we're disconnected, start the connect thread
        m_connect = new ConnectEvent(this);
        m_pixyDFUConnected = false;
    }

    updateButtons();
}

void MainWindow::connectPixy(bool state)
{
    if (state) // connect
    {
        try
        {
            if (m_pixyDFUConnected) // we're in programming mode
            {
                m_flash = new Flash();
                if (m_firmwareFile!="")
                    program(m_firmwareFile);
                else
                {
                    QString dir;
                    QFileDialog fd(this);
                    dir = m_settings->value("fw_dialog").toString();
                    fd.setWindowTitle("Select a Firmware File");
                    fd.setDirectory(QDir(dir));
                    fd.setNameFilter("Firmware (*.hex)");
                    if (fd.exec())
                    {
                        QStringList slist = fd.selectedFiles();
                        if (slist.size()==1 && m_flash)
                        {
                            program(slist.at(0));
                        }
                    }
                    dir = fd.directory().absolutePath();
                    m_settings->setValue("fw_dialog", QVariant(dir));
                }
            }
            else
            {
                m_console->print("Pixy detected.\n");
                m_interpreter = new Interpreter(m_console, m_video);

                m_initScriptExecuted = false; // reset so we'll execute for this instance
                connect(m_interpreter, SIGNAL(runState(uint)), this, SLOT(handleRunState(uint)));
                connect(m_interpreter, SIGNAL(finished()), this, SLOT(interpreterFinished()));
                connect(m_interpreter, SIGNAL(connected(Device,bool)), this, SLOT(handleConnected(Device,bool)));
            }
            m_pixyConnected = true;
        }
        catch (std::runtime_error &exception)
        {
            m_console->error(QString(exception.what())+"\n");
            m_flash = NULL;
            m_pixyDFUConnected = false;
            m_pixyConnected = false;
        }
    }
    else // disconnect
    {
        m_console->error("Pixy has stopped working.\n");
        if (m_interpreter)
        {
            qDebug("closing interpreter");
            m_interpreter->close();
        }
    }

    updateButtons();
}

void MainWindow::setEnabledActions(bool enable)
{
    uint i;

    for (i=0; i<m_actions.size(); i++)
        m_actions[i]->setEnabled(enable);
}

void MainWindow::addAction(const QString &label, const QStringList &command)
{
    QAction *action = new QAction(this);
    action->setText(label);
    action->setProperty("command", QVariant(command));
    m_ui->menuAction->addAction(action);
    m_actions.push_back(action);
    connect(action, SIGNAL(triggered()), this, SLOT(handleActions()));
}

void MainWindow::handleActions()
{
    QAction *action = (QAction *)sender();
    if (m_interpreter)
    {
        QVariant var = action->property("command");
        QStringList list = var.toStringList();
        m_interpreter->execute(list);
    }
}


void MainWindow::handleConnected(Device device, bool state)
{
    if (m_configDialog)
        m_configDialog->close();

    // kill connect thread
    if (m_connect)
    {
        delete m_connect;
        m_connect = NULL;
    }
    if (device==PIXY)
        connectPixy(state);
    else if (device==PIXY_DFU)
        connectPixyDFU(state);
}

void MainWindow::on_actionAbout_triggered()
{
    AboutDialog *about;
    QString contents;

    contents.sprintf("%s version %d.%d.%d\n", PIXYMON_TITLE, VER_MAJOR, VER_MINOR, VER_BUILD);

    if (m_interpreter)
    {
        QString fwver;
        uint16_t *version;
        version = m_interpreter->getVersion();
        contents += fwver.sprintf("Pixy firmware version (queried) %d.%d.%d\n", version[0], version[1], version[2]);
    }

    contents += "\nCMUCam5 Pixy and PixyMon are open hardware and open source software ";
    contents += "maintained by Charmed Labs and Carnegie Mellon University.\n\n";
    contents += "Send problems, suggestions, etc. to cmucam@cs.cmu.edu";

    about = new AboutDialog(contents);
    about->setAttribute(Qt::WA_DeleteOnClose);
    about->show();
}


void MainWindow::on_actionPlay_Pause_triggered()
{
    m_interpreter->runOrStopProgram();
}


void MainWindow::program(const QString &file)
{
    try
    {
        m_console->print("Programming... (" + file + ")\n");
        QApplication::processEvents(); // render message before we continue
        m_flash->program(file);
        m_console->print("done!\n");

    }
    catch (std::runtime_error &exception)
    {
        m_console->error(QString(exception.what())+"\n");
    }

    // clean up...
    delete m_flash;
    m_flash = NULL;
    m_pixyDFUConnected = false;
    m_pixyConnected = false;

    // start looking for devices again (wait 4 seconds before we start looking though)
    m_connect = new ConnectEvent(this, 4000);

}

void MainWindow::on_actionConfigure_triggered()
{
    m_configDialog = new ConfigDialog(this, m_interpreter);
    connect(m_configDialog, SIGNAL(done()), this, SLOT(configFinished()));
    m_configDialog->show();
    updateButtons();
}

void MainWindow::on_actionRaw_video_triggered()
{
    if (m_interpreter)
        m_interpreter->execute("runprog 8");
}

void MainWindow::on_actionCooked_video_triggered()
{
    if (m_interpreter)
        m_interpreter->execute("runprogArg 8 1");
}


void MainWindow::configFinished()
{
    qDebug("config finished");
    m_configDialog->deleteLater();
    m_configDialog = NULL;
    updateButtons();
    // if we're exitting, close interpreter (handin it down the chain...)
    if (m_exitting)
    {
        qDebug("closing interpreter");
        m_interpreter->close();
    }
}

void MainWindow::interpreterFinished()
{
    qDebug("interpreter finished");
    m_interpreter->deleteLater();
    m_interpreter = NULL;
    if (m_exitting) // if we're exitting, shut down
    {
        close();
        return;
    }

    m_video->clear();
    m_console->acceptInput(false);
    // if we're disconnected, start the connect thread
    m_connect = new ConnectEvent(this);
    m_pixyConnected = false;
    updateButtons();
}

void MainWindow::on_actionExit_triggered()
{
    close();
}
