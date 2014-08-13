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
#include <QDesktopServices>
#if QT_VERSION>QT_VERSION_CHECK(5,0,0)
#include <QStandardPaths>
#endif
#include <QUrl>
#include "mainwindow.h"
#include "pixymon.h"
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

#include "parameters.h"
#include "paramfile.h"

extern ChirpProc c_grabFrame;

MainWindow::MainWindow(int argc, char *argv[], QWidget *parent) :
    QMainWindow(parent),
    m_ui(new Ui::MainWindow)
{
    QCoreApplication::setOrganizationName(PIXYMON_COMPANY);
    QCoreApplication::setApplicationName(PIXYMON_TITLE);

    parseCommandline(argc, argv);
    loadParameters();

    qRegisterMetaType<Device>("Device");

    m_ui->setupUi(this);
    setWindowTitle(PIXYMON_TITLE);

    m_interpreter = 0;
    m_flash = NULL;
    m_pixyConnected = false;
    m_pixyDFUConnected = false;
    m_configDialog = NULL;
    m_initScriptExecuted = false;

    m_waiting = WAIT_NONE;

    m_settings = new QSettings(QSettings::NativeFormat, QSettings::UserScope, PIXYMON_COMPANY, PIXYMON_TITLE);
    m_console = new ConsoleWidget(this);
    m_video = new VideoWidget(this);

    m_ui->imageLayout->addWidget(m_video);
    m_ui->imageLayout->addWidget(m_console);

    m_ui->toolBar->addAction(m_ui->actionPlay_Pause);

    m_ui->actionDefault_program->setIcon(QIcon(":/icons/icons/home.png"));
    m_ui->toolBar->addAction(m_ui->actionDefault_program);    

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
}

MainWindow::~MainWindow()
{
    if (m_connect)
        delete m_connect;

    qDebug("deleting mainWindow");
    // we don't delete any of the widgets because the parent deletes it's children upon deletion

    delete m_settings;

    saveParameters();
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
        m_ui->actionDefault_program->setEnabled(false);
        m_ui->actionRaw_video->setEnabled(false);
        m_ui->actionCooked_video->setEnabled(false);
        m_ui->actionConfigure->setEnabled(false);
        m_ui->actionLoad_Pixy_parameters->setEnabled(false);
        m_ui->actionSave_Pixy_parameters->setEnabled(false);
        m_ui->actionSave_Image->setEnabled(false);
        setEnabledActions(false);
    }
    else if (runstate==2) // we're in forced runstate
    {
        m_ui->actionPlay_Pause->setEnabled(false);
        m_ui->actionDefault_program->setEnabled(false);
        m_ui->actionRaw_video->setEnabled(false);
        m_ui->actionCooked_video->setEnabled(false);
        m_ui->actionConfigure->setEnabled(true);
        m_ui->actionLoad_Pixy_parameters->setEnabled(true);
        m_ui->actionSave_Pixy_parameters->setEnabled(true);
        m_ui->actionSave_Image->setEnabled(true);
        setEnabledActions(false);
    }
    else if (runstate) // runstate==1
    {
        m_ui->actionPlay_Pause->setEnabled(true);
        m_ui->actionDefault_program->setEnabled(true);
        m_ui->actionRaw_video->setEnabled(true);
        m_ui->actionCooked_video->setEnabled(true);
        m_ui->actionConfigure->setEnabled(true);
        m_ui->actionLoad_Pixy_parameters->setEnabled(true);
        m_ui->actionSave_Pixy_parameters->setEnabled(true);
        m_ui->actionSave_Image->setEnabled(true);
        setEnabledActions(true);
    }
    else if (m_pixyConnected) // runstate==0
    {
        m_ui->actionPlay_Pause->setEnabled(true);
        m_ui->actionDefault_program->setEnabled(true);
        m_ui->actionRaw_video->setEnabled(true);
        m_ui->actionCooked_video->setEnabled(true);
        m_ui->actionConfigure->setEnabled(true);
        m_ui->actionLoad_Pixy_parameters->setEnabled(true);
        m_ui->actionSave_Pixy_parameters->setEnabled(true);
        m_ui->actionSave_Image->setEnabled(true);
        setEnabledActions(true);
    }
    else // nothing connected
    {
        m_ui->actionPlay_Pause->setEnabled(false);
        m_ui->actionDefault_program->setEnabled(false);
        m_ui->actionRaw_video->setEnabled(false);
        m_ui->actionCooked_video->setEnabled(false);
        m_ui->actionConfigure->setEnabled(false);
        m_ui->actionLoad_Pixy_parameters->setEnabled(false);
        m_ui->actionSave_Pixy_parameters->setEnabled(false);
        m_ui->actionSave_Image->setEnabled(false);
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
        m_configDialog->close();
        m_configDialog = NULL;
    }
    if (m_interpreter)
    {
        m_waiting = WAIT_EXITTING;
        event->ignore(); // ignore event
        qDebug("closing interpreter");
        m_interpreter->close();
    }
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
                m_interpreter = new Interpreter(m_console, m_video, &m_parameters);

                m_initScriptExecuted = false; // reset so we'll execute for this instance
                connect(m_interpreter, SIGNAL(runState(uint)), this, SLOT(handleRunState(uint)));
                connect(m_interpreter, SIGNAL(finished()), this, SLOT(interpreterFinished()));
                connect(m_interpreter, SIGNAL(connected(Device,bool)), this, SLOT(handleConnected(Device,bool)));
                connect(m_interpreter, SIGNAL(actionScriptlet(int,QString,QStringList)), this, SLOT(handleActionScriptlet(int,QString,QStringList)));
                connect(m_interpreter, SIGNAL(paramLoaded()), this, SLOT(handleLoadParams()));
                clearActions();
                m_interpreter->getAction(0);  // start action query process
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

void MainWindow::clearActions()
{
    uint i;

    for (i=0; i<m_actions.size(); i++)
        m_ui->menuAction->removeAction(m_actions[i]);
    m_actions.clear();
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


void MainWindow::handleActionScriptlet(int index, QString action, QStringList scriptlet)
{
    if (m_interpreter)
    {
        m_interpreter->getAction(index+1);  // get next action, we'll stop getting called back when there are no more actions
        addAction(action, scriptlet);
    }
}

void MainWindow::handleConnected(Device device, bool state)
{
    if (m_configDialog)
    {
        m_configDialog->close();
        m_configDialog = NULL;
    }

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

void MainWindow::handleConfigDialogFinished()
{
    if (m_configDialog)
    {
        m_configDialog = NULL;
        updateButtons();
    }
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
    if (m_interpreter)
        m_interpreter->runOrStopProgram();
}

void MainWindow::on_actionDefault_program_triggered()
{
    if (m_interpreter)
        m_interpreter->execute("runprog 0");
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

void MainWindow::saveParameters()
{
    ParamFile pf;
    QFileInfo fi(docPath(), CONFIGFILE_FILENAME);

    pf.open(fi.absoluteFilePath(), false);
    pf.write(CONFIGFILE_TAG, &m_parameters);
    pf.close();
}

void MainWindow::loadParameters()
{
    // initialize paramters
    QString dp = docPath();

    m_parameters.add(Parameter("Document folder", dp, PT_PATH,
                               "The directory where images and other data files are saved"));

    // now see if config file exists
    QFileInfo fi(dp, CONFIGFILE_FILENAME);
    ParamFile pf;
    if (pf.open(fi.absoluteFilePath(), true)>=0)
    {
        pf.read(CONFIGFILE_TAG, &m_parameters);
        pf.close();
    }
    else // there was an error, so write (or rewrite) config file
        saveParameters();
}

void MainWindow::on_actionConfigure_triggered()
{
    if (m_interpreter)
    {
        m_configDialog = new ConfigDialog(this, m_interpreter);
        connect(m_configDialog, SIGNAL(finished(int)), this, SLOT(handleConfigDialogFinished()));
        m_configDialog->setAttribute(Qt::WA_DeleteOnClose);
        updateButtons();
    }
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


void MainWindow::interpreterFinished()
{
    qDebug("interpreter finished");
    m_interpreter->deleteLater();
    m_interpreter = NULL;
    if (m_waiting==WAIT_EXITTING) // if we're exitting, shut down
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
    if (m_configDialog)
    {
        m_configDialog->close();
        m_configDialog = NULL;
    }
    if (m_interpreter)
    {
        m_waiting = WAIT_EXITTING;
        qDebug("closing interpreter");
        m_interpreter->close();
    }
    else
        close();
}

void MainWindow::on_actionHelp_triggered()
{
    QDesktopServices::openUrl(QUrl("http://charmedlabs.com/pixymonhelp"));
}

void MainWindow::handleLoadParams()
{
    int res;

    if (m_waiting==WAIT_SAVING_PARAMS)
    {
        m_waiting = WAIT_NONE;
        QString dir;
        QFileDialog fd(this);
        fd.setWindowTitle("Please provide a filename for the parameter file");
        dir = docPath();
        fd.setDirectory(QDir(dir));
        fd.setNameFilter("XML file (*.xml)");
        fd.setAcceptMode(QFileDialog::AcceptSave);
        if (fd.exec())
        {
            QStringList flist = fd.selectedFiles();
            if (flist.size()==1)
            {
                ParamFile pf;
                if (pf.open(flist[0], false)>=0)
                {
                    pf.write(PIXY_PARAMFILE_TAG, &m_interpreter->m_pixyParameters);
                    pf.close();
                }
            }
        }
    }
    else if (m_waiting==WAIT_LOADING_PARAMS)
    {
        m_waiting = WAIT_NONE;
        QString dir;
        QFileDialog fd(this);
        fd.setWindowTitle("Please choose a parameter file");
        dir = docPath();
        fd.setDirectory(QDir(dir));
        fd.setNameFilter("XML file (*.xml)");
        if (fd.exec())
        {
            QStringList flist = fd.selectedFiles();
            if (flist.size()==1)
            {
                ParamFile pf;
                pf.open(flist[0], true);
                res = pf.read(PIXY_PARAMFILE_TAG, &m_interpreter->m_pixyParameters);
                pf.close();

                if (res>=0)
                    m_interpreter->saveParams();
            }
        }
    }
}

const QString uniqueFilename(const QDir &dir, const QString &filebase, const QString &extension)
{
    int i;

    for (i=1; true; i++)
    {
        QFileInfo testFile(dir, filebase + QString::number(i) + "." + extension);
        if (!testFile.exists())
            return testFile.absoluteFilePath();
    }
}

QString MainWindow::docPath()
{
    QString path;

#if QT_VERSION>QT_VERSION_CHECK(5,0,0)
    path = QStandardPaths::writableLocation(QStandardPaths::DocumentsLocation);
#else
    path = QDesktopServices::storageLocation(QDesktopServices::DocumentsLocation);
#endif
    QDir dir(path);

    if (!dir.exists(PM_DEFAULT_DATA_DIR))
        dir.mkdir(PM_DEFAULT_DATA_DIR);
    dir.cd(PM_DEFAULT_DATA_DIR);

    return dir.absolutePath();
}

void MainWindow::on_actionSave_Image_triggered()
{
    QString filename;

    if (m_interpreter)
    {
        filename = uniqueFilename(m_parameters.value("Document folder")->toString(), "image", "png");
        m_interpreter->saveImage(filename);
    }
}

void MainWindow::on_actionSave_Pixy_parameters_triggered()
{
    if (m_interpreter)
    {
        m_interpreter->loadParams();
        m_waiting = WAIT_SAVING_PARAMS;
    }
}

void MainWindow::on_actionLoad_Pixy_parameters_triggered()
{
    if (m_interpreter)
    {
        // we need to load recent params from Pixy to compare to the params file
        m_interpreter->loadParams();
        m_waiting = WAIT_LOADING_PARAMS;
    }
}


