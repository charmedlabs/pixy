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
#include "debug.h"
#include <QMessageBox>
#include <QFileDialog>
#include <QMetaType>
#include <QSettings>
#include <QUrl>
#include <QDesktopServices>
#include <QSysInfo>
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
#include "dataexport.h"
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

    qRegisterMetaType<Device>("Device");

    m_ui->setupUi(this);
    setWindowTitle(PIXYMON_TITLE);

    m_interpreter = NULL;
    m_flash = NULL;
    m_pixyConnected = false;
    m_pixyDFUConnected = false;
    m_configDialog = NULL;
    m_fwInstructions = NULL;
    m_fwMessage = NULL;
    m_versionIncompatibility = false;

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

    m_parameters.add("Pixy start command", PT_STRING, "runprogArg 8 1",
        "The command that is sent to Pixy upon initialization");

    // start looking for devices
    m_connect = new ConnectEvent(this);
    if (m_connect->getConnected()==NONE)
        m_console->error("No Pixy devices have been detected.\n");
}

MainWindow::~MainWindow()
{
    if (m_connect)
        delete m_connect;

    DBG("deleting mainWindow");
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
            m_argvFirmwareFile = argv[i];
            m_argvFirmwareFile.remove(QRegExp("[\"']"));
        }
        else if (!strcmp("-initscript", argv[i]) && i+1<argc)
        {
            i++;
            m_initScript = argv[i];
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
        DBG("closing interpreter");
        m_interpreter->close();
    }
}

void MainWindow::handleRunState(int state)
{
    updateButtons();
    if (state==-1 && m_interpreter)
    {
        if (m_configDialog)
        {
            m_configDialog->close();
            m_configDialog = NULL;
        }
        DBG("closing interpreter");
        m_interpreter->close();
    }
}


void MainWindow::connectPixyDFU(bool state)
{
    if (state) // connect
    {
        m_console->print("Pixy programming state detected.\n");
        // close the instuctions dialog if it's open
        if (m_fwInstructions)
            m_fwInstructions->done(0);
        try
        {
            if (!m_pixyDFUConnected)
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
                if (m_argvFirmwareFile!="")
                    program(m_argvFirmwareFile);
                else if (m_firmwareFile!="")
                {
                    program(m_firmwareFile);
                    m_firmwareFile = "";
                }
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
            else if (m_interpreter==NULL)
            {
                m_console->print("Pixy detected.\n");
                clearActions();
                m_interpreter = new Interpreter(m_console, m_video, &m_parameters, m_initScript);

                connect(m_interpreter, SIGNAL(runState(int)), this, SLOT(handleRunState(int)));
                connect(m_interpreter, SIGNAL(finished()), this, SLOT(interpreterFinished())); // thread will send finished event when it exits
                connect(m_interpreter, SIGNAL(connected(Device,bool)), this, SLOT(handleConnected(Device,bool)));
                connect(m_interpreter, SIGNAL(actionScriptlet(QString,QStringList)), this, SLOT(handleActionScriptlet(QString,QStringList)));
                connect(m_interpreter, SIGNAL(paramLoaded()), this, SLOT(handleLoadParams()));
                connect(m_interpreter, SIGNAL(version(ushort,ushort,ushort,QString)), this, SLOT(handleVersion(ushort,ushort,ushort,QString)));
                m_interpreter->start();
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
            DBG("closing interpreter");
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



void MainWindow::handleActionScriptlet(QString action, QStringList scriptlet)
{
    addAction(action, scriptlet);
}

// this is called from ConnectEvent and from Interpreter
void MainWindow::handleConnected(Device device, bool state)
{
    if (m_configDialog)
    {
        m_configDialog->close();
        m_configDialog = NULL;
    }

    // reset versionIncompatibility because we've unplugged, so we'll try again
    if (device==NONE && !state && m_versionIncompatibility)
        m_versionIncompatibility = false;

    // if we're incompatible, don't create/connect to device... wait for next event
    if (m_versionIncompatibility)
        return;

    if (m_connect && state)
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

    contents.sprintf("<b>%s version %d.%d.%d</b><br>", PIXYMON_TITLE, VER_MAJOR, VER_MINOR, VER_BUILD);

    if (m_interpreter)
    {
        QString fwver, fwtype;
        uint16_t *version;
        version = m_interpreter->getVersion();
        fwtype = m_interpreter->getVersionType();
        contents += fwver.sprintf("<b>Pixy firmware version %d.%d.%d ", version[0], version[1], version[2]) + fwtype + " build</b> (queried)<br>";
      }

    contents +=
            "<br>The latest version of PixyMon and Pixy firmware can be downloaded "
            "<a href=\"http://charmedlabs.com/pixylatest\">here</a>.<br><br>"
            "CMUCam5 Pixy and PixyMon are open hardware and open source software "
            "maintained by Charmed Labs and Carnegie Mellon University. "
            "All software and firmware is provided under the GNU "
            "General Public License. Complete source code is available at "
            "<a href=\"http://charmedlabs.com/pixysource\">github.com</a>.<br><br>"
            "Please send problems, suggestions, ideas, inquiries and feedback, positive or negative "
            "(although we do love to hear the positive stuff!) "
            "to <a href=\"mailto:support@charmedlabs.com\">support@charmedlabs.com</a>. "
            "Additional information can be found <a href=\"http://charmedlabs.com/pixy\">here</a> "
            "and <a href=\"http://charmedlabs.com/pixywiki\">here</a>.<br><br>"
            "Thanks for supporting our little guy!";

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
        m_console->print("done!\nPlease wait a few seconds while Pixy resets...\n");
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
    DBG("interpreter finished");
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

void MainWindow::handleFirmware(ushort major, ushort minor, ushort build, const QString &type)
{
    // check executable directory for firmware
    int i;
    QStringList filters;

    filters << "*.hex";
    QString path = QFileInfo(QCoreApplication::applicationFilePath()).absolutePath();
    QDir dir(path);
    QStringList files = dir.entryList(filters);
    QStringList parts, dparts;
    QString fwFilename;
    QString maxVerStr;
    QString ftype;
    ushort fmajor, fminor, fbuild;
    qlonglong ver, currVer, maxVer;

    // find the maximum version in the executable directory
    currVer = ((qlonglong)major<<32) | ((qlonglong)minor<<16) | build;
    for (i=0, maxVer=0; i<files.size(); i++)
    {
        dparts = files[i].split('-');
        if (dparts.size()>1)
        {
            parts = dparts[1].split('.');
            if (parts.size()>=3)
            {
                fmajor = parts[0].toInt();
                fminor = parts[1].toInt();
                fbuild = parts[2].toInt();
                if (dparts.size()>=3)
                {
                    parts = dparts[2].split('.');
                    ftype = parts[0];
                }
                else
                    ftype = "general";
                ver = ((qlonglong)fmajor<<32) | ((qlonglong)fminor<<16) | fbuild;
                if (ver>currVer && ftype.compare(type, Qt::CaseInsensitive)==0)
                {
                    if (ver>maxVer)
                    {
                        maxVer = ver;
                        fwFilename = files[i];
                        maxVerStr = QString::number(fmajor) + "." + QString::number(minor) + "." + QString::number(fbuild) + "-" + ftype;
                    }
                }
            }
        }
    }

#if 0
    bool xp = false;
#ifdef __WINDOWS__
    xp = QSysInfo::WindowsVersion<=QSysInfo::WV_2003;
#endif
#endif
    // if this dialog is already open, close
    if (m_fwInstructions)
        m_fwInstructions->done(0);
    if (m_fwMessage==NULL)
    {
        if (fwFilename!="") // newer version!
        {
            QString str =
                    "There is a more recent firmware version (" + maxVerStr + ") available.<br>"
                    "Your Pixy is running firmware version " + QString::number(major) + "." + QString::number(minor) + "." + QString::number(build) + "-" + type + ".<br><br>"
                    "Would you like to upload this firmware into your Pixy? <i>Psst, click yes!</i>";
            m_fwMessage = new QMessageBox(QMessageBox::Question, "New firmware available!", str, QMessageBox::Yes|QMessageBox::No);
            m_fwMessage->setDefaultButton(QMessageBox::Yes);
            m_fwMessage->exec();
            if (m_fwMessage->standardButton(m_fwMessage->clickedButton())==QMessageBox::Yes)
            {
                m_firmwareFile = QFileInfo(path, fwFilename).absoluteFilePath();
                m_fwInstructions = new QMessageBox(QMessageBox::Information, "Get your Pixy ready!",
                                                   "Please follow these steps to get your Pixy ready to accept new firmware:<br><br>"
                                                   "1. Unplug your Pixy from USB (do this now).<br>"
                                                   "2. Press and hold down the white button on top of Pixy.<br>"
                                                   "3. Plug the USB cable back in while continuing to hold down the button.<br>"
                                                   "4. Release the button after the USB cable is plugged back in.<br>"
                                                   "5. Wait a bit. This window will go away automatically when all is ready<br>"
                                                   "to proceed, but the drivers sometimes take a few minutes to install.");
                //m_fwInstructions->setStandardButtons(QMessageBox::NoButton);
                m_fwInstructions->setModal(false);
                m_fwInstructions->exec();
                m_fwInstructions = NULL;
            }
        }
        else if (m_versionIncompatibility) // otherwise tell them where to find the latest version
        {
            m_fwMessage = new QMessageBox(QMessageBox::Information, "Firmware incompatibility",
                                          "The firmware installed on Pixy is incompatible with this version of PixyMon<br>"
                                          "The latest version of PixyMon and Pixy firmware can be downloaded "
                                          "<a href=\"http://charmedlabs.com/pixylatest\">here</a>.<br><br>"
                                          "(For instructions on how to upload firmware onto your Pixy, go "
                                          "<a href=\"http://charmedlabs.com/pixyfirmwarehowto\">here</a>.)");
            m_fwMessage->exec();
        }
        m_fwMessage = NULL;
    }
}

void MainWindow::handleVersion(ushort major, ushort minor, ushort build, QString type)
{
    QString str;

    if (major!=VER_MAJOR || minor>VER_MINOR)
    {
        // Interpreter will automatically exit if there's a version incompatibility, so no need to close interpreter
        m_versionIncompatibility = true;
    }
    handleFirmware(major, minor, build, type);
    //str = str.sprintf("Pixy firmware version %d.%d.%d.\n", major, minor, build);
    //m_console->print(str);
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
        DBG("closing interpreter");
        m_interpreter->close();
    }
    else
        close();
}

void MainWindow::on_actionHelp_triggered()
{
    QString fwtype;
    if (m_interpreter)
        fwtype = m_interpreter->getVersionType();

    if (fwtype.contains("LEGO", Qt::CaseInsensitive))
        QDesktopServices::openUrl(QUrl("http://charmedlabs.com/pixymonhelp_lego"));
    else
        QDesktopServices::openUrl(QUrl("http://charmedlabs.com/pixymonhelp"));
}

void MainWindow::handleLoadParams()
{
    if (m_waiting==WAIT_SAVING_PARAMS)
    {
        m_waiting = WAIT_NONE;
        QString dir;
        QFileDialog fd(this);
        fd.setWindowTitle("Please provide a filename for the parameter file");
        dir = m_parameters.value("Document folder").toString();
        fd.setDirectory(QDir(dir));
        fd.setNameFilter("Parameter file (*.prm)");
        fd.setAcceptMode(QFileDialog::AcceptSave);
        fd.setDefaultSuffix("prm");
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
}


void MainWindow::on_actionSave_Image_triggered()
{
    QString filename;

    if (m_interpreter)
    {
        filename = uniqueFilename(m_parameters.value("Document folder").toString(), "image", "png");
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
        int res;
        QString dir;
        QFileDialog fd(this);
        fd.setWindowTitle("Please choose a parameter file");
        dir = m_parameters.value("Document folder").toString();
        fd.setDirectory(QDir(dir));
        fd.setNameFilter("Parameter file (*.prm)");
        if (fd.exec())
        {
            QStringList flist = fd.selectedFiles();
            if (flist.size()==1)
            {
                ParamFile pf;
                pf.open(flist[0], true);
                res = pf.read(PIXY_PARAMFILE_TAG, &m_interpreter->m_pixyParameters, true);
                pf.close();

                if (res>=0)
                {
                    m_interpreter->saveParams(); // save parapeters back to pixy
                    m_interpreter->execute("close");
                    m_console->print("Parameters have been successfully loaded!  Resetting...\n");
                }
            }
        }
    }
}


