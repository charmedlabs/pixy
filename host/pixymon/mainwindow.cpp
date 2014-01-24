#include <stdexcept>
#include <QDebug>
#include <QMessageBox>
#include <QFileDialog>
#include <QMetaType>
#include "mainwindow.h"
#include "videowidget.h"
#include "console.h"
#include "interpreter.h"
#include "chirpmon.h"
#include "dfu.h"
#include "flash.h"
#include "ui_mainwindow.h"
#include "configdialog.h"

extern ChirpProc c_grabFrame;

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    m_ui(new Ui::MainWindow)
{
    qRegisterMetaType<Device>("Device");

    m_ui->setupUi(this);
    setWindowTitle(PIXYMON_TITLE);

    m_interpreter = 0;
    m_flash = NULL;
    m_pixyConnected = false;
    m_pixyDFUConnected = false;
    m_exitting = false;
    m_configDialog = NULL;

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

    addAction("Run default program", "runprog 1");
    addAction("Run pan/tilt demo", "runprog 2");
    addAction("Set signature 1", "cc_setSigRegion 1");
    addAction("Set signature 2", "cc_setSigRegion 2");
    addAction("Set signature 3", "cc_setSigRegion 3");
    addAction("Set signature 4", "cc_setSigRegion 4");
    addAction("Set signature 5", "cc_setSigRegion 5");
    addAction("Set signature 6", "cc_setSigRegion 6");
    addAction("Set signature 7", "cc_setSigRegion 7");
}

MainWindow::~MainWindow()
{
    if (m_connect)
        delete m_connect;

    // we don't delete any of the widgets because the parent deletes it's children upon deletion
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
            dfu->download("pixyflash.bin.hdr");
            m_pixyDFUConnected = true;
            delete dfu;
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
                try
                {
                    m_flash = new Flash();
                }
                catch (std::runtime_error &exception)
                {
                    m_console->error(QString(exception.what())+"\n");
                    m_flash = NULL;
                    m_pixyDFUConnected = false;
                }
            }
            else
            {
                m_console->print("Pixy detected.\n");
                m_interpreter = new Interpreter(m_console, m_video, this);

                connect(m_interpreter, SIGNAL(runState(uint)), this, SLOT(handleRunState(uint)));
                connect(m_interpreter, SIGNAL(finished()), this, SLOT(interpreterFinished()));
            }
            m_pixyConnected = true;
        }
        catch (std::runtime_error &exception)
        {
            m_console->error(QString(exception.what())+"\n");
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

void MainWindow::addAction(const QString &label, const QString &command)
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
        m_interpreter->execute(var.toString());
    }
}


void MainWindow::handleConnected(Device device, bool state)
{
    if (m_configDialog)
    {
        delete m_configDialog;
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

void MainWindow::on_actionPlay_Pause_triggered()
{
    m_interpreter->runOrStopProgram();
}


void MainWindow::on_actionProgram_triggered()
{
    if (!m_pixyDFUConnected || !m_pixyConnected)
    {
        m_console->print("Pixy needs to be put into programming mode.\n"
                         "Do this by unplugging the USB cable, holding down the button,\n"
                         "and then plugging the USB cable back in while continuing to\n"
                         "hold down the button. You may do this now.\n");
    }
    else
    {
        QFileDialog fd(this);
        fd.setWindowTitle("Select a Firmware File");
        fd.setDirectory(QDir("\\"));
        fd.setNameFilter("Firmware (*.hex)");

        if (fd.exec())
        {
            QStringList slist = fd.selectedFiles();
            if (slist.size()==1)
            {
                if (m_flash)
                {
                    try
                    {
                        m_console->print("Programming...\n");
                        QApplication::processEvents(); // render message before we continue
                        m_flash->program(slist.at(0));
                        m_console->print("done! (Reset Pixy by unplugging/plugging USB cable.)\n");

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
            }
        }
    }
}

void MainWindow::on_actionConfigure_triggered()
{
    m_configDialog = new ConfigDialog(m_interpreter);
    connect(m_configDialog, SIGNAL(done()), this, SLOT(configFinished()));
    m_configDialog->show();
    updateButtons();
}

void MainWindow::on_actionRaw_video_triggered()
{
    if (m_interpreter)
        m_interpreter->execute("runprog 0");
}

void MainWindow::on_actionCooked_video_triggered()
{
    if (m_interpreter)
        m_interpreter->execute("runprogArg 0 1");
}


void MainWindow::configFinished()
{
    qDebug("config finished");
    m_configDialog->deleteLater();
    m_configDialog = NULL;
    updateButtons();
    // if we're exitting, close interpreter (handin it down the chain)
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
    m_video->clear();
    m_console->acceptInput(false);
    // if we're disconnected, start the connect thread
    m_connect = new ConnectEvent(this);
    m_pixyConnected = false;

    if (m_exitting) // if we're exitting, shut down
        close();
}

void MainWindow::on_actionExit_triggered()
{
    close();
}
