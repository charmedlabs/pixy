#include <stdexcept>
#include <QDebug>
#include <QMessageBox>
#include "mainwindow.h"
#include "videowidget.h"
#include "console.h"
#include "interpreter.h"
#include "chirpmon.h"
#include "dfu.h"
#include "ui_mainwindow.h"

extern ChirpProc c_grabFrame;

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    m_ui(new Ui::MainWindow)
{
    m_ui->setupUi(this);
    setWindowTitle("PixyMon");

    try
    {
        Dfu *dfu;
        dfu = new Dfu();
        dfu->download("video.bin.hdr");
    }
    catch (std::runtime_error &exception)
    {
        QMessageBox::critical(this, "Error", exception.what());
    }

    m_console = new ConsoleWidget(this);
    m_video = new VideoWidget(this);

    try
    {
        m_interpreter = new Interpreter(m_console, m_video);
    }
    catch (std::runtime_error &exception)
    {
        QMessageBox::critical(this, "Error", exception.what());
        exit(1);
    }

    // start with a program (normally would be read from a config file instead of hard-coded)
    m_interpreter->beginProgram();
#if 1
    m_interpreter->call("cam_getFrame 33, 0, 0, 320, 200");
#else
    m_interpreter->call("img_getComponents 16744579, 65, 0");
    //m_interpreter->m_chirp->callAsync(ChirpMon::getComponents,
    //                   UINT32(0x00ff8083), UINT32(65), UINT32(0), END_OUT_ARGS);
#endif
    m_interpreter->endProgram();
    m_interpreter->runProgram();

    m_ui->imageLayout->addWidget(m_video);
    m_ui->imageLayout->addWidget(m_console);

    m_ui->toolBar->addAction(m_ui->actionPlay_Pause);

    m_ui->actionConfigure->setIcon(QIcon(":/icons/icons/config.png"));
    m_ui->toolBar->addAction(m_ui->actionConfigure);

    connect(m_interpreter, SIGNAL(runState(bool)), this, SLOT(handleRunState(bool)));

    updateButtons();
}

MainWindow::~MainWindow()
{
    delete m_ui;
}

void MainWindow::updateButtons()
{
    if (m_interpreter->programRunning())
    {
        m_ui->actionPlay_Pause->setIcon(QIcon(":/icons/icons/pause.png"));

    }
    else
    {
        m_ui->actionPlay_Pause->setIcon(QIcon(":/icons/icons/play.png"));

    }
}

void MainWindow::close()
{
    delete m_interpreter;
    QCoreApplication::exit();
}

void MainWindow::closeEvent(QCloseEvent *event)
{
    close();
}

void MainWindow::handleRunState(bool state)
{
    updateButtons();
}

void MainWindow::on_actionPlay_Pause_triggered()
{
    if (m_interpreter->programRunning())
        m_interpreter->stopProgram();
    else
        m_interpreter->resumeProgram();
    updateButtons();
}

void MainWindow::on_actionExit_triggered()
{
    close();
}
