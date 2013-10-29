#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include "connectevent.h"

namespace Ui {
    class MainWindow;
}

class ChirpMon;
class VideoWidget;
class ConsoleWidget;
class Interpreter;

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();

    friend class VideoWidget;
    friend class ConsoleWidget;

private slots:
    void handleRunState(bool state);
    void handleConnected(ConnectEvent::Device device, bool state);
    void on_actionPlay_Pause_triggered();
    void on_actionExit_triggered();
    void on_actionProgram_triggered();

protected:
     void closeEvent(QCloseEvent *event);

private:
    void connectPixy(bool state);
    void connectPixyDFU(bool state);
    void updateButtons();
    void close();

    bool m_pixyConnected;
    bool m_pixyDFUConnected;
    VideoWidget *m_video;
    ConsoleWidget *m_console;
    Interpreter *m_interpreter;
    ConnectEvent *m_connect;
    Ui::MainWindow *m_ui;
};

#endif // MAINWINDOW_H
