#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>

#define PIXYMON_TITLE   "PixyMon"

namespace Ui {
    class MainWindow;
}

class ChirpMon;
class VideoWidget;
class ConsoleWidget;
class Interpreter;
class Flash;
class ConnectEvent;

enum Device {NONE, PIXY, PIXY_DFU};

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
    void handleConnected(Device device, bool state);
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
    Flash *m_flash;
    Ui::MainWindow *m_ui;
};

#endif // MAINWINDOW_H
