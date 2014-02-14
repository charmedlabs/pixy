#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <vector>

#define PIXYMON_TITLE   "PixyMon"

#define VER_MAJOR       0
#define VER_MINOR       1
#define VER_BUILD       41

namespace Ui {
    class MainWindow;
}

class ChirpMon;
class VideoWidget;
class ConsoleWidget;
class Interpreter;
class Flash;
class ConnectEvent;
class ConfigDialog;

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
    void handleRunState(uint state);
    void handleConnected(Device device, bool state);
    void handleActions();
    void configFinished();
    void interpreterFinished();
    void on_actionAbout_triggered();
    void on_actionPlay_Pause_triggered();
    void on_actionConfigure_triggered();
    void on_actionExit_triggered();
    void on_actionProgram_triggered();
    void on_actionRaw_video_triggered();
    void on_actionCooked_video_triggered();

protected:
     void closeEvent(QCloseEvent *event);

private:
    void connectPixy(bool state);
    void connectPixyDFU(bool state);
    void updateButtons();
    void addAction(const QString &label, const QString &command);
    void setEnabledActions(bool enable);
    void close();

    bool m_pixyConnected;
    bool m_pixyDFUConnected;
    bool m_exitting;
    VideoWidget *m_video;
    ConsoleWidget *m_console;
    Interpreter *m_interpreter;
    ConnectEvent *m_connect;
    Flash *m_flash;
    ConfigDialog *m_configDialog;
    std::vector<QAction *> m_actions;
    Ui::MainWindow *m_ui;
};

#endif // MAINWINDOW_H
