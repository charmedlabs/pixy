#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>

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
    void on_actionPlay_Pause_triggered();
    void on_actionExit_triggered();

protected:
     void closeEvent(QCloseEvent *event);

private:
    void updateButtons();
    void close();

    VideoWidget *m_video;
    ConsoleWidget *m_console;
    Interpreter *m_interpreter;
    Ui::MainWindow *m_ui;
};

#endif // MAINWINDOW_H
