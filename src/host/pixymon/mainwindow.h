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

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <vector>
#include "monparameterdb.h"

#define PIXY_PARAMFILE_TAG      "Pixy_parameters"

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
class QSettings;
class QMessageBox;

enum Device {NONE, PIXY, PIXY_DFU};

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(int argc, char *argv[], QWidget *parent = 0);
    ~MainWindow();

    friend class VideoWidget;
    friend class ConsoleWidget;

private slots:
    void handleRunState(int state);
    void handleConnected(Device device, bool state);
    void handleActions();
    void handleActionScriptlet(QString action, QStringList scriptlet);
    void handleLoadParams();
    void handleConfigDialogFinished();
    void interpreterFinished();
    void handleVersion(ushort major, ushort minor, ushort build, QString type);
    void on_actionAbout_triggered();
    void on_actionPlay_Pause_triggered();
    void on_actionDefault_program_triggered();
    void on_actionConfigure_triggered();
    void on_actionHelp_triggered();
    void on_actionSave_Image_triggered();
    void on_actionSave_Pixy_parameters_triggered();
    void on_actionLoad_Pixy_parameters_triggered();
    void on_actionExit_triggered();
    void on_actionRaw_video_triggered();
    void on_actionCooked_video_triggered();

protected:
     void closeEvent(QCloseEvent *event);

private:
    void connectPixy(bool state);
    void connectPixyDFU(bool state);
    void updateButtons();
    void addAction(const QString &label, const QStringList &command);
    void clearActions();
    void setEnabledActions(bool enable);
    void close();
    void parseCommandline(int argc, char *argv[]);
    void program(const QString &file);
    void handleFirmware(ushort major, ushort minor, ushort build, const QString &type);

    bool m_pixyConnected;
    bool m_pixyDFUConnected;
    enum {WAIT_NONE, WAIT_EXITTING, WAIT_SAVING_PARAMS, WAIT_LOADING_PARAMS} m_waiting;
    VideoWidget *m_video;
    ConsoleWidget *m_console;
    Interpreter *m_interpreter;
    ConnectEvent *m_connect;
    Flash *m_flash;
    ConfigDialog *m_configDialog;
    std::vector<QAction *> m_actions;
    Ui::MainWindow *m_ui;

    QString m_firmwareFile;
    QString m_argvFirmwareFile;
    QString m_initScript;
    bool m_versionIncompatibility;
    QSettings *m_settings;
    MonParameterDB m_parameters;
    QMessageBox *m_fwInstructions;
    QMessageBox *m_fwMessage;
};

#endif // MAINWINDOW_H
