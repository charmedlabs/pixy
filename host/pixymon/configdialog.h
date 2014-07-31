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

#ifndef CONFIGDIALOG_H
#define CONFIGDIALOG_H

#include <QDialog>
#include <QLineEdit>
#include <QLabel>
#include <QThread>
#include <QTableWidget>
#include "chirpmon.h"
#include "parameters.h"
#include "ui_configdialog.h"
#include <vector>

#define CD_GENERAL     "General"

class Interpreter;
class ConfigDialog;

// struct to store parameter values
struct Param
{
    Param(QString id, QString category, QString desc, uint8_t type, uint32_t flags, uint32_t len, uint8_t *data)
    {
        m_id = id;
        m_category = category;
        m_desc = desc;
        m_type = type;
        m_flags = flags;
        m_len = len;
        memcpy(m_data, data, len);
        m_line = NULL;
        m_label = NULL;
    }

    QString m_id;
    QString m_category;
    QString m_desc;
    uint8_t m_type;
    uint32_t m_flags;
    uint32_t m_len;
    uint8_t m_data[0x100];
    QLineEdit *m_line;
    QLabel *m_label;
};

// need worker thread because worker thread in Interpreter is calling chirp-- and we don't want gui thread to block,
// which is what would happen if another thread isn't created.
// If gui thread blocks and worker thread in Interpreter is blocking (because there's a mutex in the paint call)---
// then we have deadlock.
// The nice thing is that we can load and save all the parameters while streaming and rendering image data without any issues.
class ConfigWorker : public QObject
{
    Q_OBJECT

public:
    ConfigWorker(ConfigDialog *dialog);
    ~ConfigWorker();

public slots:
    void load();
    void save();

signals:
    void loaded();
    void saved();
    void error(QString);

private:
    ConfigDialog *m_dialog;
};

class ConfigDialog : public QDialog
{
    Q_OBJECT

public:
    ConfigDialog(QWidget *parent, Interpreter *interpreter);
    ~ConfigDialog();

    friend class ConfigWorker;

signals:
    void load();
    void save();
    void done();
    void change();

public slots:

protected slots:
    void loaded();
    void saved();
    void error(QString message);
    void apply(QAbstractButton *button);
    virtual void accept();
    virtual void reject();

protected:

private:
    QWidget *findCategory(const QString &category);

    Ui::ConfigDialog *m_ui;
    QTabWidget *m_tabs;
    Interpreter *m_interpreter;
    std::vector<Param> m_paramList;
    bool m_loading;
    bool m_rejecting;
    bool m_applying;

    ParameterDB m_parameters;

    QThread m_thread;
};

#endif // CONFIGDIALOG_H
