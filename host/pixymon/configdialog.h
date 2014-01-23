#ifndef CONFIGDIALOG_H
#define CONFIGDIALOG_H

#include <QDialog>
#include <QLineEdit>
#include <QLabel>
#include <QThread>
#include "chirpmon.h"
#include "ui_configdialog.h"
#include <vector>

class Interpreter;
class ConfigDialog;

struct Param
{
    Param(QString id,  QString desc, uint8_t type, uint32_t len, uint8_t *data)
    {
        m_id = id;
        m_desc = desc;
        m_type = type;
        m_len = len;
        memcpy(m_data, data, len);
        m_line = NULL;
        m_label = NULL;
    }

    QString m_id;
    QString m_desc;
    uint8_t m_type;
    uint32_t m_len;
    uint8_t m_data[0x100];
    QLineEdit *m_line;
    QLabel *m_label;
};


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
    ConfigDialog(Interpreter *interpreter);
    ~ConfigDialog();

    friend class ConfigWorker;

signals:
    void load();
    void save();
    void done();

public slots:

protected slots:
    void loaded();
    void saved();
    void error(QString message);
    virtual void accept();
    virtual void reject();

private:
    Ui::ConfigDialog *m_ui;
    Interpreter *m_interpreter;
    std::vector<Param> m_paramList;

    QThread m_thread;
};

#endif // CONFIGDIALOG_H
