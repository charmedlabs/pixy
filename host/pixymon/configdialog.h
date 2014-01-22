#ifndef CONFIGDIALOG_H
#define CONFIGDIALOG_H

#include <QDialog>
#include "ui_configdialog.h"

class Interpreter;

class ConfigDialog : public QDialog
{
    Q_OBJECT

public:
    ConfigDialog(Interpreter *interpreter);
    ~ConfigDialog();

signals:

public slots:

protected slots:
    virtual void accept();
    virtual void reject();

private:
    Ui::ConfigDialog *m_ui;
    Interpreter *m_interpreter;
};

#endif // CONFIGDIALOG_H
