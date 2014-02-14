#ifndef ABOUTDIALOG_H
#define ABOUTDIALOG_H

#include <QDialog>
#include "ui_about.h"


class AboutDialog : public QDialog
{
    Q_OBJECT

public:
    AboutDialog(const QString &contents);
    ~AboutDialog();


signals:

public slots:

protected slots:

private:
    Ui::AboutDialog *m_ui;

};

#endif // ABOUTDIALOG_H
