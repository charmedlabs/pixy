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
