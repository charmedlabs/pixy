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

#include "mainwindow.h"
#include "aboutdialog.h"
#include "pixymon.h"

AboutDialog::AboutDialog(const QString &contents) :
    m_ui(new Ui::AboutDialog)
{
    m_ui->setupUi(this);
    m_ui->iconLabel->setPixmap(QPixmap(":/icons/icons/pixy.png"));
    m_ui->textEdit->setOpenExternalLinks(true);
    m_ui->textEdit->setHtml(contents);
    setWindowTitle("About " PIXYMON_TITLE);
}

AboutDialog::~AboutDialog()
{
}

