#include "mainwindow.h"
#include "aboutdialog.h"

AboutDialog::AboutDialog(const QString &contents) :
    m_ui(new Ui::AboutDialog)
{
    m_ui->setupUi(this);
    m_ui->iconLabel->setPixmap(QPixmap(":/icons/icons/pixy.png"));
    m_ui->textEdit->setPlainText(contents);
    setWindowTitle("About " PIXYMON_TITLE);
}

AboutDialog::~AboutDialog()
{
    delete m_ui;
}

