#include "Interpreter.h"
#include "configdialog.h"
#include <QLabel>
#include <QLineEdit>

ConfigDialog::ConfigDialog(Interpreter *interpreter) : m_ui(new Ui::ConfigDialog)
{
    m_ui->setupUi(this);
    m_interpreter = interpreter;

    QLabel *label = new QLabel("Hello");
    label->setAlignment(Qt::AlignRight);
    label->setToolTip("how are you?");
    m_ui->gridLayout->addWidget(label, 0, 0);
    label = new QLabel("There");
    label->setAlignment(Qt::AlignRight);
    label->setToolTip("this is a tooltip the quick brown fox Note\nthat by default tooltips are only shown for widgets that are children\nof the active window. You can change this behavior by setting\nthe attribute Qt::WA_AlwaysShowToolTips on the window, not on the widget with the tooltip.");
    m_ui->gridLayout->addWidget(label, 1, 0);
    QLineEdit *line = new QLineEdit();
    m_ui->gridLayout->addWidget(line, 0, 1);
    line = new QLineEdit();
    m_ui->gridLayout->addWidget(line, 1, 1);


}


ConfigDialog::~ConfigDialog()
{
    delete m_ui;
}


void ConfigDialog::accept()
{
    QDialog::accept();
}

void ConfigDialog::reject()
{
    QDialog::reject();
}





