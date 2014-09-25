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

#include "interpreter.h"
#include "configdialog.h"
#include "pixytypes.h"
#include "paramfile.h"
#include <QLabel>
#include <QMessageBox>
#include <QDebug>
#include <QTableWidget>
#include <QPushButton>
#include <QAbstractButton>
#include <QCheckBox>
#include <QFileDialog>
#include <stdexcept>



ConfigDialog::ConfigDialog(QWidget *parent, Interpreter *interpreter) : QDialog(parent), m_ui(new Ui::ConfigDialog)
{

    m_ui->setupUi(this);
    m_interpreter = interpreter;
    m_interpreter->unwait(); // unhang interpreter if it's waiting

    m_pixyTabs = new QTabWidget(this);
    m_ui->pixyLayout->addWidget(m_pixyTabs);
    m_pixymonTabs = new QTabWidget(this);
    m_ui->pixymonLayout->addWidget(m_pixymonTabs);
    connect(interpreter, SIGNAL(paramLoaded()), this, SLOT(loaded()));
    connect(m_ui->buttonBox, SIGNAL(clicked(QAbstractButton*)), this, SLOT(apply(QAbstractButton*)));

    m_interpreter->loadParams();

    render(m_interpreter->m_pixymonParameters, m_ui->pixymonLayout, m_pixymonTabs);

#ifdef __MACOS__
    setMinimumWidth(650);
#endif
#ifdef __LINUX__
    setMinimumWidth(600);
#endif
#ifdef __WINDOWS__
    setMinimumWidth(550);
#endif
}


ConfigDialog::~ConfigDialog()
{
}

QWidget *ConfigDialog::findCategory(const QString &category, QTabWidget *tabs)
{
    int i;
    QString tabText;

    for (i=0; true; i++)
    {
        tabText = tabs->tabText(i);
        if (tabText=="")
            break;
        else if (tabText==category)
            return tabs->widget(i);
    }
    return NULL;
}

int ConfigDialog::updateDB()
{
    int res1, res2;

    res1 = updateDB(&m_interpreter->m_pixyParameters);
    res2 = updateDB(m_interpreter->m_pixymonParameters);

    if (res1<0)
        return res1;
    if (res2<0)
        return res2;
    return 0;
}

int ConfigDialog::updateDB(ParameterDB *data)
{
    Parameters &parameters = data->parameters();

    for (int i=0; i<parameters.size(); i++)
    {
        Parameter &parameter = parameters[i];
        int type = parameter.type();
        uint flags = parameter.property(PP_FLAGS).toInt();

        parameter.clearShadow();

        if (flags&PRM_FLAG_INTERNAL) // don't render!
            continue;

        QLineEdit *line = (QLineEdit *)parameter.property(PP_WIDGET).toLongLong();

        if (type==PT_FLT32)
        {
            bool ok;
            float val;
            val = line->text().toFloat(&ok);
            if (!ok)
            {
                QMessageBox::critical(NULL, "Error", parameter.id() + " needs to be a floating point number!");
                return -1;
            }
            if (val!=parameter.value().toFloat())
            {
                parameter.set(val);
                parameter.setDirty(true);
            }
        }
        else if (type==PT_STRING)
            parameter.set(line->text());
        else if (type==PT_PATH)
        {
            QDir dir(line->text());

            if (!dir.exists())
            {
                QMessageBox::critical(NULL, "Error", parameter.id() + " \"" + line->text() + "\" is not a valid folder!");
                return -1;
            }
            if (line->text()!=parameter.value().toString())
            {
                parameter.set(line->text());
                parameter.setDirty(true);
            }
        }
        else if (type==PT_BOOL)
        {
            QCheckBox *cbox = (QCheckBox *)line;

            if(cbox->isChecked()!=parameter.value().toBool())
            {
                parameter.set(cbox->isChecked());
                parameter.setDirty(true);
            }
        }
        else // must be int type
        {
            int base;
            bool ok;
            if (line->text().left(2)=="0x")
                base = 16;
            else
                base = 10;
            if (flags&PRM_FLAG_SIGNED)
            {
                int val = line->text().toInt(&ok, base);
                if (!ok)
                {
                    QMessageBox::critical(NULL, "Error", parameter.id() + " needs to be an integer!");
                    return -1;
                }
                if (val!=parameter.valueInt())
                {
                    parameter.set(val);
                    parameter.setDirty(true);
                }
            }
            else
            {
                uint val = line->text().toUInt(&ok, base);
                if (!ok)
                {
                    QMessageBox::critical(NULL, "Error", parameter.id() + " needs to be an unsigned integer!");
                    return -1;
                }
                if (val!=parameter.value().toUInt())
                {
                    parameter.set(val);
                    parameter.setDirty(true);
                }
           }
        }
    }
    return 0;
}

void ConfigDialog::loaded()
{
    render(&m_interpreter->m_pixyParameters, NULL, m_pixyTabs);
    show();
}

void ConfigDialog::render(ParameterDB *data, QGridLayout *layout, QTabWidget *tabs)
{
    int i;
    QWidget *tab;

    qDebug("rendering config...");
    Parameters &parameters = data->parameters();

    for (i=0; i<parameters.size(); i++)
    {
        Parameter &parameter = parameters[i];
        uint flags = parameter.property(PP_FLAGS).toInt();

        if (flags&PRM_FLAG_INTERNAL) // don't render!
            continue;

        PType type = parameter.type();
        QPushButton *button = NULL;
        QCheckBox *cbox = NULL;
        QLineEdit *line = new QLineEdit();
        QLabel *label = new QLabel(parameter.id());
        label->setToolTip(parameter.help());
        label->setAlignment(Qt::AlignRight);
        parameter.setProperty(PP_WIDGET, (qlonglong)line);
        line->setMinimumWidth(50);
        line->setMaximumWidth(75);
        line->setToolTip(parameter.help());

        if (type!=PT_INTS8) // make sure it's a scalar type
        {
            if (type==PT_PATH)
            {
                button = new QPushButton("Change...");
                button->setProperty("Parameter", (qlonglong)&parameter);
                connect(button, SIGNAL(clicked()), this, SLOT(handleChangeClicked()));
                button->setToolTip("Select a new path");
                line->setMinimumWidth(200);
                line->setMaximumWidth(300);
                line->setText(parameter.value().toString());
            }
            else if (type==PT_FLT32)
            {
                float val = parameter.value().toFloat();
                line->setText(QString::number(val, 'f', 6));
            }
            else if (type==PT_BOOL)
            {
                cbox = new QCheckBox();
                cbox->setProperty("Parameter", (qlonglong)&parameter);
                cbox->setChecked(parameter.value().toBool());
                connect(cbox, SIGNAL(clicked()), this, SLOT(handleCheckBox()));
                parameter.setProperty(PP_WIDGET, (qlonglong)cbox);
                delete line;
                line = NULL;
            }
            else if (!(flags&PRM_FLAG_SIGNED))
            {
                uint val = parameter.value().toUInt();
                if (flags&PRM_FLAG_HEX_FORMAT)
                    line->setText("0x" + QString::number(val, 16));
                else
                    line->setText(QString::number(val));
            }
            else
            {
                int val = parameter.valueInt();
                line->setText(QString::number(val));
            }

            // deal with categories-- create category tab if needed
            if (tabs)
            {
                QString category = parameter.property(PP_CATEGORY).toString();
                if (category=="")
                    category = CD_GENERAL;
                tab = findCategory(category, tabs);
                if (tab==NULL)
                {
                    tab = new QWidget();
                    layout = new QGridLayout(tab);
                    tabs->addTab(tab, category);
                }
                else
                    layout = (QGridLayout *)tab->layout();
            }
            layout->addWidget(label, i, 0);
            if (cbox)
                layout->addWidget(cbox, i, 1);
            if (line)
                layout->addWidget(line, i, 1);
            if (button)
                layout->addWidget(button, i, 2);
        }
    }

    if (tabs)
    {
        // set stretch on all tabs
        for (i=0; true; i++)
        {
            QWidget *tab = tabs->widget(i);
            if (tab)
            {
                ((QGridLayout *)tab->layout())->setRowStretch(100, 1);
                ((QGridLayout *)tab->layout())->setColumnStretch(100, 1);
            }
            else
                break;
        }
    }
    if (layout)
    {
        layout->setRowStretch(100, 1);
        layout->setColumnStretch(100, 1);
    }

    qDebug("rendering config done");
}

void ConfigDialog::accept()
{
    if (updateDB()<0)
        return;
    m_interpreter->saveParams();
    QDialog::accept();
}

void ConfigDialog::reject()
{
    qDebug("reject called");
    m_interpreter->m_pixymonParameters->clearShadow();
    m_interpreter->saveParams();
    QDialog::reject();
}

void ConfigDialog::apply(QAbstractButton *button)
{
    if (button->text()=="Apply")
    {
        if (updateDB()<0)
            return;
        m_interpreter->saveParams();
    }
}

void ConfigDialog::handleChangeClicked()
{
    // get button that was clicked
    QPushButton *button = (QPushButton *)sender();
    // then grab parameter associated with the button
    Parameter *parameter = (Parameter *)button->property("Parameter").toLongLong();

    // bring up file dialog
    QFileDialog fd(this);
    fd.setWindowTitle("Please choose a folder");
    fd.setDirectory(parameter->value().toString());
    fd.setFileMode(QFileDialog::Directory);
    fd.setOption(QFileDialog::ShowDirsOnly);

    if (fd.exec())
    {
        QStringList dir = fd.selectedFiles();
        // update text box
        QLineEdit *line = (QLineEdit *)parameter->property(PP_WIDGET).toLongLong();
        line->setText(dir[0]);
    }
}


void ConfigDialog::handleCheckBox()
{
    QCheckBox *cbox = (QCheckBox *)sender();
    Parameter *parameter = (Parameter *)cbox->property("Parameter").toLongLong();

    MonModule *mm = (MonModule *)parameter->property(PP_MM_CALLBACK).toLongLong();
    if (mm)
    {
        parameter->set(cbox->isChecked(), true);
        parameter->setDirty(true);
        m_interpreter->updateParam(mm);
    }

}

