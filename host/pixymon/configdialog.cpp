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
#include <QAbstractButton>
#include <stdexcept>



ConfigDialog::ConfigDialog(QWidget *parent, Interpreter *interpreter) : QDialog(parent), m_ui(new Ui::ConfigDialog)
{

    m_ui->setupUi(this);
    m_interpreter = interpreter;
    m_interpreter->unwait(); // unhang interpreter if it's waiting

    m_tabs = new QTabWidget(this);
    m_ui->gridLayout->addWidget(m_tabs);

    connect(interpreter, SIGNAL(paramLoaded()), this, SLOT(loaded()));
    connect(m_ui->buttonBox, SIGNAL(clicked(QAbstractButton*)), this, SLOT(apply(QAbstractButton*)));

    m_interpreter->loadParams();

#ifdef __MACOS__
    setMinimumWidth(550);
#endif
#ifdef __LINUX__
    setMinimumWidth(500);
#endif
#ifdef __WINDOWS__
    setMinimumWidth(420);
#endif
}


ConfigDialog::~ConfigDialog()
{
}

QWidget *ConfigDialog::findCategory(const QString &category)
{
    int i;
    QString tabText;

    for (i=0; true; i++)
    {
        tabText = m_tabs->tabText(i);
        if (tabText=="")
            break;
        else if (tabText==category)
            return m_tabs->widget(i);
    }
    return NULL;
}

int ConfigDialog::updateDB()
{
    Parameters &parameters = m_interpreter->m_pixyParameters.parameters();

    for (int i=0; i<parameters.size(); i++)
    {
        Parameter &parameter = parameters[i];
        int type = parameter.value().type();
        uint flags = parameter.property(PP_FLAGS).toInt();

        if (flags&PRM_FLAG_INTERNAL) // don't render!
            continue;

        QLineEdit *line = (QLineEdit *)parameter.property(PP_WIDGET).toLongLong();

        if (type==QMetaType::Float)
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
                parameter.set(QVariant(QMetaType::Float, &val));
                parameter.setDirty(true);
            }
        }
        else if (type==QMetaType::QString)
        {
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
                    parameter.set(QVariant(parameter.value().type(), &val));
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
                    parameter.set(QVariant(parameter.value().type(), &val));
                    parameter.setDirty(true);
                }
           }
        }
    }
    return 0;
}

void ConfigDialog::loaded()
{
    int i;
    QWidget *tab;
    QGridLayout *layout;

    qDebug("rendering config...");
    Parameters &parameters = m_interpreter->m_pixyParameters.parameters();

    for (i=0; i<parameters.size(); i++)
    {
        Parameter &parameter = parameters[i];
        uint flags = parameter.property(PP_FLAGS).toInt();

        if (flags&PRM_FLAG_INTERNAL) // don't render!
            continue;

        QLineEdit *line = new QLineEdit();
        QLabel *label = new QLabel(parameter.id());
        label->setToolTip(parameter.help());
        label->setAlignment(Qt::AlignRight);
        qlonglong lline = (qlonglong)line;
        parameter.setProperty(PP_WIDGET, lline);

        if ((QMetaType::Type)parameter.value().type()!=QMetaType::QByteArray) // make sure it's a scalar type
        {
            QChar type = parameter.property(PP_TYPE).toChar();
            if (type==CRP_FLT32)
            {
                float val = parameter.value().toFloat();
                line->setText(QString::number(val, 'f', 3));
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
            QString category = parameter.property(PP_CATEGORY).toString();
            tab = findCategory(category);
            if (tab==NULL)
            {
                tab = new QWidget();
                layout = new QGridLayout(tab);
                m_tabs->addTab(tab, category);
            }
            else
                layout = (QGridLayout *)tab->layout();
            line->setMaximumWidth(75);
            layout->addWidget(label, i, 0);
            layout->addWidget(line, i, 1);
        }
    }

#if 0
    ParamFile pfile;

    pfile.open("pixyparameters.xml", false);
    pfile.write("Pixyparams", &m_parameters);
    pfile.close();
#endif

    // set stretch on all tabs
    for (i=0; true; i++)
    {
        QWidget *tab = m_tabs->widget(i);
        if (tab)
        {
            ((QGridLayout *)tab->layout())->setRowStretch(100, 1);
            ((QGridLayout *)tab->layout())->setColumnStretch(100, 1);
        }
        else
            break;
    }

    show();

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





