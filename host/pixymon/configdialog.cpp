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


ConfigWorker::ConfigWorker(ConfigDialog *dialog)
{
    m_dialog = dialog;
}

ConfigWorker::~ConfigWorker()
{
}

QString typeString(uint8_t type, uint32_t flags)
{
    switch(type)
    {
    case CRP_INT8:
        return flags&PRM_FLAG_SIGNED ? "INT8" : "UINT8";
    case CRP_INT16:
        return flags&PRM_FLAG_SIGNED ? "INT16" : "UINT16";
    case CRP_INT32:
        return flags&PRM_FLAG_SIGNED ? "INT32" : "UINT32";
    case CRP_FLT32:
        return "FLOAT32";
    default:
        return "?";
    }
}

void ConfigWorker::load()
{
    qDebug("loading...");
    QMutexLocker locker(&m_dialog->m_interpreter->m_chirp->m_mutex);
    uint i;
    char *id, *desc;
    uint32_t len;
    uint32_t flags;
    int response;
    uint8_t *data, *argList;

    ChirpProc prm_getAll = m_dialog->m_interpreter->m_chirp->getProc("prm_getAll");
    if (prm_getAll<0)
        return;

    int res;
    for (i=0; true; i++)
    {
        QString category;

        res = m_dialog->m_interpreter->m_chirp->callSync(prm_getAll, UINT16(i), END_OUT_ARGS, &response, &flags, &argList, &id, &desc, &len, &data, END_IN_ARGS);
        if (res<0)
            break;

        if (response<0)
            break;

        QString sdesc(desc);

        // deal with param category
        QStringList words = QString(desc).split(QRegExp("\\s+"));
        int i = words.indexOf("@c");
        if (i>=0 && words.size()>i+1)
        {
            category = words[i+1];
            sdesc = sdesc.remove("@c "); // remove form description
            sdesc = sdesc.remove(category + " "); // remove from description
            category = category.replace('_', ' '); // make it look prettier
        }
        else
            category = CD_GENERAL;

        Parameter parameter(id, "("+typeString(argList[0], flags)+") "+sdesc);
        parameter.setProperty(PP_CATEGORY, category);
        parameter.setProperty(PP_FLAGS, flags);
        if (strlen((char *)argList)>1)
        {
            QByteArray a((char *)data, len);
            parameter.set(a);
        }
        else
        {
            // save off type
            parameter.setProperty(PP_TYPE, QVariant(QMetaType::UChar, argList));

            if (argList[0]==CRP_INT8 || argList[0]==CRP_INT16 || argList[0]==CRP_INT32)
            {
                int32_t val = 0;
                Chirp::deserialize(data, len, &val, END);
                parameter.set(QVariant(val));
            }
            else if (argList[0]==CRP_FLT32)
            {
                float val;
                Chirp::deserialize(data, len, &val, END);
                parameter.set(val);
            }
            else // not sure what to do with it, so we'll save it as binary
            {
                QByteArray a((char *)data, len);
                parameter.set(a);
            }
        }
        m_dialog->m_parameters.add(parameter);
    }

    qDebug("loaded");
    emit loaded();
}


void ConfigWorker::save()
{
    QMutexLocker locker(&m_dialog->m_interpreter->m_chirp->m_mutex);
    int i;
    int res, response;

    ChirpProc prm_set = m_dialog->m_interpreter->m_chirp->getProc("prm_set");
    if (prm_set<0)
        return;

    Parameters &parameters = m_dialog->m_parameters.parameters();

    for (i=0; i<parameters.size(); i++)
    {
        uint8_t buf[0x100];

        if (parameters[i].dirty())
        {
            int len;
            QByteArray str = parameters[i].id().toUtf8();
            const char *id = str.constData();
            QChar type = parameters[i].property(PP_TYPE).toChar();

            if (type==CRP_INT8 || type==CRP_INT16 || type==CRP_INT32)
            {
                int val = parameters[i].value().toInt();
                len = Chirp::serialize(NULL, buf, 0x100, type, val, END);
            }
            else if (type==CRP_FLT32)
            {
                float val = parameters[i].value().toFloat();
                len = Chirp::serialize(NULL, buf, 0x100, type, val, END);
            }
            else
                continue; // don't know what to do!

            res = m_dialog->m_interpreter->m_chirp->callSync(prm_set, STRING(id), UINTS8(len, buf), END_OUT_ARGS, &response, END_IN_ARGS);
            if (res<0 || response<0)
            {
                emit error("There was a problem setting a parameter.");
                return;
            }
        }
    }

    emit saved();
}


ConfigDialog::ConfigDialog(QWidget *parent, Interpreter *interpreter) : QDialog(parent), m_ui(new Ui::ConfigDialog)
{

    m_ui->setupUi(this);
    m_interpreter = interpreter;
    m_interpreter->unwait(); // unhang interpreter if it's waiting

    m_tabs = new QTabWidget(this);
    m_ui->gridLayout->addWidget(m_tabs);

    ConfigWorker *worker = new ConfigWorker(this);

    worker->moveToThread(&m_thread);
    connect(this, SIGNAL(load()), worker, SLOT(load()));
    connect(this, SIGNAL(save()), worker, SLOT(save()));
    connect(worker, SIGNAL(loaded()), this, SLOT(loaded()));
    connect(worker, SIGNAL(saved()), this, SLOT(saved()));
    connect(worker, SIGNAL(error(QString)), this, SLOT(error(QString)));
    connect(m_ui->buttonBox, SIGNAL(clicked(QAbstractButton*)), this, SLOT(apply(QAbstractButton*)));
    connect(this, SIGNAL(change()), m_interpreter, SLOT(handleParamChange()));

    m_thread.start();
    m_rejecting = false;
    m_loading = true;
    m_applying = false;
    emit load();

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
    qDebug("destroying config dialog...");
    m_thread.quit();
    m_thread.wait();
    // we don't delete any of the widgets because the parent deletes its children

    qDebug("done");
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
    Parameters &parameters = m_parameters.parameters();

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
                if (val!=parameter.value().toInt())
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
    Parameters &parameters = m_parameters.parameters();

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
                int val = parameter.value().toInt();
                if (type==CRP_INT16) // sign extend
                {
                    val <<= 16;
                    val >>= 16;
                }
                else if (type==CRP_INT8) // sign extend
                {
                    val <<= 24;
                    val >>= 24;
                }

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
    m_loading = false;

    ParamFile pfile;

    pfile.open("pixyparameters.xml", false);
    pfile.write("Pixyparams", &m_parameters);
    pfile.close();

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

    if (m_rejecting) // resume reject
    {
        emit done();
        QDialog::reject();
    }
    qDebug("rendering config done");
}

void ConfigDialog::saved()
{
    ParamFile pf;
    pf.open("pixyparameters.xml", true);
    m_parameters.set("signature1", QVariant(0));
    pf.read("Pixyparams", &m_parameters);
    pf.close();

    if (!m_applying)
    {
        emit done();
        QDialog::accept();
    }
    m_applying = false;

    emit change(); // signal other folks
}

void ConfigDialog::error(QString message)
{
    QMessageBox::critical(NULL, "Error", message);
}


void ConfigDialog::accept()
{
    if (updateDB()<0)
        return;
    emit save();
    QDialog::accept();
}

void ConfigDialog::reject()
{
    qDebug("reject called");
    if (!m_loading) // if we're in the middle of loading, defer rejection
    {
        QDialog::reject();
        emit done();
    }
    else
        m_rejecting = true; // defer reject
}

void ConfigDialog::apply(QAbstractButton *button)
{
    if (button->text()=="Apply")
    {
        if (updateDB()<0)
            return;
        m_applying = true;
        emit save();
    }
}





