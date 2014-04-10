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

QString typeString(uint8_t type)
{
    switch(type)
    {
    case CRP_INT8:
        return "INT8";
    case CRP_INT16:
        return "INT16";
    case CRP_INT32:
        return "INT32";
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

        QString sdesc(desc);

        if (response<0)
            break;

        if (flags&PRM_FLAG_INTERNAL)
            continue;

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

        m_dialog->m_paramList.push_back(Param(id, category, "("+typeString(argList[0])+") "+sdesc, argList[0], flags, len, data));
    }

    qDebug("loaded");
    emit loaded();
}

void ConfigWorker::save()
{
    QMutexLocker locker(&m_dialog->m_interpreter->m_chirp->m_mutex);
    uint i;
    int res, response;

    ChirpProc prm_set = m_dialog->m_interpreter->m_chirp->getProc("prm_set");
    if (prm_set<0)
        return;

    for (i=0; i<m_dialog->m_paramList.size(); i++)
    {
        uint8_t buf[0x100];
        Param &param = m_dialog->m_paramList[i];
        QByteArray str = param.m_id.toUtf8();
        const char *id = str.constData();
        if (param.m_type==CRP_INT8 || param.m_type==CRP_INT16 || param.m_type==CRP_INT32)
        {
            int val, base;
            bool ok;
            if (param.m_line->text().left(2)=="0x")
                base = 16;
            else
                base = 10;
            val = param.m_line->text().toInt(&ok, base);
            if (!ok)
            {
                emit error(param.m_id + " needs to be an integer!");
                return;
            }
            Chirp::serialize(NULL, buf, 0x100, param.m_type, val, END);
        }
        else if (param.m_type==CRP_FLT32)
        {
            bool ok;
            float val;
            val = param.m_line->text().toFloat(&ok);
            if (!ok)
            {
                emit error(param.m_id + " needs to be a floating point number!");
                return;
            }
            Chirp::serialize(NULL, buf, 0x100, param.m_type, val, END);
        }
        if (memcmp(buf, param.m_data, param.m_len)) // only write those that have changed to save the flash sector
        {
            qDebug("saving config params");
            res = m_dialog->m_interpreter->m_chirp->callSync(prm_set, STRING(id), UINTS8(param.m_len, buf), END_OUT_ARGS, &response, END_IN_ARGS);
            if (res<0 || response<0)
            {
                emit error("There was a problem setting a parameter.");
                return;
            }
            // copy into param
            memcpy(param.m_data, buf, param.m_len);
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

void ConfigDialog::loaded()
{
    uint i;
    QWidget *tab;
    QGridLayout *layout;

    qDebug("rendering config...");
    for (i=0; i<m_paramList.size(); i++)
    {
        Param &param = m_paramList[i];

        param.m_line = new QLineEdit();
        param.m_label = new QLabel(param.m_id);
        param.m_label->setToolTip(param.m_desc);
        param.m_label->setAlignment(Qt::AlignRight);

        if (param.m_type==CRP_INT8)
        {
            int8_t val;
            Chirp::deserialize(param.m_data, param.m_len, &val, END);
            if (param.m_flags&PRM_FLAG_SIGNED)
            {
                if (param.m_flags&PRM_FLAG_HEX_FORMAT)
                    param.m_line->setText("0x" + QString::number(val, 16));
                else
                    param.m_line->setText(QString::number(val));
            }
            else
            {
                if (param.m_flags&PRM_FLAG_HEX_FORMAT)
                    param.m_line->setText("0x" + QString::number((uint8_t)val, 16));
                else
                    param.m_line->setText(QString::number((uint8_t)val));
            }
        }
        else if (param.m_type==CRP_INT16)
        {
            int16_t val;
            Chirp::deserialize(param.m_data, param.m_len, &val, END);
            if (param.m_flags&PRM_FLAG_SIGNED)
            {
                if (param.m_flags&PRM_FLAG_HEX_FORMAT)
                    param.m_line->setText("0x" + QString::number(val, 16));
                else
                    param.m_line->setText(QString::number(val));
            }
            else
            {
                if (param.m_flags&PRM_FLAG_HEX_FORMAT)
                    param.m_line->setText("0x" + QString::number((uint16_t)val, 16));
                else
                    param.m_line->setText(QString::number((uint16_t)val));
            }
        }
        else if (param.m_type==CRP_INT32)
        {
            int32_t val;
            Chirp::deserialize(param.m_data, param.m_len, &val, END);
            if (param.m_flags&PRM_FLAG_SIGNED)
            {
                if (param.m_flags&PRM_FLAG_HEX_FORMAT)
                    param.m_line->setText("0x" + QString::number(val, 16));
                else
                    param.m_line->setText(QString::number(val));
            }
            else
            {
                if (param.m_flags&PRM_FLAG_HEX_FORMAT)
                    param.m_line->setText("0x" + QString::number((uint32_t)val, 16));
                else
                    param.m_line->setText(QString::number((uint32_t)val));
            }
        }
        else if (param.m_type==CRP_FLT32)
        {
            float val;
            Chirp::deserialize(param.m_data, param.m_len, &val, END);
            param.m_line->setText(QString::number(val, 'f', 3));
        }

        // deal with categories-- create category tab if needed
        tab = findCategory(param.m_category);
        if (tab==NULL)
        {
            tab = new QWidget();
            layout = new QGridLayout(tab);
            m_tabs->addTab(tab, param.m_category);
        }
        else
            layout = (QGridLayout *)tab->layout();
        param.m_line->setMaximumWidth(75);
        layout->addWidget(param.m_label, i, 0);
        layout->addWidget(param.m_line, i, 1);
    }
    m_loading = false;

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
    if (!m_applying)
    {
        emit done();
        QDialog::accept();
    }
    m_applying = false;
}

void ConfigDialog::error(QString message)
{
    QMessageBox::critical(NULL, "Error", message);
}


void ConfigDialog::accept()
{
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
        m_applying = true;
        emit save();
    }
}





