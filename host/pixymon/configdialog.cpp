#include "interpreter.h"
#include "configdialog.h"
#include "pixytypes.h"
#include <QLabel>
#include <QMessageBox>
#include <QDebug>
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

    for (i=0; true; i++)
    {
        if (m_dialog->m_interpreter->m_chirp->callSync(prm_getAll, UINT16(i), END_OUT_ARGS, &response, &flags, &argList, &id, &desc, &len, &data, END_IN_ARGS)<0)
            break;

        if (response<0)
            break;

        if (flags&PRM_FLAG_INTERNAL)
            continue;


        m_dialog->m_paramList.push_back(Param(id, "("+typeString(argList[0])+") "+desc, argList[0], len, data));
    }

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
        res = m_dialog->m_interpreter->m_chirp->callSync(prm_set, STRING(id), UINTS8(param.m_len, buf), END_OUT_ARGS, &response, END_IN_ARGS);
        if (res<0 || response<0)
        {
            emit error("There was a problem setting a parameter.");
            return;
        }
    }

    emit saved();
}


ConfigDialog::ConfigDialog(Interpreter *interpreter) : m_ui(new Ui::ConfigDialog)
{

    m_ui->setupUi(this);
    m_interpreter = interpreter;

    ConfigWorker *worker = new ConfigWorker(this);

    worker->moveToThread(&m_thread);
    connect(this, SIGNAL(load()), worker, SLOT(load()));
    connect(this, SIGNAL(save()), worker, SLOT(save()));
    connect(worker, SIGNAL(loaded()), this, SLOT(loaded()));
    connect(worker, SIGNAL(saved()), this, SLOT(saved()));
    connect(worker, SIGNAL(error(QString)), this, SLOT(error(QString)));
    m_thread.start();

    emit load();
}


ConfigDialog::~ConfigDialog()
{
    uint i;

    m_thread.quit();
    m_thread.wait();

    for (i=0; i<m_paramList.size(); i++)
    {
        Param &param = m_paramList[i];
        delete param.m_line;
        delete param.m_label;
    }

    delete m_ui;
}

void ConfigDialog::loaded()
{
    uint i;

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
            param.m_line->setText(QString::number(val));
        }
        else if (param.m_type==CRP_INT16)
        {
            int16_t val;
            Chirp::deserialize(param.m_data, param.m_len, &val, END);
            param.m_line->setText(QString::number(val));
        }
        else if (param.m_type==CRP_INT32)
        {
            int32_t val;
            Chirp::deserialize(param.m_data, param.m_len, &val, END);
            param.m_line->setText(QString::number(val));
        }
        else if (param.m_type==CRP_FLT32)
        {
            float val;
            Chirp::deserialize(param.m_data, param.m_len, &val, END);
            param.m_line->setText(QString::number(val, 'f', 3));
        }

        m_ui->gridLayout->addWidget(param.m_label, i, 0);
        m_ui->gridLayout->addWidget(param.m_line, i, 1);
    }
}

void ConfigDialog::saved()
{
    emit done();
    QDialog::accept();
}

void ConfigDialog::error(QString message)
{
    QMessageBox::critical(NULL, "Error", message);
}


void ConfigDialog::accept()
{
    emit save();
}

void ConfigDialog::reject()
{
    emit done();
    QDialog::reject();
}





