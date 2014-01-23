#include "interpreter.h"
#include "configdialog.h"
#include "pixytypes.h"
#include <QLabel>
#include <QMessageBox>
#include <stdexcept>


ConfigWorker::ConfigWorker(ConfigDialog *dialog)
{
    m_dialog = dialog;
}

ConfigWorker::~ConfigWorker()
{
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


        m_dialog->m_paramList.push_back(Param(id, desc, argList[0], len, data));
    }

    emit loaded();
}

void ConfigWorker::save()
{
    QMutexLocker locker(&m_dialog->m_interpreter->m_chirp->m_mutex);
    uint i;
    int res;

    ChirpProc prm_set = m_dialog->m_interpreter->m_chirp->getProc("prm_set");
    if (prm_set<0)
        return;

    for (i=0; i<m_dialog->m_paramList.size(); i++)
    {
        Param &param = m_dialog->m_paramList[i];
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
                emit error(param.m_id + " needs to be a number!");
                return;
            }
            char *id = (char *)param.m_id.data();
            res = m_dialog->m_interpreter->m_chirp->callSync(prm_set, STRING(id), param.m_type, val, END);
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
            char *id = (char *)param.m_id.data();
            res = m_dialog->m_interpreter->m_chirp->callSync(prm_set, STRING(id), param.m_type, val, END);
        }
        if (res<0)
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
    connect(&m_thread, SIGNAL(finished()), worker, SLOT(deleteLater()));
    m_thread.start();

    emit load();


#if 0
    QLineEdit *line = new QLineEdit();
    line->setProperty("id", QVariant("hello"));
    QVariant var = line->property("id");
    qDebug(var.typeName());
#endif
#if 0
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
#endif
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
            param.m_line->setText(QString::number(*(int8_t *)param.m_data));
        else if (param.m_type==CRP_INT16)
            param.m_line->setText(QString::number(*(int16_t *)param.m_data));
        else if (param.m_type==CRP_INT32)
            param.m_line->setText(QString::number(*(int32_t *)param.m_data));
        else if (param.m_type==CRP_FLT32)
            param.m_line->setText(QString::number(*(float *)param.m_data));

        m_ui->gridLayout->addWidget(param.m_label, i, 0);
        m_ui->gridLayout->addWidget(param.m_line, i, 1);
    }
}

void ConfigDialog::saved()
{
}

void ConfigDialog::error(QString message)
{
    QMessageBox::critical(0, "Error", message);
}


void ConfigDialog::accept()
{
    emit save();
    QDialog::accept();
}

void ConfigDialog::reject()
{
    QDialog::reject();
}





