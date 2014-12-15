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

#include <stdexcept>
#include <QFile>
#include <QTextStream>
#include "dataexport.h"

const QString uniqueFilename(const QDir &dir, const QString &filebase, const QString &extension)
{
    int i;

    for (i=1; true; i++)
    {
        QFileInfo testFile(dir, filebase + QString::number(i) + "." + extension);
        if (!testFile.exists())
            return testFile.absoluteFilePath();
    }
}



DataExport::DataExport(const QDir &dir, const QString &filebase, ExportType type)
{
    m_file = NULL;
    m_stream = NULL;
    m_array = false;
    open(dir, filebase, type);
}

DataExport::DataExport()
{
    m_file = NULL;
    m_stream = NULL;
    m_array = false;
}

DataExport::~DataExport()
{
    close();
}

void DataExport::open(const QDir &dir, const QString &filebase, ExportType type)
{
    QString extension;

    m_type = type;
    if (m_type==ET_MATLAB)
        extension = "m";
    else if (m_type==ET_PYTHON)
        extension = "py";
    else if (m_type==ET_R)
        extension = "r";
    QString filename = uniqueFilename(dir, filebase, extension);
    m_file = new QFile(filename);
    if (!m_file->open(QIODevice::WriteOnly | QIODevice::Text))
        throw std::runtime_error("Unable to open file" + filename.toStdString() + ".");
    m_stream = new QTextStream(m_file);

}

void DataExport::startArray(int width, const QString &label)
{
    if (m_file==NULL)
        return;

    m_width = width;
    m_col = 0;
    m_array = true;

    if (m_type==ET_MATLAB)
    {
        *m_stream << label << "=[\n";
        m_delimiter = false;
    }
    else if (m_type==ET_PYTHON)
    {
    }
    else if (m_type==ET_R)
    {
    }
}

void DataExport::addElement(QVariant element)
{
    if (m_file==NULL || m_stream==NULL || m_array==false)
        return;

    if (m_type==ET_MATLAB)
    {
        if (m_delimiter)
            *m_stream << ", ";
        if (element.type()==(QVariant::Type)QMetaType::Float)
            *m_stream << element.toFloat();
        else if (element.type()==QVariant::Double)
            *m_stream << element.toDouble();
        else if (element.type()==QVariant::Int)
            *m_stream << element.toInt();
        else if (element.type()==QVariant::UInt)
            *m_stream << element.toUInt();
        m_col++;
        if (m_col==m_width)
        {
            *m_stream << "\n";
            m_col = 0;
            m_delimiter = false;
        }
        else
            m_delimiter = true;

    }
    else if (m_type==ET_PYTHON)
    {
    }
    else if (m_type==ET_R)
    {
    }

}


void DataExport::endArray()
{
    if (m_file==NULL || m_stream==NULL)
        return;

    m_array = false;

    if (m_type==ET_MATLAB)
    {
        if (m_delimiter)
            *m_stream << "\n";
        *m_stream << "];\n";
    }
    else if (m_type==ET_PYTHON)
    {
    }
    else if (m_type==ET_R)
    {
    }
}

void DataExport::close()
{
    if (m_array)
        endArray();

    if (m_file)
    {
        m_file->close();
        if (m_stream)
        {
            delete m_stream;
            m_stream = NULL;
        }
        delete m_file;
        m_file = NULL;
    }

}

