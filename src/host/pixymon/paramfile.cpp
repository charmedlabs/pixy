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

#include <QTextStream>
#include <QMutexLocker>
#include <QDebug>
#include "paramfile.h"
#include "pixytypes.h"

ParamFile::ParamFile()
{
    m_doc = NULL;
    m_file = NULL;
}

ParamFile::~ParamFile()
{
    close();
}


int ParamFile::open(const QString &filename, bool read)
{
    m_read = read;
    m_file = new QFile(filename);
    m_doc = new QDomDocument();
    if (m_read)
    {
        QString error;
        int line, col;
        if (!m_file->open(QIODevice::ReadOnly | QIODevice::Text))
        {
            close();
            return -1;
        }
        if (!m_doc->setContent(m_file, &error, &line, &col))
        {
            close();
            return -2;
        }
    }
    else // write
    {
        if (!m_file->open(QIODevice::WriteOnly | QIODevice::Text))
        {
            close();
            return -1;
        }
    }

    return 0;
}

int ParamFile::write(const QString &tag, ParameterDB *data)
{
    int i;

    QDomElement element = m_doc->createElement(tag);

    if (data)
    {
        QMutexLocker(data->mutex());
        Parameters &parameters = data->parameters();
        for (i=0; i<parameters.size(); i++)
        {
            QDomElement item = m_doc->createElement("data");
            PType type = parameters[i].type();
            uint flags;
            if (parameters[i].property(PP_FLAGS).isNull())
                flags = 0;
            else
                flags = parameters[i].property(PP_FLAGS).toUInt(); // embedded flags

            item.setAttribute("key", parameters[i].id());
            item.setAttribute("type", parameters[i].typeName());
            if ((type&PT_DATATYPE_MASK)==PT_INTS8)
            {
                QByteArray a = parameters[i].value().toByteArray();
                a = a.toBase64();
                item.setAttribute("value", QString(a));
            }
            else if ((type&PT_DATATYPE_MASK)==PT_INT32 || (type&PT_DATATYPE_MASK)==PT_INT16 || (type&PT_DATATYPE_MASK)==PT_INT8)
            {
                if (flags&PRM_FLAG_SIGNED)
                {
                    int val = parameters[i].valueInt();
                    item.setAttribute("value", QString::number(val));
                }
                else
                {
                    uint val = parameters[i].value().toUInt();
                    item.setAttribute("value", QString::number(val));
                }
            }
            else // handle string and float
                item.setAttribute("value", parameters[i].value().toString());

            element.appendChild(item);
        }
    }

    m_doc->appendChild(element);
    return 0;
}

int ParamFile::read(const QString &tag, ParameterDB *data, bool create)
{
    QDomElement element, nextElement;
    QDomNode node;

    element = m_doc->firstChildElement(tag);
    node = element.firstChild();
    nextElement = node.toElement();

    while(!nextElement.isNull())
    {
        QString key;
        QString type;
        QString value;

        key = nextElement.attribute("key");
        type = nextElement.attribute("type");
        value = nextElement.attribute("value");
        Parameter *pp = data->parameter(key);

        // if we aren't creating the database and the key doesn't exist, skip
        if (create || pp)
        {
            Parameter parameter(key, Parameter::typeLookup(type));

            if (pp)
                parameter = *pp;

            // always set dirty state
            parameter.setDirty(true);

            PType ptype = Parameter::typeLookup(type);

            if ((ptype&PT_DATATYPE_MASK)==PT_FLT32)
            {
                float val = value.toFloat();
                if (parameter.value().toFloat()==val)
                    parameter.setDirty(false);
                parameter.set(val);
            }
            else if ((ptype&PT_DATATYPE_MASK)==PT_INTS8)
            {
                QByteArray a = value.toUtf8();
                if (parameter.value().toByteArray().toBase64()==value)
                    parameter.setDirty(false);
                a = QByteArray::fromBase64(a);
                parameter.set(QVariant(a));
            }
            else if ((ptype&PT_DATATYPE_MASK)==PT_INT8 || (ptype&PT_DATATYPE_MASK)==PT_INT16 || (ptype&PT_DATATYPE_MASK)==PT_INT32)
            {
                int val = value.toInt();
                if (parameter.valueInt()==val)
                    parameter.setDirty(false);
                parameter.set(val);
            }
            else // all other cases (STRING)
            {
                if (parameter.value().toString()==value)
                    parameter.setDirty(false);
                parameter.set(value);
            }

            data->add(parameter);
        }

        node = nextElement.nextSibling();
        nextElement = node.toElement();
    }

    return 0;
}


void ParamFile::close()
{
    if (m_doc && m_file)
    {
        if (!m_read)
        {
            QTextStream out(m_file);
            out << m_doc->toString();
        }

        m_file->close();

        delete m_file;
        delete m_doc;
        m_file = NULL;
        m_doc = NULL;
    }
}


