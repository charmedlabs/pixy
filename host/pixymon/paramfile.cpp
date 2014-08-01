#include <QTextStream>
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
    m_doc = new QDomDocument;
    if (m_read)
    {
        QString error;
        int line, col;
        if (!m_file->open(QIODevice::ReadOnly | QIODevice::Text))
            return -1;
        if (!m_doc->setContent(m_file, &error, &line, &col))
            return -2;
    }
    else // write
    {
        if (!m_file->open(QIODevice::WriteOnly | QIODevice::Text))
            return -1;
    }

    return 0;
}

int ParamFile::write(const QString &tag, ParameterDB *data)
{
    int i;

    QDomElement element = m_doc->createElement(tag);

    if (data)
    {
        Parameters &parameters = data->parameters();
        for (i=0; i<parameters.size(); i++)
        {
            QDomElement item = m_doc->createElement("data");
            QMetaType::Type type = (QMetaType::Type)parameters[i].value().type();
            QChar etype;
            uint flags;
            if (parameters[i].property(PP_TYPE).isNull())
                etype = 0;
            else
                etype = parameters[i].property(PP_TYPE).toChar(); // embedded type
            if (parameters[i].property(PP_FLAGS).isNull())
                flags = 0;
            else
                flags = parameters[i].property(PP_FLAGS).toUInt(); // embedded flags

            item.setAttribute("key", parameters[i].id());
            if (parameters[i].description())
            {
                item.setAttribute("type", "Radio");
                item.setAttribute("value", *parameters[i].description());
            }
            else
            {
                item.setAttribute("type", QMetaType::typeName(type));
                if (type==QMetaType::QByteArray)
                {
                    QByteArray a = parameters[i].value().toByteArray();
                    a = a.toBase64();
                    item.setAttribute("value", QString(a));
                }
                else if (etype==PRM_INT32 || etype==PRM_INT16 || etype==PRM_INT8)
                {
                    if (flags&PRM_FLAG_SIGNED)
                    {
                        int val = parameters[i].value().toInt();
                        if (etype==PRM_INT16)
                        {
                            val <<= 16;
                            val >>= 16; // sign extend
                        }
                        if (etype==PRM_INT8)
                        {
                            val <<= 24;
                            val >>= 24; // sign extend
                        }
                        item.setAttribute("value", QString::number(val));
                    }
                    else
                    {
                        int val = parameters[i].value().toUInt();
                        item.setAttribute("value", QString::number(val));
                    }
                }
                else
                    item.setAttribute("value", parameters[i].value().toString());
            }
            element.appendChild(item);
        }
    }

    m_doc->appendChild(element);
    return 0;
}

int ParamFile::read(const QString &tag, ParameterDB *data)
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
        if (type=="Radio")
            data->set(key, value);
        else
        {
            if (type=="float")
            {
                float val = value.toFloat();
                data->set(key, val);
            }
            else if (type=="QString")
                data->set(key, QVariant(value));
            else if (type=="QByteArray")
            {
                QByteArray a = value.toUtf8();
                a = QByteArray::fromBase64(a);
                data->set(key, QVariant(a));
            }
            else // must be integer type
            {
                int val = value.toInt();
                data->set(key, val);
            }
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


