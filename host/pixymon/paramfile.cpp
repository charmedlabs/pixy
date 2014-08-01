#include <QTextStream>
#include <QDebug>
#include "paramfile.h"

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
            item.setAttribute("key", parameters[i].id());
            if (parameters[i].description())
            {
                item.setAttribute("value", *parameters[i].description());
                element.appendChild(item);
            }
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
        QString value;
        key = nextElement.attribute("key");
        value = nextElement.attribute("value");
        data->set(key, value);
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


