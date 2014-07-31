#include <stdexcept>
#include "parameters.h"


Parameter::Parameter(const QString &id)
{
    m_id = id;
    m_radioValue = 0;
}

Parameter::~Parameter()
{
}

const QString &Parameter::id()
{
    return m_id;
}

const QVariant &Parameter::value()
{
    if (m_radioValues.size()>0)
        return m_radioValues[m_radioValue].m_value;
    else
        return m_value;
}

const QString *Parameter::description()
{
    if (m_radioValues.size()>0)
        return &m_radioValues[m_radioValue].m_description;
    return NULL;
}

int Parameter::set(const QVariant &value)
{
    if (m_radioValues.size()>0)
    {
        for (int i=0; i<m_radioValues.size(); i++)
        {
            if (m_radioValues[i].m_value==value)
            {
                m_radioValue = i;
                return 0;
            }
        }
        return -1;
    }
    else
        m_value = value;

    return 0;
}

int Parameter::setRadio(const QString &description)
{
    if (m_radioValues.size()>0)
    {
        for (int i=0; i<m_radioValues.size(); i++)
        {
            if (QString::compare(m_radioValues[i].m_description, description, Qt::CaseInsensitive)==0)
            {
                m_radioValue = i;
                return 0;
            }
        }
    }

    return -1;
}

void Parameter::addRadioValue(const RadioValue &value)
{
    m_radioValues.push_back(value);
}

void Parameter::onOff()
{
    addRadioValue(RadioValue("Off", 0));
    addRadioValue(RadioValue("On", 1));
}

void Parameter::trueFalse()
{
    addRadioValue(RadioValue("True", 0));
    addRadioValue(RadioValue("False", 1));
}




ParameterDB::ParameterDB()
{
}

ParameterDB::~ParameterDB()
{
    for (int i; i<m_parameters.size(); i++)
        delete m_parameters[i];
}

const QVariant *ParameterDB::value(const QString &id)
{
    for (int i=0; i<m_parameters.size(); i++)
    {
        if (QString::compare(m_parameters[i]->id(), id, Qt::CaseInsensitive)==0)
            return &m_parameters[i]->value();
    }
    return NULL;
}

Parameter *ParameterDB::parameter(const QString &id)
{
    for (int i=0; i<m_parameters.size(); i++)
    {
        if (QString::compare(m_parameters[i]->id(), id, Qt::CaseInsensitive)==0)
            return m_parameters[i];
    }
    return NULL;
}

const QString *ParameterDB::description(const QString &id)
{
    for (int i=0; i<m_parameters.size(); i++)
    {
        if (QString::compare(m_parameters[i]->id(), id, Qt::CaseInsensitive)==0)
            return m_parameters[i]->description();
    }
    return NULL;
}

Parameters &ParameterDB::parameters()
{
    return m_parameters;
}

int ParameterDB::set(const QString &id, const QVariant &value)
{
    for (int i=0; i<m_parameters.size(); i++)
    {
        if (QString::compare(m_parameters[i]->id(), id, Qt::CaseInsensitive)==0)
            return m_parameters[i]->set(value);
    }
    return -1;
}

int ParameterDB::set(const QString &id, const QString &description)
{
    for (int i=0; i<m_parameters.size(); i++)
    {
        if (QString::compare(m_parameters[i]->id(), id, Qt::CaseInsensitive)==0)
            return m_parameters[i]->set(description);
    }
    return -1;
}

void ParameterDB::add(Parameter *parameter)
{
    m_parameters.push_back(parameter);
}


