#include <stdexcept>
#include "parameters.h"


Parameter::Parameter(const QString &id, PType type, const QString &help)
{
    m_id = id;
    m_help = help;
    m_radioValue = 0;
    m_type = type;
    m_dirty = false;
}

Parameter::~Parameter()
{
}

const QString &Parameter::id()
{
    return m_id;
}

QString Parameter::typeName()
{
    if (m_type==PT_INT8)
        return "INT8";
    else if (m_type==PT_INT16)
        return "INT16";
    else if (m_type==PT_INT32)
        return "INT32";
    else if (m_type==PT_FLT32)
        return "FLOAT32";
    else if (m_type==PT_INTS8)
        return "INT8_ARRAY";
    else if (m_type==PT_STRING)
        return "STRING";
    else if (m_type==PT_RADIO)
        return "RADIO";
    else
        return "?";
}

PType Parameter::typeLookup(const QString &name)
{
    if (name=="INT8")
        return PT_INT8;
    else if (name=="INT16")
        return PT_INT16;
    else if (name=="INT32")
        return PT_INT32;
    else if (name=="FLOAT32")
        return PT_FLT32;
    else if (name=="INT8_ARRAY")
        return PT_INTS8;
    else if (name=="STRING")
        return PT_STRING;
    else if (name=="RADIO")
        return PT_RADIO;
    else
        return PT_UNKNOWN;
}

PType Parameter::type()
{
    return m_type;
}

const QVariant &Parameter::value()
{
    if (m_type==PT_RADIO)
        return m_radioValues[m_radioValue].m_value;
    else
        return m_value;
}

// deal with signed and less than 32 bit integers
// These need to be treated special because a 16-bit negative integer
// treated as an unsigned 32-bit integer won't translate correctly
int Parameter::valueInt()
{
    int val = m_value.toInt();
    if (m_type==PT_INT16)
    {
        val <<= 16;
        val >>= 16; // sign extend
    }
    if (m_type==PT_INT8)
    {
        val <<= 24;
        val >>= 24; // sign extend
    }
    return val;
}

const QString *Parameter::description()
{
    if (m_type==PT_RADIO)
        return &m_radioValues[m_radioValue].m_description;
    return NULL;
}

int Parameter::set(const QVariant &value)
{
    if (m_type==PT_RADIO)
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
    if (m_type==PT_RADIO)
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

void Parameter::setDirty(bool dirty)
{
    m_dirty = dirty;
}

bool Parameter::dirty()
{
    return m_dirty;
}

const QString &Parameter::help()
{
    return m_help;
}

void Parameter::setProperty(const QString &label, const QVariant &val)
{
    for (int i=0; i<m_properties.size(); i++)
    {
        if (m_properties[i].first==label)
        {
            m_properties[i].second = val;
            return;
        }
    }
    m_properties.push_back(QPair<QString, QVariant>(label, val));
}

QVariant Parameter::property(const QString &label)
{
    for (int i=0; i<m_properties.size(); i++)
    {
        if (m_properties[i].first==label)
            return m_properties[i].second;
    }
    return QVariant();
}


ParameterDB::ParameterDB()
{
}

ParameterDB::~ParameterDB()
{
}

const QVariant *ParameterDB::value(const QString &id)
{
    for (int i=0; i<m_parameters.size(); i++)
    {
        if (QString::compare(m_parameters[i].id(), id, Qt::CaseInsensitive)==0)
            return &m_parameters[i].value();
    }
    return NULL;
}

Parameter *ParameterDB::parameter(const QString &id)
{
    for (int i=0; i<m_parameters.size(); i++)
    {
        if (QString::compare(m_parameters[i].id(), id, Qt::CaseInsensitive)==0)
            return &m_parameters[i];
    }
    return NULL;
}

const QString *ParameterDB::description(const QString &id)
{
    for (int i=0; i<m_parameters.size(); i++)
    {
        if (QString::compare(m_parameters[i].id(), id, Qt::CaseInsensitive)==0)
            return m_parameters[i].description();
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
        if (QString::compare(m_parameters[i].id(), id, Qt::CaseInsensitive)==0)
            return m_parameters[i].set(value);
    }
    return -1;
}

int ParameterDB::set(const QString &id, const QString &description)
{
    for (int i=0; i<m_parameters.size(); i++)
    {
        if (QString::compare(m_parameters[i].id(), id, Qt::CaseInsensitive)==0)
            return m_parameters[i].set(description);
    }
    return -1;
}

void ParameterDB::add(Parameter &parameter)
{
    for (int i=0; i<m_parameters.size(); i++)
    {
        if (QString::compare(m_parameters[i].id(), parameter.id(), Qt::CaseInsensitive)==0)
        {
            m_parameters[i].set(parameter.value());
            return;
        }
    }
    // else put in list
    m_parameters.push_back(parameter);
}


