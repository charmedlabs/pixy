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

#include <QMutexLocker>
#include <stdexcept>
#include "parameters.h"


Parameter::Parameter(const QString &id, PType type, const QString &help)
{
    m_id = id;
    m_help = help;
    m_type = type;
    m_dirty = false;
}

Parameter::Parameter(const QString &id, PType type, const QVariant &value, const QString &help)
{
   m_value = value;
   m_id = id;
   m_help = help;
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
    if ((m_type&PT_DATATYPE_MASK)==PT_INT8)
        return "INT8";
    else if ((m_type&PT_DATATYPE_MASK)==PT_INT16)
        return "INT16";
    else if ((m_type&PT_DATATYPE_MASK)==PT_INT32)
        return "INT32";
    else if ((m_type&PT_DATATYPE_MASK)==PT_FLT32)
        return "FLOAT32";
    else if ((m_type&PT_DATATYPE_MASK)==PT_INTS8)
        return "INT8_ARRAY";
    else if ((m_type&PT_DATATYPE_MASK)==PT_STRING)
        return "STRING";
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
    else
        return PT_UNKNOWN;
}

PType Parameter::type()
{
    return m_type;
}

const QVariant &Parameter::value()
{
    if (radio())
        return m_radioValues[m_value.toInt()].m_value;
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
    if (radio())
        return &m_radioValues[m_value.toInt()].m_description;
    return NULL;
}

int Parameter::set(const QVariant &value, bool shadow)
{
    if (shadow && m_saved.isNull())
        m_saved = m_value;

    if (radio())
    {
        for (int i=0; i<m_radioValues.size(); i++)
        {
            if (m_radioValues[i].m_value==value)
            {
                m_value = i;
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
    if (radio())
    {
        for (int i=0; i<m_radioValues.size(); i++)
        {
            if (QString::compare(m_radioValues[i].m_description, description, Qt::CaseInsensitive)==0)
            {
                m_value = i;
                return 0;
            }
        }
    }

    return -1;
}

void Parameter::setHelp(const QString &help)
{
    m_help = help;
}

void Parameter::addRadioValue(const RadioValue &value)
{
    m_radioValues.push_back(value);
}

RadioValues &Parameter::getRadioValues()
{
    return m_radioValues;
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

bool Parameter::radio()
{
    return m_radioValues.length()>1;
}

void Parameter::clearShadow()
{
    if (shadow())
    {
        m_value = m_saved;
        m_saved = QVariant();
        m_dirty = true;  // set dirty bit because we've effectively changed (back)
    }
}

bool Parameter::shadow()
{
    return !m_saved.isNull();
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


ParameterDB::ParameterDB() : m_mutex(QMutex::Recursive)
{
}

ParameterDB::~ParameterDB()
{
}

QVariant ParameterDB::value(const QString &id)
{
    QMutexLocker locker(&m_mutex);
    for (int i=0; i<m_parameters.size(); i++)
    {
        if (QString::compare(m_parameters[i].id(), id, Qt::CaseInsensitive)==0)
            return m_parameters[i].value();
    }
    return QVariant();
}

Parameter *ParameterDB::parameter(const QString &id)
{
    QMutexLocker locker(&m_mutex);
    for (int i=0; i<m_parameters.size(); i++)
    {
        if (QString::compare(m_parameters[i].id(), id, Qt::CaseInsensitive)==0)
            return &m_parameters[i];
    }
    return NULL;
}

Parameters &ParameterDB::parameters()
{
    return m_parameters;
}

int ParameterDB::set(const QString &id, const QVariant &value)
{
    QMutexLocker locker(&m_mutex);
    Parameter *param = parameter(id);

    if (param)
        return param->set(value);

    return -1;
}

int ParameterDB::set(const QString &id, const QString &description)
{
    QMutexLocker locker(&m_mutex);
    Parameter *param = parameter(id);

    if (param)
        return param->set(description);

    return -1;
}

void ParameterDB::add(Parameter param)
{
    QMutexLocker locker(&m_mutex);

    Parameter *pparam = parameter(param.id());

    if (pparam) // if it's in the database, we don't want to add another values
    {
        // copy value and dirty flag, leave everything else intact --- esp the properties, which we're assuming don't change
        pparam->set(param.value());
        pparam->setDirty(param.dirty());
    }
    else  // else put in list
        m_parameters.push_back(param);
}

void ParameterDB::add(const QString &id, PType type, const QVariant &value, const QString &help, const QString &category, uint flags)
{
    Parameter param(id, type, value, help);
    param.setProperty(PP_CATEGORY, category);
    param.setProperty(PP_FLAGS, flags);
    add(param);
}

void ParameterDB::clearShadow()
{
    QMutexLocker locker(&m_mutex);
    for (int i=0; i<m_parameters.size(); i++)
        m_parameters[i].clearShadow();
}

void ParameterDB::clean()
{
    QMutexLocker locker(&m_mutex);
    for (int i=0; i<m_parameters.size(); i++)
        m_parameters[i].setDirty(false);
}


