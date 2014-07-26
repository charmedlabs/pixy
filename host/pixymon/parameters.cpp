#include <stdexcept>
#include "parameters.h"

Parameter::Parameter(const QString &id, bool continuous)
{
    m_id = id;
    m_value = -1; // invalid
    m_continuous = continuous;
    if (continuous)
    {
        m_values.push_back(Value("0", 0));
        m_value = 0;
    }
}


Parameter::~Parameter()
{
}

Value *Parameter::getValue()
{
    if (m_value>=0)
        return &m_values[m_value];
    else
        return NULL;
}

int Parameter::get()
{
    if (m_value>=0)
        return m_values[m_value].value();
    return -1;
}

int Parameter::setValue(int value)
{
    unsigned int i;

    if (m_continuous)
    {
        m_values[0].setValue(value);
        return 0;
    }

    for (i=0; i<m_values.size(); i++)
    {
        if (m_values[i].value()==value)
        {
            m_value = i;
            return 0;
        }
    }

    return -1;
}

int Parameter::setValue(const QString &desc)
{
    unsigned int i;

    if (m_continuous)
    {
        m_values[0].setDescription(desc);
        return 0;
    }

    for (i=0; i<m_values.size(); i++)
    {
        if (m_values[i].description()==desc)
        {
            m_value = i;
            return 0;
        }
    }
    return -1;
}


void Parameter::add(const Value &data)
{
    m_values.push_back(data);
}

void Parameter::onOff()
{
    add(Value("Off", 0));
    add(Value("On", 1));
}

void Parameter::trueFalse()
{
    add(Value("False", 0));
    add(Value("True", 1));
}

ParameterList::ParameterList()
{
}


ParameterList::~ParameterList()
{
}


Value *ParameterList::getData(const QString &id)
{
    Parameter *data = getValues(id);
    if (data)
        return data->getValue();
    else
        return NULL;
 }

Parameter *ParameterList::getValues(const QString &id)
{
    unsigned int i;

    for (i=0; i<m_values.size(); i++)
    {
        if (m_values[i].getId()==id)
            return &m_values[i];
    }

    return NULL;
}

Parameters *ParameterList::getValues()
{
    return &m_values;
}


int ParameterList::get(const QString &id)
{
    Value *data;

    data = getData(id);

    if (data)
        return data->value();
    else
        return -1;
}

const QString ParameterList::getDescription(const QString &id)
{
    Value *data;

    data = getData(id);

    if (data)
        return data->description();
    else
        return "";
}

int ParameterList::set(const QString &id, const QString &desc)
{
    Parameter *list = getValues(id);
    if (list)
        return list->setValue(desc);
    else
        return -1;
}

int ParameterList::set(const QString &id, int value)
{
    Parameter *list = getValues(id);
    if (list)
        return list->setValue(value);
    else
        return -1;
}


void ParameterList::add(const Parameter &data)
{
    m_values.push_back(data);
}

void ParameterList::add(const QString &id, const QString &desc)
{
    Parameter data(id, true);
    data.setValue(desc);
    m_values.push_back(data);
}



