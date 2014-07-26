#ifndef PARAMETERS_H
#define PARAMETERS_H

#include <QString>
#include <vector>

class Value
{
public:
    Value(const QString &description,  int value)
    {
        m_description = description;
        m_value = value;
    }

    const QString &description()
    {
        return m_description;
    }

    int value()
    {
        return m_value;
    }

    // meant to only be used for continuous values
    void setValue(int value)
    {
        m_value = value;
        m_description = QString::number(value);
    }

    // meant to only be used for continuous values
    void setDescription(const QString &description)
    {
        m_value = description.toInt();
        m_description = description;
     }

private:
    QString m_description;
    int m_value;
};

typedef std::vector<Value> Values;

class ParameterList;

class Parameter
{
public:
    Parameter(const QString &id, bool continuous=false);
    ~Parameter();

    Value *getValue();
    int get();

    int setValue(int value);
    int setValue(const QString &desc);

    void add(const Value &data);
    void onOff();
    void trueFalse();

    const QString &getId()
    {
        return m_id;
    }

    Values *getValues()
    {
        return &m_values;
    }

private:
    Values m_values;
    QString m_id;
    int m_value; // index
    bool m_continuous;
};


typedef std::vector<Parameter> Parameters;

class ParameterList
{
public:
    ParameterList();
    ~ParameterList();

    Value *getData(const QString &id);
    Parameter *getValues(const QString &id);
    Parameters *getValues();
    int get(const QString &id);
    const QString getDescription(const QString &id);
    int set(const QString &id, const QString &desc);
    int set(const QString &id, int value);

    void add(const Parameter &data);
    void add(const QString &id, const QString &desc); // continuous

private:
    Parameters m_values;
};

#endif // PARAMETERS_H


