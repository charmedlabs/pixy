#ifndef PARAMETERS_H
#define PARAMETERS_H

#include <QString>
#include <QVariant>
#include <QList>

#define PP_DESCRIPTION  "description"
#define PP_CATEGORY     "category"
#define PP_WIDGET       "widget"
#define PP_FLAGS        "flags"
#define PP_TYPE         "type"

struct RadioValue
{
    RadioValue(const QString &description,  const QVariant &value)
    {
        m_description = description;
        m_value = value;
    }

    QString m_description;
    QVariant m_value;
};


typedef QList<RadioValue> RadioValues;

class Parameter
{
public:
    Parameter(const QString &id);
    ~Parameter();

    const QString &id();
    const QVariant &value();
    const QString *description();

    int set(const QVariant &value);
    int setRadio(const QString &description);
    void setDirty(bool dirty);
    bool dirty();

    void addRadioValue(const RadioValue &value);
    void onOff();
    void trueFalse();

    void setProperty(const QString &label, const QVariant &val);
    QVariant property(const QString &label);

private:
    RadioValues m_radioValues; // m_radioValues.size()>0 we're a radio parameter
    int m_radioValue;
    QVariant m_value; // else we're just a regular parameter with value in m_value.

    QString m_id;
    bool m_dirty;
    QList<QPair<QString, QVariant> > m_properties;
};


typedef QList<Parameter> Parameters;

class ParameterDB
{
public:
    ParameterDB();
    ~ParameterDB();

    const QVariant *value(const QString &id);
    Parameter *parameter(const QString &id);
    const QString *description(const QString &id);

    Parameters &parameters();
    int set(const QString &id, const QVariant &value);
    int set(const QString &id, const QString &description);

    void add(const Parameter &parameter);

private:
    Parameters m_parameters;
};

#endif // PARAMETERS_H


