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

// So, the parameters set of classes needs to do or facilitate several things in order to be fully useful:
//
// 1) Get parameters from Pixy and into ParameterDB (load Pixy parameters)
// 2) Get ParameterDB into parameter dialog (render parameters)
// 3) Get parameter dialog into ParameterDB (update gui-modified values)
// 4) Get ParameterDB back into Pixy (save Pixy parameters)
// 5) Get ParameterDB into config file (XML) (write parameters to file)
// 6) Get config file (XML) into ParameterDB (read parameters from file)

#ifndef PARAMETERS_H
#define PARAMETERS_H

#include <QString>
#include <QVariant>
#include <QList>

#define PP_CATEGORY     "category"
#define PP_WIDGET       "widget"
#define PP_FLAGS        "flags"

#define PRM_FLAG_INTERNAL            0x01
#define PRM_FLAG_ADVANCED            0x02
#define PRM_FLAG_HEX_FORMAT          0x10
#define PRM_FLAG_SIGNED              0x80


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

#define  PT_RADIO_MASK  0x8000

enum PType
{
    PT_UNKNOWN = 0x00,

    // type values copied from Chirp and are Chirp-compatible
    PT_INT8 = 0x01,
    PT_INT16 = 0x02,
    PT_INT32 = 0x04,
    PT_FLT32 = 0x14,
    PT_INTS8 = 0x81,
    PT_STRING = 0xa1,

    // radio types
    PT_INT8_RADIO = PT_INT8 | PT_RADIO_MASK,
    PT_INT16_RADIO = PT_INT16 | PT_RADIO_MASK,
    PT_INT32_RADIO = PT_INT32 | PT_RADIO_MASK,
    PT_FLT32_RADIO = PT_FLT32 | PT_RADIO_MASK,
    PT_INTS8_RADIO = PT_INTS8 | PT_RADIO_MASK,
    PT_STRING_RADIO = PT_STRING | PT_RADIO_MASK,

    // other types
    PT_PATH = 0x100
};

typedef QList<RadioValue> RadioValues;

class Parameter
{
public:
    Parameter(const QString &id, PType type, const QString &help="");
    Parameter(const QString &id, const QVariant &value, PType type, const QString &help="");
    ~Parameter();

    QString typeName();
    static PType typeLookup(const QString &name);
    PType type();

    const QString &id();
    const QVariant &value();
    int valueInt();
    const QString *description();

    int set(const QVariant &value);
    int setRadio(const QString &description);
    void setDirty(bool dirty);
    bool dirty();

    void addRadioValue(const RadioValue &value);
    void onOff();
    void trueFalse();

    const QString &help();

    void setProperty(const QString &label, const QVariant &val);
    QVariant property(const QString &label);

private:
    RadioValues m_radioValues; // m_radioValues.size()>0 we're a radio parameter
    int m_radioValue;

    QVariant m_value; // else we're just a regular parameter with value in m_value.
    PType m_type; // only applies to m_value

    QString m_id;
    QString m_help;
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

    void add(Parameter param);

private:
    Parameters m_parameters;
};

#endif // PARAMETERS_H


