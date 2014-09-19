#ifndef MONPARAMETERS_H
#define MONPARAMETERS_H

#include <QString>
#include "parameters.h"

#define CONFIGFILE_FILENAME     "config.xml"
#define CONFIGFILE_TAG          "PixyMon_parameters"

class MonParameterDB : public ParameterDB
{
public:
    MonParameterDB();
    ~MonParameterDB();

    int load();
    int save();

    static QString docPath();
};

#endif // MONPARAMETERS_H
