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

#ifndef MONPARAMETERS_H
#define MONPARAMETERS_H

#include <QString>
#include "parameters.h"
#include "monmodule.h"

#define CONFIGFILE_FILENAME     "config.xml"
#define CONFIGFILE_TAG          "PixyMon_parameters"

class MonParameterDB : public ParameterDB
{
public:
    MonParameterDB();
    ~MonParameterDB();

    int load();
    int save();

    void addSlider(const QString &id, const QVariant &value, const QVariant &min, const QVariant &max,
                   const QString &help="", const QString &category="");
    void addCheckbox(const QString &id, bool value, const QString &help="", const QString &category="");

    static QString docPath();
};

#endif // MONPARAMETERS_H
