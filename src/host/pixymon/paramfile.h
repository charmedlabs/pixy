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

#ifndef PARAMFILE_H
#define PARAMFILE_H

#include <QFile>
#include <QDomDocument>
#include "paramfile.h"
#include "parameters.h"

class ParamFile
{
public:
    ParamFile();
    ~ParamFile();

    int open(const QString &filename, bool read);
    int write(const QString &tag, ParameterDB *data);
    int read(const QString &tag, ParameterDB *data, bool create=false);

    void close();

private:
    QDomDocument *m_doc;
    QFile *m_file;
    bool m_read;
};


#endif // PARAMFILE_H


