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
    int read(const QString &tag, ParameterDB *data);

    void close();

private:
    QDomDocument *m_doc;
    QFile *m_file;
    bool m_read;
};


#endif // PARAMFILE_H


