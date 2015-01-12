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

#include <QDir>
#include <QDesktopServices>
#if QT_VERSION>QT_VERSION_CHECK(5,0,0)
#include <QStandardPaths>
#endif
#include "monparameterdb.h"
#include "pixymon.h"
#include "paramfile.h"
#include "pixytypes.h"

MonParameterDB::MonParameterDB() : ParameterDB()
{
    add("Document folder", PT_STRING, docPath(),
        "The directory where images and other data files are saved", "", PRM_FLAG_PATH);
    load();
}

MonParameterDB::~MonParameterDB()
{
    save();
}

QString MonParameterDB::docPath()
{
    QString path;

#if QT_VERSION>QT_VERSION_CHECK(5,0,0)
    path = QStandardPaths::writableLocation(QStandardPaths::DocumentsLocation);
#else
    path = QDesktopServices::storageLocation(QDesktopServices::DocumentsLocation);
#endif
    QDir dir(path);

    if (!dir.exists(PM_DEFAULT_DATA_DIR))
    {
        dir.mkpath(path); // make dirs if necessary
        dir.mkdir(PM_DEFAULT_DATA_DIR);
    }
    dir.cd(PM_DEFAULT_DATA_DIR);

    return dir.absolutePath();
}


int MonParameterDB::save()
{
    ParamFile pf;
    QFileInfo fi(docPath(), CONFIGFILE_FILENAME);

    pf.open(fi.absoluteFilePath(), false);
    pf.write(CONFIGFILE_TAG, this);
    pf.close();
    return 0;
}

int MonParameterDB::load()
{
    // initialize paramters
    QString dp = docPath();

    // now see if config file exists
    QFileInfo fi(dp, CONFIGFILE_FILENAME);
    ParamFile pf;
    if (pf.open(fi.absoluteFilePath(), true)>=0)
    {
        pf.read(CONFIGFILE_TAG, this);
        pf.close();
    }
    else // there was an error, so write (or rewrite) config file
        save();

    return 0;
}


void MonParameterDB::addSlider(const QString &id, const QVariant &value, const QVariant &min, const QVariant &max,
                               const QString &help, const QString &category)
{
    if (value.type()==(QVariant::Type)QMetaType::Float || value.type()==QVariant::Double)
        add(id, PT_FLT32, value, help, category, PRM_FLAG_SLIDER);
    else
        add(id, PT_INT32, value, help, category, PRM_FLAG_SLIDER);

    Parameter *param = parameter(id);
    param->setProperty(PP_MIN, min);
    param->setProperty(PP_MAX, max);
}

void MonParameterDB::addCheckbox(const QString &id, bool value, const QString &help, const QString &category)
{
    add(id, PT_INT8, value, help, category, PRM_FLAG_CHECKBOX);
}
