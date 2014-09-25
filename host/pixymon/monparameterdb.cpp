#include <QDir>
#include <QDesktopServices>
#if QT_VERSION>QT_VERSION_CHECK(5,0,0)
#include <QStandardPaths>
#endif
#include "monparameterDB.h"
#include "pixymon.h"
#include "paramfile.h"

MonParameterDB::MonParameterDB() : ParameterDB()
{
    add("Document folder", PT_PATH, docPath(),
        "The directory where images and other data files are saved");
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
        dir.mkdir(PM_DEFAULT_DATA_DIR);
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


void MonParameterDB::addSlider(const QString &id, const QVariant &value, const QVariant &min, const QVariant &max, MonModule *mm,
                               const QString &help, const QString &category)
{

}

void MonParameterDB::addBool(const QString &id, bool value, MonModule *mm, const QString &help, const QString &category)
{
    add(id, PT_BOOL, value, help, category);
    Parameter *param = parameter(id);
    param->setProperty(PP_MM_CALLBACK, (qlonglong)mm);
}
