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

#ifndef DATAEXPORT_H
#define DATAEXPORT_H
#include <QString>
#include <QDir>

class QFile;
class QTextStream;

const QString uniqueFilename(const QDir &dir, const QString &filebase, const QString &extension);

enum ExportType
{
    ET_MATLAB,
    ET_PYTHON,
    ET_R
};


class DataExport
{
public:
    DataExport(const QDir &dir, const QString &filebase, ExportType type);
    DataExport();
    ~DataExport();

    void open(const QDir &dir, const QString &filebase, ExportType type);
    void startArray(int width, const QString &label);
    void addElement(QVariant element);
    void endArray();
    void close();

private:

    bool m_delimiter;
    bool m_array;
    int m_col;
    ExportType m_type;
    int m_width;
    QFile *m_file;
    QTextStream *m_stream;
};

#endif // DATAEXPORT_H
