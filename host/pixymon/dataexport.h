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
