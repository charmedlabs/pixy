#ifndef SIGEDITDIALOG_H
#define SIGEDITDIALOG_H

#include <QDialog>
#include <QPixmap>
#include "monmodule.h"

class Interpreter;

namespace Ui {
class SigEditDialog;
}

class SigEditDialog : public QDialog
{
    Q_OBJECT

public:
    SigEditDialog(QWidget *parent, Interpreter *interpreter);
    ~SigEditDialog();

public slots:
    void handleImage(QImage image, uchar renderFlags);

protected:
    void paintEvent(QPaintEvent *event);

private:
    Ui::SigEditDialog *ui;

    QImage m_satmap;
    QList<QImage> m_images;
    static const int MAX_IMAGES = 4;
    int m_framecount;

    Interpreter *m_interpreter;
};

#endif // SIGEDITDIALOG_H
