#ifndef SIGEDITDIALOG_H
#define SIGEDITDIALOG_H

#include <QDialog>
#include <QPixmap>
#include "monmodule.h"
#include "colorlut.h"

class Interpreter;

namespace Ui {
    class SigEditDialog;
    class SigModule;
}

class SigModule : public MonModule
{
public:
    SigModule(Interpreter *interpreter);
    ~SigModule();
    virtual bool render(uint32_t fourcc, const void *args[]);
    virtual void paramChange();

    // current signatures
    static ColorSignature m_signatures[CL_NUM_SIGNATURES];
    // current signature range multipliers
    static float m_ranges[CL_NUM_SIGNATURES];
    // minimum brightness
    static int32_t m_yMin;
};

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
