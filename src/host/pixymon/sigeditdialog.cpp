#include "interpreter.h"
#include "sigeditdialog.h"
#include "ui_sigeditdialog.h"

#include <QPainter>
#include <QImage>
#include <QPaintEngine>
#include <QDebug>
#include "renderer.h"
#include "calc.h"
#include "string.h"

// COLORSPACE UTILS ***********************************************************

#define UV_SCALE_MAX ((0xFF << CL_LUT_ENTRY_SCALE) / 0xFF)

inline void rgb2yuv(uint32_t rgb, int32_t *y, int32_t *u, int32_t *v) {
    int r = (rgb >> 16) & 0xFF;
    int g = (rgb >> 8) & 0xFF;
    int b = rgb & 0xFF;
    *y = r + g + b;
    if (*y == 0) {
        *u = *v = 0;
    }
    else {
        *u = ((r - g) << CL_LUT_ENTRY_SCALE) / *y;
        *v = ((b - g) << CL_LUT_ENTRY_SCALE) / *y;
    }
}

// DIALOG *********************************************************************

SigEditDialog::SigEditDialog(QWidget *parent, Interpreter *interpreter) :
    QDialog(parent),
    ui(new Ui::SigEditDialog)
{
    ui->setupUi(this);

    m_framecount = 0;

    m_interpreter = interpreter;
    m_interpreter->unwait(); // unhang interpreter if it's waiting

    Renderer *renderer = m_interpreter->m_renderer;

    connect(renderer, SIGNAL(image(QImage, uchar)),
            this, SLOT(handleImage(QImage,uchar)));

    show();
}

void SigEditDialog::handleImage(QImage image, uchar renderFlags)
{
    // simulate the image list in videowidget.cpp so we only get the background video
    if (! (renderFlags & RENDER_FLAG_BLEND)) m_framecount = 0;
    m_framecount++;
    if (m_framecount == 1) {
        // set up the map image
        int size = 512;
        QImage map(size, size, QImage::Format_ARGB32);
        map.fill(QColor(0, 0, 0, 0)); // transparency
        QPainter painter;
        painter.begin(&map);
        painter.setPen(Qt::NoPen);
        QRect spot;
        // transfer the camera image onto the colorspace map
        int px, py;
        int32_t y, u, v;
        uint *p;
        for (py = 0; py < image.height(); py++) {
            p = (uint *)image.scanLine(py - 1);
            for (px = 0; px < image.width(); px++) {
                // project into the colorspace
                rgb2yuv(*p, &y, &u, &v);
                // scale onto the image
                u = ((u + UV_SCALE_MAX) * size) / (UV_SCALE_MAX * 2);
                v = ((v + UV_SCALE_MAX) * size) / (UV_SCALE_MAX * 2);
                // draw a spot on the map
                if (y > SigModule::m_yMin) {
                    spot = QRect(u - 2, v - 2, 4, 4);
                    painter.fillRect(spot, *p);
                }
                p++;
            }
        }
        painter.end();
        // add the image to the stack
        m_images.push_back(map);
        while (m_images.count() > MAX_IMAGES) m_images.pop_front();
        // redraw to show the new image
        update(rect());
    }
    // clear the simulated image list if asked to flush
    if (renderFlags & RENDER_FLAG_FLUSH) m_framecount = 0;
}

void SigEditDialog::paintEvent(QPaintEvent *event)
{
    int i, j, y, u, v;
    QPainter painter(this);
    QPixmap pixmap;
    QRect xybounds = this->ui->imageLabel->geometry();
    // map coordinates into (u, v) space
    painter.translate(xybounds.center().x(), xybounds.center().y());
    qreal xscale = (qreal)xybounds.width() / (qreal)(2 * UV_SCALE_MAX);
    qreal yscale = (qreal)xybounds.height() / (qreal)(2 * UV_SCALE_MAX);
    painter.scale(xscale, yscale);
    QRect uvbounds = QRect(QPoint(- UV_SCALE_MAX, - UV_SCALE_MAX),
                     QPoint(UV_SCALE_MAX, UV_SCALE_MAX));
    // draw a background showing the extents of the uv prjection
    //  with reference colors at the edges
    uint32_t refcolors[6] = { 0xFF0000, 0xFFFF00, 0x00FF00,
                              0x00FFFF, 0x0000FF, 0xFF00FF };
    QPoint corners[6];
    painter.setPen(Qt::NoPen);
    for (i = 0; i < 6; i++) {
        rgb2yuv(refcolors[i], &y, &u, &v);
        corners[i] = QPoint(u, v);
    }
    // draw the background
    painter.setBrush(QColor(0x80, 0x80, 0x80));
    painter.drawPolygon(corners, 6);
    // draw the edge
    painter.setBrush(Qt::NoBrush);
    QPen edgePen;
    j = 5;
    for (i = 0; i < 6; i++) {
        QLinearGradient grad = QLinearGradient(QPointF(corners[i]), QPointF(corners[j]));
        grad.setColorAt(0.0, refcolors[i]);
        grad.setColorAt(1.0, refcolors[j]);
        edgePen = QPen(QBrush(grad), 3);
        edgePen.setCosmetic(true);
        edgePen.setCapStyle(Qt::RoundCap);
        painter.setPen(edgePen);
        painter.drawLine(corners[i], corners[j]);
        j = i;
    }
    // draw the stack of colorspace map frames
    for (i = 0; i < m_images.count(); i++) {
        pixmap.convertFromImage(m_images[i]);
        painter.drawPixmap(uvbounds, pixmap);
    }
    // draw signature boxes
    ColorSignature *sig;
    QRect main;
    QRect scaled;
    float range;
    QPen sigPen = QPen(QColor("white"), 2);
    sigPen.setCosmetic(true);
    QBrush sigBrush = QBrush(QColor(255, 255, 255, 64));
    for (i = 0; i < CL_NUM_SIGNATURES; i++) {
        sig = &(SigModule::m_signatures[i]);
        main = QRect(QPoint(sig->m_uMin, sig->m_vMin),
                     QPoint(sig->m_uMax, sig->m_vMax));
        range = SigModule::m_ranges[i];
        scaled.setLeft(sig->m_uMean + (sig->m_uMin - sig->m_uMean) * range);
        scaled.setRight(sig->m_uMean + (sig->m_uMax - sig->m_uMean) * range);
        scaled.setTop(sig->m_vMean + (sig->m_vMin - sig->m_vMean) * range);
        scaled.setBottom(sig->m_vMean + (sig->m_vMax - sig->m_vMean) * range);
        // draw the area after applying the scale factor
        painter.setBrush(sigBrush);
        painter.setPen(Qt::NoPen);
        painter.drawRect(scaled);
        // draw the core area of the signature
        painter.setBrush(Qt::NoBrush);
        painter.setPen(sigPen);
        painter.drawRect(main);
    }
}

SigEditDialog::~SigEditDialog()
{
    delete ui;
}

// MONMODULE IMPLEMENTATION ***************************************************

MON_MODULE(SigModule)

ColorSignature SigModule::m_signatures[CL_NUM_SIGNATURES];
float SigModule::m_ranges[CL_NUM_SIGNATURES];
int32_t SigModule::m_yMin;

SigModule::SigModule(Interpreter *interpreter) : MonModule(interpreter)
{
    // clear signatures and ranges
    memset(m_signatures, 0, sizeof(m_signatures));
    memset(m_ranges, 0, sizeof(m_ranges));
    m_yMin = 0;
}

SigModule::~SigModule() { }

bool SigModule::render(uint32_t fourcc, const void *args[])
{
    return(false);
}

void SigModule::paramChange()
{
    int i;
    QString id;
    QVariant val;
    QByteArray ba;
    uint32_t sigLen;
    uint8_t *sigData;
    // get all existing signatures and ranges
    for (i = 0; i < CL_NUM_SIGNATURES; i++) {
        id.sprintf("signature%d", i + 1);
        val = pixyParameter(id);
        ba = val.toByteArray();
        Chirp::deserialize((uint8_t *)ba.data(), ba.size(), &sigLen, &sigData, END);
        if (sigLen == sizeof(ColorSignature)) {
            memcpy(&(m_signatures[i]), sigData, sizeof(ColorSignature));
        }
        id.sprintf("Signature %d range", i + 1);
        val = pixyParameter(id);
        if (val.canConvert(QVariant::Double)) {
            m_ranges[i] = val.toFloat();
        }
    }
    // get the minimum brightness
    val = pixyParameter("Min brightness");
    if (val.canConvert(QVariant::Double)) {
        m_yMin = val.toFloat() * (3 * 0xFF);
    }
}
