#include "interpreter.h"
#include "sigeditdialog.h"
#include "ui_sigeditdialog.h"

#include <QPainter>
#include <QImage>
#include <QPaintEngine>
#include <QDebug>
#include "renderer.h"
#include "calc.h"

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
        QImage map(512, 512, QImage::Format_ARGB32);
        QPainter painter;
        painter.begin(&map);
        painter.setPen(Qt::NoPen);
        QRect spot;
        // fill the map with transparency
        map.fill(QColor(0, 0, 0, 0));
        // transfer the camera image onto the colorspace map
        int x, y, r, g, b;
        int32_t u, v;
        uint *p;
        for (y = 0; y < image.height(); y++) {
            p = (uint *)image.scanLine(y - 1);
            for (x = 0; x < image.width(); x++) {
                // get color components
                r = (*p >> 16) & 0xFF;
                g = (*p >> 8) & 0xFF;
                b = *p & 0xFF;
                // convert into (u, v) space
                u = (r - g) + 0xFF;
                v = (b - g) + 0xFF;
                // draw a spot on the map
                spot = QRect(u - 2, v - 2, 4, 4);
                painter.fillRect(spot, *p);
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
    int i;
    QPainter p(this);
    QPixmap pixmap;
    QRect r = this->ui->imageLabel->geometry();
    // fill the background with middle gray so the colors stand out
    p.fillRect(r, QColor(0x80, 0x80, 0x80));
    // draw the stack of colorspace map frames
    for (i = 0; i < m_images.count(); i++) {
        pixmap.convertFromImage(m_images[i]);
        p.drawPixmap(r, pixmap);
    }
}

SigEditDialog::~SigEditDialog()
{
    delete ui;
}
