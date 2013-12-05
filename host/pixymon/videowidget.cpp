#include <QTimer>
#include <QPainter>
#include <QDebug>
#include <QMouseEvent>
#include "mainwindow.h"
#include "videowidget.h"
#include "console.h"
// experimental
#include "interpreter.h"
#include "renderer.h"

VideoWidget::VideoWidget(MainWindow *main) : QWidget((QWidget *)main)
{
    m_main = main;
    m_xOffset=0;
    m_yOffset=0;
    m_scale = 1.0;
    m_drag = false;
    m_selection = false;
    m_pm = new QPixmap;
    m_background = new QImage;
    m_backgroundFrame = true;

    // set size policy--- preferred aspect ratio
    QSizePolicy policy = sizePolicy();
    policy.setHeightForWidth(true);
    setSizePolicy(policy);

    setMouseTracking(true);
}

VideoWidget::~VideoWidget()
{
    delete m_pm;
    delete m_background;
}



void VideoWidget::handleImage(QImage image)
{
    if (m_backgroundFrame)
    {
        *m_background = image;
        m_backgroundFrame = false;
    }
    else
        blend(&image);
}

void VideoWidget::handleFlush()
{
    if (m_backgroundFrame)
        return; // nothing to render...

    *m_pm = QPixmap::fromImage(*m_background);
    repaint();
    m_backgroundFrame = true;
}

void VideoWidget::clear()
{
    m_pm->fill(Qt::black);
    repaint();
}

void VideoWidget::paintEvent(QPaintEvent *event)
{

    int width = this->width();
    int height = this->height();
    float war = (float)width/(float)height; // widget aspect ratio
    float pmar = (float)m_pm->width()/(float)m_pm->height();
    QPainter p(this);

    if (war>pmar)
    {   // width is greater than video
        width = this->height()*pmar;
        m_xOffset = (this->width()-width)/2;
        m_yOffset = 0;
    }
    else
    { // height is greater than video
        height = this->width()/pmar;
        m_yOffset = (this->height()-height)/2;
        m_xOffset = 0;
    }

    m_scale = (float)width/m_pm->width();
    p.save();
    p.translate(m_xOffset, m_yOffset);
    p.scale(m_scale, m_scale);
    p.drawPixmap(0, 0, *m_pm);
    if (m_selection)
        p.drawRect((m_x0-m_xOffset)/m_scale+.5, (m_y0-m_yOffset)/m_scale+.5, m_sbWidth/m_scale+.5, m_sbHeight/m_scale+.5);
    p.restore();
}

int VideoWidget::heightForWidth(int w) const
{
    return w/VW_ASPECT_RATIO;
}

void VideoWidget::mouseMoveEvent(QMouseEvent *event)
{
    Qt::MouseButtons b = event->buttons();
    int x = event->x();
    int y = event->y();

    if (m_drag==false && b&Qt::LeftButton)
    {
        m_drag = true;
        m_x0 = x;
        m_y0 = y;
    }
    else if (m_drag==true && b==Qt::NoButton)
    {
        m_drag = false;
    }

    if (m_drag)
    {
        m_sbWidth = x-m_x0;
        m_sbHeight = y-m_y0;
        if (m_x0-m_xOffset>0 && m_y0-m_yOffset>0)
            m_selection = true;

        repaint();
    }
    QWidget::mouseMoveEvent(event);
}

void VideoWidget::mousePressEvent(QMouseEvent *event)
{
    m_selection = false;
    repaint();

    if (m_main->m_interpreter)
        m_main->m_interpreter->m_renderer->setFilter(0x00, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff);
    QWidget::mousePressEvent(event);
}

void VideoWidget::mouseReleaseEvent(QMouseEvent *event)
{
    if (m_selection)
        emit selection((m_x0-m_xOffset)/m_scale+.5, (m_y0-m_yOffset)/m_scale+.5, m_sbWidth/m_scale+.5, m_sbHeight/m_scale+.5);
    QWidget::mouseReleaseEvent(event);
}

void VideoWidget::resizeEvent(QResizeEvent *event)
{
    m_selection = false;
    QWidget::resizeEvent(event);
}

void VideoWidget::acceptInput(uint type)
{

}

void VideoWidget::blend(QImage *foreground)
{
    int i, j;
    unsigned int *fline, *bline;
    unsigned int bpixel, fpixel, br, bg, bb, fr, fg, fb;
    unsigned int ralpha;
    uint32_t alpha;

    alpha = 0x80;
    if (m_background)
    {
        if (foreground->width()!=m_background->width() || foreground->height()!=m_background->height())
            *foreground = foreground->scaled(m_background->width(), m_background->height());

        for (i=0; i<foreground->height(); i++)
        {
            fline = (unsigned int *)foreground->scanLine(i);
            bline = (unsigned int *)m_background->scanLine(i);
            for (j=0; j<m_background->width(); j++)
            {
                bpixel = bline[j];
                bb = bpixel&0xff;
                bpixel >>= 8;
                bg = bpixel&0xff;
                bpixel >>= 8;
                br = bpixel&0xff;

                fpixel = fline[j];
                fb = fpixel&0xff;
                fpixel >>= 8;
                fg = fpixel&0xff;
                fpixel >>= 8;
                fr = fpixel&0xff;
                fpixel >>= 8;
                alpha = fpixel&0xff;
                ralpha = 0x100 - alpha;

                bb = (alpha*fb + ralpha*bb)>>8;
                bg = (alpha*fg + ralpha*bg)>>8;
                br = (alpha*fr + ralpha*br)>>8;

                bline[j] = (br<<16) | (bg<<8) | bb;
            }
        }
    }
}
