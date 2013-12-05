#include <QTimer>
#include <QPainter>
#include <QDebug>
#include <QMouseEvent>
#include <QMutexLocker>
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
        g_foreground = image;
        //blend(&image);
}


void VideoWidget::handleFlush()
{
    if (m_backgroundFrame)
        return; // nothing to render...

    *m_pm = QPixmap::fromImage(*m_background);
    g_fpm = QPixmap::fromImage(g_foreground);
    repaint();
    m_backgroundFrame = true;
}

void VideoWidget::clear()
{
    m_pm->fill(Qt::black);
    repaint();
}

int VideoWidget::activeWidth()
{
    QMutexLocker locker(&m_mutex);
    return m_width;
}
int  VideoWidget::activeHeight()
{
    QMutexLocker locker(&m_mutex);
    return m_height;
}

void VideoWidget::paintEvent(QPaintEvent *event)
{
    QMutexLocker locker(&m_mutex);
    m_width = this->width();
    m_height = this->height();
    float war;
    float pmar;
    QPainter p(this);

    war = (float)m_width/(float)m_height; // widget aspect ratio
    pmar = (float)m_pm->width()/(float)m_pm->height();

    p.setCompositionMode(QPainter::CompositionMode_SourceOver);

    if (war>pmar)
    {   // width is greater than video
        m_width = this->height()*pmar;
        m_xOffset = (this->width()-m_width)/2;
        m_yOffset = 0;
    }
    else
    { // height is greater than video
        m_height = this->width()/pmar;
        m_yOffset = (this->height()-m_height)/2;
        m_xOffset = 0;
    }

    m_scale = (float)m_width/m_pm->width();
    //p.save();
    //p.translate(m_xOffset, m_yOffset);
    //p.scale(m_scale, m_scale);
    p.drawPixmap(QRect(m_xOffset, m_yOffset, m_width, m_height), *m_pm);
    p.drawPixmap(QRect(m_xOffset, m_yOffset, m_width, m_height), g_fpm);
    if (m_selection)
        p.drawRect(m_x0-m_xOffset, m_y0-m_yOffset, m_sbWidth, m_sbHeight);
    //p.restore();
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

