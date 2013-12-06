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

VideoWidget::VideoWidget(MainWindow *main) : QWidget((QWidget *)main), m_mutex(QMutex::Recursive)
{
    m_main = main;
    m_xOffset=0;
    m_yOffset=0;
    m_scale = 1.0;
    m_drag = false;
    m_selection = false;

    // set size policy--- preferred aspect ratio
    QSizePolicy policy = sizePolicy();
    policy.setHeightForWidth(true);
    setSizePolicy(policy);

    setMouseTracking(true);
}

VideoWidget::~VideoWidget()
{
}



void VideoWidget::handleImage(QImage image)
{
    QMutexLocker locker(&m_mutex);

    m_images.push_back(image);
}


void VideoWidget::handleFlush()
{
    QMutexLocker locker(&m_mutex);

    if (m_images.size()==0)
        return; // nothing to render...

    m_renderedImages.clear();
    m_renderedImages = m_images;
    m_images.clear();
    repaint();
}

void VideoWidget::clear()
{
    QMutexLocker locker(&m_mutex);

    m_images.clear();
    QImage img(m_width, m_height, QImage::Format_RGB32);
    img.fill(Qt::black);

    handleImage(img);
    handleFlush();
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
    unsigned int i;
    m_width = this->width();
    m_height = this->height();
    float war;
    float pmar;
    QPainter p(this);
    QPixmap bgPixmap;

    if (m_renderedImages.size()==0)
        return;

    bgPixmap = QPixmap::fromImage(m_renderedImages[0]);

    war = (float)m_width/(float)m_height; // widget aspect ratio
    pmar = (float)bgPixmap.width()/(float)bgPixmap.height();

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

    m_scale = (float)m_width/bgPixmap.width();

    p.drawPixmap(QRect(m_xOffset, m_yOffset, m_width, m_height), bgPixmap);

    for (i=1; i<m_renderedImages.size(); i++)
        p.drawPixmap(QRect(m_xOffset, m_yOffset, m_width, m_height), QPixmap::fromImage(m_renderedImages[i]));

    if (m_selection)
        p.drawRect(m_x0-m_xOffset, m_y0-m_yOffset, m_sbWidth, m_sbHeight);
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

