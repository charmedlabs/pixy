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

#include <QTimer>
#include <QPainter>
#include <QDebug>
#include <QMouseEvent>
#include <QMetaType>
#include "mainwindow.h"
#include "videowidget.h"
#include "console.h"
// experimental
//#include "interpreter.h"
#include "renderer.h"

VideoWidget::VideoWidget(MainWindow *main) : QWidget((QWidget *)main)
{
    qRegisterMetaType<VideoWidget::InputMode>("VideoWidget::InputMode");

    m_main = main;
    m_xOffset=0;
    m_yOffset=0;
    m_scale = 1.0;
    m_drag = false;
    m_inputMode = NONE;
    m_selection = false;
    m_aspectRatio = VW_ASPECT_RATIO;

    // set size policy--- preferred aspect ratio
    QSizePolicy policy = sizePolicy();
    policy.setHeightForWidth(true);
    setSizePolicy(policy);

    setMouseTracking(true);
}

VideoWidget::~VideoWidget()
{
}


//0: don’t render, just put on image list, first
//RENDER_FLAG_BLEND: tack on to end of image list
//RENDER_FLAG_FLUSH put on image list first, and render
//RENDER_FLAG_BLEND | RENDER_FLAG_FLUSH: tack on end of image list and render

void VideoWidget::handleImage(QImage image, uchar renderFlags)
{
    if (!(renderFlags&RENDER_FLAG_BLEND))
        m_images.clear();
    m_images.push_back(image);
    if (renderFlags&RENDER_FLAG_FLUSH)
    {
        m_renderedImages.clear();
        m_renderedImages = m_images;
        m_images.clear();
        repaint();
    }
}


void VideoWidget::clear()
{
    m_images.clear();
    QImage img(m_width, m_height, QImage::Format_RGB32);
    img.fill(0xff000000);

    handleImage(img, RENDER_FLAG_FLUSH);
}

int VideoWidget::activeWidth()
{
    return m_width;
}

int  VideoWidget::activeHeight()
{
    return m_height;
}

void VideoWidget::paintEvent(QPaintEvent *event)
{
    unsigned int i;
    m_width = this->width();
    m_height = this->height();
    float war;
    float pmar;
    QPainter p(this); // we could render to a QImage instead of to a widget.  This might take longer, but
    // it would allow us to save off the blended image, e.g. to a file.
    QPixmap bgPixmap;

    if (m_renderedImages.size()==0)
        return;

    // background pixmap
    bgPixmap = QPixmap::fromImage(m_renderedImages[0]);

    // calc aspect ratios
    war = (float)m_width/(float)m_height; // widget aspect ratio
    pmar = (float)bgPixmap.width()/(float)bgPixmap.height();

    if (pmar!=m_aspectRatio)
    {
        m_aspectRatio = pmar;
        updateGeometry();
    }

    // set blending mode
    p.setCompositionMode(QPainter::CompositionMode_SourceOver);

    // figure out if we need to offset our rendering rectangle
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

    // figure out scale between background resolution and active width of widget
    m_scale = (float)m_width/bgPixmap.width();

    // draw background
    p.drawPixmap(QRect(m_xOffset, m_yOffset, m_width, m_height), bgPixmap);

    // draw/blend foreground images
    for (i=1; i<m_renderedImages.size(); i++)
        p.drawPixmap(QRect(m_xOffset, m_yOffset, m_width, m_height), QPixmap::fromImage(m_renderedImages[i]));

    // draw selection rectangle
    if (m_selection)
    {
        p.setBrush(QBrush(QColor(0xff, 0xff, 0xff, 0x20)));
        p.setPen(QPen(QColor(0xff, 0xff, 0xff, 0xff)));
        p.drawRect(m_x0, m_y0, m_sbWidth, m_sbHeight);
    }

    QWidget::paintEvent(event);
}

int VideoWidget::heightForWidth(int w) const
{
    return w/m_aspectRatio;
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

    if (m_drag && m_inputMode==REGION)
    {
        m_sbWidth = x-m_x0;
        m_sbHeight = y-m_y0;
        // check if we have clicked outside of active region
        if (m_x0-m_xOffset>0 && m_y0-m_yOffset>0 && m_x0<m_xOffset+m_width && m_y0<m_yOffset+m_height)
        {
            m_selection = true;
            // limit drag to within active region
            if (m_x0-m_xOffset+m_sbWidth>m_width)
                m_sbWidth = m_width-m_x0+m_xOffset;
            if (m_y0-m_yOffset+m_sbHeight>m_height)
                m_sbHeight = m_height-m_y0+m_yOffset;
            if (m_x0-m_xOffset+m_sbWidth<0)
                m_sbWidth = -m_x0+m_xOffset;
            if (m_y0-m_yOffset+m_sbHeight<0)
                m_sbHeight = -m_y0+m_yOffset;
            repaint();
        }
    }
    QWidget::mouseMoveEvent(event);
}

void VideoWidget::mousePressEvent(QMouseEvent *event)
{
    m_selection = false;
    repaint();

    QWidget::mousePressEvent(event);
}

void VideoWidget::mouseReleaseEvent(QMouseEvent *event)
{
    int x, y, width, height;

    if (m_selection)
    {

        x = (m_x0-m_xOffset)/m_scale+.5;
        y = (m_y0-m_yOffset)/m_scale+.5;

        width = m_sbWidth/m_scale+.5;
        height = m_sbHeight/m_scale+.5;

        // deal with box inversion
        if (width<0)
        {
            x += width;
            width = -width;
        }
        if (height<0)
        {
            y += height;
            height = -height;
        }
        emit selection(x, y, width, height);
        acceptInput(NONE);
        //DBG("%d %d %d %d", x, y, width, height);
        m_selection = false;
    }
    else if (m_inputMode==POINT)
    {
        x = (event->x()-m_xOffset)/m_scale+.5;
        y = (event->y()-m_yOffset)/m_scale+.5;
        emit selection(x, y, 0, 0);
        acceptInput(NONE);
    }
    QWidget::mouseReleaseEvent(event);
}

void VideoWidget::resizeEvent(QResizeEvent *event)
{
    m_selection = false;
    QWidget::resizeEvent(event);
}

void VideoWidget::acceptInput(VideoWidget::InputMode mode)
{
    m_inputMode = mode;
    if (mode==REGION || mode==POINT)
        setCursor(Qt::CrossCursor);
    else
    {
        m_selection = false;
        setCursor(Qt::ArrowCursor);
    }
}

