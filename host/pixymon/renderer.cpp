#include <QPainter>
#include <QFont>
#include <QDebug>
#include "renderer.h"
#include "videowidget.h"
#include <chirp.hpp>
#include "calc.h"
#include <math.h>

Renderer::Renderer(VideoWidget *video) : m_background(0, 0)
{
    m_video = video;

    m_rawFrame.m_pixels = new uint8_t[0x10000];

    m_backgroundFrame = true;

    m_mode = 3;

    connect(this, SIGNAL(image(QImage)), m_video, SLOT(handleImage(QImage))); // Qt::BlockingQueuedConnection);
    connect(this, SIGNAL(flushImage()), m_video, SLOT(handleFlush())); //, Qt::BlockingQueuedConnection);
}


Renderer::~Renderer()
{
    delete[] m_rawFrame.m_pixels;
}


inline void Renderer::interpolateBayer(unsigned int width, unsigned int x, unsigned int y, unsigned char *pixel, unsigned int &r, unsigned int &g, unsigned int &b)
{
#if 1
    if (y&1)
    {
        if (x&1)
        {
            r = *pixel;
            g = (*(pixel-1)+*(pixel+1)+*(pixel+width)+*(pixel-width))>>2;
            b = (*(pixel-width-1)+*(pixel-width+1)+*(pixel+width-1)+*(pixel+width+1))>>2;
        }
        else
        {
            r = (*(pixel-1)+*(pixel+1))>>1;
            g = *pixel;
            b = (*(pixel-width)+*(pixel+width))>>1;
        }
    }
    else
    {
        if (x&1)
        {
            r = (*(pixel-width)+*(pixel+width))>>1;
            g = *pixel;
            b = (*(pixel-1)+*(pixel+1))>>1;
        }
        else
        {
            r = (*(pixel-width-1)+*(pixel-width+1)+*(pixel+width-1)+*(pixel+width+1))>>2;
            g = (*(pixel-1)+*(pixel+1)+*(pixel+width)+*(pixel-width))>>2;
            b = *pixel;
        }
    }
#endif
#if 0
    if (y&1)
    {
        if (x&1)
        {
            r = *pixel;
            g = (*(pixel-1)+*(pixel+1))>>1;
            b = (*(pixel-width-1)+*(pixel-width+1))>>1;
        }
        else
        {
            r = (*(pixel-1)+*(pixel+1))>>1;
            g = *pixel;
            b = *(pixel-width);
        }
    }
    else
    {
        if (x&1)
        {
            r = *(pixel+width);
            g = *pixel;
            b = (*(pixel-1)+*(pixel+1))>>1;
        }
        else
        {
            r = (*(pixel+width-1)+*(pixel+width+1))>>1;
            g = (*(pixel-1)+*(pixel+1))>>1;
            b = *pixel;
        }
    }
#endif
#if 0
    if (y&1)
    {
        if (x&1)
        {
            r = *pixel;
            g = (*(pixel-1)+*(pixel+1))>>1;
            b = (*(pixel-width-1)+*(pixel-width+1))>>1;
        }
        else
        {
            r = (*(pixel-1)+*(pixel+1))>>1;
            g = *pixel;
            b = *(pixel-width);
        }
    }
    else
    {
        if (x&1)
        {
            r = *(pixel-width);
            g = *pixel;
            b = (*(pixel-1)+*(pixel+1))>>1;
        }
        else
        {
            r = (*(pixel-width-1)+*(pixel-width+1))>>1;
            g = (*(pixel-1)+*(pixel+1))>>1;
            b = *pixel;
        }
    }
#endif
#if 0
    if (y&1)
    {
        if (x&1)
        {
            r = *pixel;
            g = *(pixel-1);
            b = *(pixel-width-1);
        }
        else
        {
            r = *(pixel-1);
            g = *pixel;
            b = *(pixel-width);
        }
    }
    else
    {
        if (x&1)
        {
            r = *(pixel-width);
            g = *pixel;
            b = *(pixel-1);
        }
        else
        {
            r = *(pixel-width-1);
            g = *(pixel-1);
            b = *pixel;
        }
    }
#endif
}



int Renderer::renderBA81(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame)
{
    uint16_t x, y;
    uint32_t *line;
    uint32_t r, g, b;
    uint8_t *frame0;


    frame0 = frame;
    memcpy(m_rawFrame.m_pixels, frame, width*height);
    m_rawFrame.m_width = width;
    m_rawFrame.m_height = height;

    // skip first line
    frame += width;

    // don't render top and bottom rows, and left and rightmost columns because of color
    // interpolation
    QImage img(width-2, height-2, QImage::Format_RGB32);

    for (y=1; y<height-1; y++)
    {
        line = (unsigned int *)img.scanLine(y-1);
        frame++;
        for (x=1; x<width-1; x++, frame++)
        {
            interpolateBayer(width, x, y, frame, r, g, b);
            // simulate 15 bit color r >>= 4; g >>= 4; b >>= 4; r <<= 4; g <<= 4; b <<= 4;
            *line++ = (0x40<<24) | (r<<16) | (g<<8) | (b<<0);
        }
        frame++;
    }
    // send image to ourselves across threads
    // from chirp thread to gui thread
    emitImage(img);

    m_background = img;

    if (renderFlags&RENDER_FLAG_FLUSH)
        emitFlushImage();

    return 0;
}

int Renderer::renderCCB1(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t numBlobs, uint16_t *blobs)
{
    uint16_t i, left, right, top, bottom;
    float scale = (float)m_video->activeWidth()/width;
    QImage img(width*scale, height*scale, QImage::Format_ARGB32);
    QPainter p;
    uint16_t model;
    QString str;

    numBlobs /= sizeof(BlobA)/sizeof(uint16_t);

    // qDebug() << "numblobs " << numBlobs;

    // render background so we can blend ontop of it
    if (renderFlags&RENDER_FLAG_BLEND_BG)
        renderBackground();

    if (m_backgroundFrame) // if we're the background, we should be opaque
        img.fill(0xff000000);
    else
        img.fill(0x00000000); // otherwise, we're transparent
    p.begin(&img);
    p.setBrush(QBrush(QColor(0xff, 0xff, 0xff, 0x20)));
    p.setPen(QPen(QColor(0xff, 0xff, 0xff, 0xff)));
    QFont font("verdana", 12);
    p.setFont(font);
    for (i=0; i<numBlobs; i++)
    {
        model = blobs[i*5+0];
        left = scale*blobs[i*5+1];
        right = scale*blobs[i*5+2];
        top = scale*blobs[i*5+3];
        bottom = scale*blobs[i*5+4];
        //qDebug() << left << " " << right << " " << top << " " << bottom;
        p.drawRect(left, top, right-left, bottom-top);
        if (model)
        {
#if 0
            label = m_blobs.getLabel(model);
            if (label)
                str = *label;
            else
#endif
            str = str.sprintf("s=%d", model);
            p.setPen(QPen(QColor(0, 0, 0, 0xff)));
            p.drawText(left+1, top+1, str);
            p.setPen(QPen(QColor(0xff, 0xff, 0xff, 0xff)));
            p.drawText(left, top, str);
        }
    }
#if 0
    //deal with coded blobs
    blobs += i*5;
    numBlobs -= i;
    for (i=0; i<numBlobs; i++)
    {
#ifdef RENDER_ANGLE
        int16_t angle;
        model = scale*(blobs[i*6+0]>>3);
        left = scale*(blobs[i*6+1]);
        right = scale*(blobs[i*6+2]);
        top = scale*(blobs[i*6+3]);
        bottom = scale*(blobs[i*6+4]);
        angle = scale*(blobs[i*6+5]);
#else
        model = scale*(blobs[i*5+0]>>3);
        left = scale*(blobs[i*5+1]);
        right = scale*(blobs[i*5+2]);
        top = scale*(blobs[i*5+3]);
        bottom = scale*(blobs[i*5+4]);
#endif
        //qDebug() << left << " " << right << " " << top << " " << bottom;
        p.drawRect(left, top, right-left, bottom-top);
        label = m_blobs.getLabel(model);
        if (label)
            str = *label;
        else
            str = "m=" + code2string(model); // + QChar(0xa6, 0x03); //QChar(0xb8, 0x03);//  QChar(0xa6, 0x03)

        p.setPen(QPen(QColor(0, 0, 0, 0xff)));
        p.drawText(left+1, top+1, str);
        p.setPen(QPen(QColor(0xff, 0xff, 0xff, 0xff)));
        p.drawText(left, top, str);

#ifdef RENDER_ANGLE
        str = QChar(0xa6, 0x03) + str.sprintf("=%d", angle);
        p.setPen(QPen(QColor(0, 0, 0, 0xff)));
        p.drawText(left+1, bottom+12, str);
        p.setPen(QPen(QColor(0xff, 0xff, 0xff, 0xff)));
        p.drawText(left, bottom+11, str);
#endif
    }
    //qDebug() << numBlobs;
#endif
    p.end();

    emitImage(img);
    if (renderFlags&RENDER_FLAG_FLUSH)
        emitFlushImage();

    return 0;
}

int Renderer::renderRect(uint16_t width, uint16_t height, const RectA &rect)
{
    float scale = (float)m_video->activeWidth()/width;
    QImage img(width*scale, height*scale, QImage::Format_ARGB32);
    QPainter p;

    img.fill(0x00000000);
    p.begin(&img);
    p.setBrush(QBrush(QColor(0xff, 0xff, 0xff, 0x20)));
    p.setPen(QPen(QColor(0xff, 0xff, 0xff, 0xff)));
    p.drawRect(scale*rect.m_xOffset, scale*rect.m_yOffset, scale*rect.m_width, scale*rect.m_height);
    p.end();

    emitImage(img);

    return 0;
}

void Renderer::handleRL(QImage *image, uint color, uint row, uint startCol, uint len)
{
    uint *line;
    uint col;

    if (row>=(uint)image->height() || startCol>=(uint)image->width() || startCol+len>=(uint)image->width())
        return;
    line = (unsigned int *)image->scanLine(row);
    for (col=startCol; col<startCol+len; col++)
        line[col] = color;
}

int Renderer::renderCCQ1(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t numVals, uint32_t *qVals)
{
    int32_t row;
    uint32_t i, startCol, length;
    uint8_t model;
    QImage img(width, height, QImage::Format_ARGB32);
    unsigned int palette[] = {0x00000000, 0x80ff0000, 0x8000ff00, 0x800000ff, 0x80ffff00, 0x8000ffff, 0x80ff00ff};

    // if we're a background frame, set alpha to 1.0
    if (m_backgroundFrame)
    {
        for (i=0; i<sizeof(palette)/sizeof(unsigned int); i++)
            palette[i] |= 0xff000000;
    }

    qDebug() << numVals;

    // q val:
    // | 4 bits    | 7 bits      | 9 bits | 9 bits    | 3 bits |
    // | shift val | shifted sum | length | begin col | model  |

    img.fill(palette[0]);
    for (i=0, row=-1; i<numVals; i++)
    {
        if (qVals[i]==0xffffffff)
            continue;
        if (qVals[i]==0)
        {
            row++;
            continue;
        }
        model = qVals[i]&0x07;
        qVals[i] >>= 3;
        startCol = qVals[i]&0x1ff;
        qVals[i] >>= 9;
        length = qVals[i]&0x1ff;
        handleRL(&img, palette[model], row, startCol, length);
    }
    emitImage(img);
    if (renderFlags&RENDER_FLAG_FLUSH)
        emitFlushImage();

    return 0;
}

int Renderer::renderCMV1(uint8_t renderFlags, uint32_t cmodelsLen, uint8_t *cmodels, uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame)
{
    int i;
    uint32_t numBlobs;
    BlobA *blobs;
    uint32_t numQvals;
    uint32_t *qVals;

    qDebug("CMV1 %d %d %d %d", cmodelsLen, width, height, frameLen);

    if (cmodelsLen) // create lookup table
    {
        m_blobs.m_blobs->m_clut->clear();
        for (i=0; i<NUM_MODELS; i++, cmodels+=sizeof(ColorModel))
            m_blobs.m_blobs->m_clut->add((ColorModel *)cmodels, i+1);
    }

    m_blobs.process(Frame8(frame, width, height), &numBlobs, &blobs, &numQvals, &qVals);

    renderBA81(0, width, height, frameLen, frame);
    renderCCQ1(0, width/2, height/2, numQvals, qVals);
    renderCCB1(RENDER_FLAG_FLUSH, width/2, height/2, numBlobs*sizeof(BlobA)/sizeof(uint16_t), (uint16_t *)blobs);

    return 0;
}

// need this because we need synchronized knowledge of whether we're the background image or not
void Renderer::emitImage(const QImage &img)
{
    m_backgroundFrame = false;
    emit image(img);
}

void Renderer::emitFlushImage()
{
    emit flushImage();
    m_backgroundFrame = true;
}


int Renderer::render(uint32_t type, void *args[])
{
    int res;

    // choose fourcc for representing formats fourcc.org
    if (type==FOURCC('B','A','8','1'))
        res = renderBA81(*(uint8_t *)args[0], *(uint16_t *)args[1], *(uint16_t *)args[2], *(uint32_t *)args[3], (uint8_t *)args[4]);
    else if (type==FOURCC('C','C','Q','1'))
        res = renderCCQ1(*(uint8_t *)args[0], *(uint16_t *)args[1], *(uint16_t *)args[2], *(uint32_t *)args[3], (uint32_t *)args[4]);
    else if (type==FOURCC('C', 'C', 'B', '1'))
        res = renderCCB1(*(uint8_t *)args[0], *(uint16_t *)args[1], *(uint32_t *)args[2], *(uint32_t *)args[3], (uint16_t *)args[4]);
    else if (type==FOURCC('C', 'M', 'V', '1'))
        res = renderCMV1(*(uint8_t *)args[0], *(uint32_t *)args[1], (uint8_t *)args[2], *(uint16_t *)args[3], *(uint32_t *)args[4], *(uint32_t *)args[5], (uint8_t *)args[6]);
    else // format not recognized
        return -1;

    return res;
}

int Renderer::renderBackground()
{
    if (m_background.width()!=0)
        emitImage(m_background);

    return 0;
}
