#include <QPainter>
#include <QFont>
#include <QDebug>
#include "renderer.h"
#include "videowidget.h"
#include "../../device/libpixy/chirp.hpp"
#include "calc.h"
#include <math.h>

uint32_t num_comps;
int16_t* comps = NULL;
void VISUcallback(QImage* image);

Renderer::Renderer(VideoWidget *video)
{
    m_video = video;

    m_frameData = new uint8_t[0x20000];
    m_backgroundFrame = true;

    m_hmin = 0x00;
    m_hmax = 0xff;
    m_smin = 0x00;
    m_smax = 0xff;
    m_vmin = 0x00;
    m_vmax = 0xff;
    m_cmin = 0x00;
    m_cmax = 0xff;
    m_lut = NULL;

    m_mode = 3;

    connect(this, SIGNAL(image(QImage)), m_video, SLOT(handleImage(QImage))); // Qt::BlockingQueuedConnection);
    connect(this, SIGNAL(flushImage()), m_video, SLOT(handleFlush())); //, Qt::BlockingQueuedConnection);
}


Renderer::~Renderer()
{
    free(comps);
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


#define MAX(a, b)  (a>b ? a : b)
#define MIN(a, b)  (a<b ? a : b)

void RGBtoHSV(uint8_t r, uint8_t g, uint8_t b, uint8_t *h, uint8_t *s, uint8_t *v, uint8_t *c)
{
    uint8_t min, max, delta;
    int hue;
    min = MIN(r, g);
    min = MIN(min, b);
    max = MAX(r, g);
    max = MAX(max, b);

    *v = max;
    delta = max - min;
    if (max!=0)
        *s = ((int)delta<<8)/max;
    if (max==0 || delta==0)
    {
        *s = 0;
        *h = 0;
        *c = 0;
        return;
    }
    if (r==max)
        hue = (((int)g - (int)b)<<8)/delta;         // between yellow & magenta
    else if (g==max)
        hue = (2<<8) + (((int)b - (int)r)<<8)/delta;     // between cyan & yellow
    else
        hue = (4<<8) + (((int)r - (int)g)<<8)/delta;     // between magenta & cyan
    if(hue < 0)
        hue += 6<<8;
    hue /= 6;
    *h = hue;
    *c = delta;
}



int Renderer::renderBA81(uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame)
{
    uint16_t x, y;
    uint32_t *line;
    uint32_t r, g, b;
    uint8_t *frame0;


    frame0 = frame;
    memcpy(m_frameData, frame, width*height);

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

    if (m_mode&0x01)
    {
        uint16_t numBlobs;
        uint16_t *blobs;
        uint32_t numQVals;
        uint32_t *qVals;

        m_blobs.process(width, height, frameLen, frame0, &numBlobs, &blobs, &numQVals, &qVals);
#if 1
        if (m_mode&0x04)
            renderCCQ1(width/2, height/2, numQVals, qVals);
        if (m_mode&0x02)
            renderCCB1(width, height, numBlobs, blobs);
#endif
    }

    return 0;
}

int Renderer::renderCCB1(uint16_t width, uint16_t height, uint16_t numBlobs, uint16_t *blobs)
{
    uint16_t i, left, right, top, bottom;
    float scale = (float)m_video->activeWidth()/width;
    QImage img(width*scale, height*scale, QImage::Format_ARGB32);
    QPainter p;
    uint16_t model;
    QString str;
    QString *label;


    //qDebug() << "numblobs " << numBlobs;
    img.fill(0x00000000);
    p.begin(&img);
    p.setBrush(QBrush(QColor(0xff, 0xff, 0xff, 0x20)));
    p.setPen(QPen(QColor(0xff, 0xff, 0xff, 0xff)));
    QFont font("verdana", 12);
    p.setFont(font);
    for (i=0; i<numBlobs; i++)
    {
        model = blobs[i*5+0];
        if (model==0)
            break;
        left = scale*blobs[i*5+1];
        right = scale*blobs[i*5+2];
        top = scale*blobs[i*5+3];
        bottom = scale*blobs[i*5+4];
        //qDebug() << left << " " << right << " " << top << " " << bottom;
        p.drawRect(left, top, right-left, bottom-top);
        label = m_blobs.getLabel(model);
        if (label)
            str = *label;
        else
            str = str.sprintf("m=%d", model);

        p.setPen(QPen(QColor(0, 0, 0, 0xff)));
        p.drawText(left+1, top+1, str);
        p.setPen(QPen(QColor(0xff, 0xff, 0xff, 0xff)));
        p.drawText(left, top, str);
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

    return 0;
}

int Renderer::renderVISU(uint32_t cc_num, int16_t* c_components)
{
#if 0
    unsigned int x, y;
    unsigned int *line;
    unsigned int r, g, b;
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
            *line++ = (0xff<<24) | (r<<16) | (g<<8) | (b<<0);
        }
        frame++;
    }
#endif


    // Save the arguments for later
    num_comps = cc_num;
    comps = (int16_t*)realloc(comps, sizeof(int16_t)*cc_num);
    memcpy(comps, c_components, sizeof(int16_t)*cc_num);
	
	// DEBUG
    int height = 200;
    int width = 320;
    double yscale = (double)100 / (double)height;
    double xscale = (double)160 / (double)width;
    int t, r, b, l;
    int m_area = 0;

    for (unsigned int i = 0; i < (num_comps/4); i++)
    {
        int16_t top = 0, right = 0, bottom = 0, left = 0;
        top = int(double(comps[(i*4)+0]) * yscale);
        right = int(double(comps[(i*4)+1]) * xscale);
        bottom = int(double(comps[(i*4)+2]) * yscale);
        left = int(double(comps[(i*4)+3]) * xscale);

        int area = (bottom - top)*(right - left);
        if (area > m_area)
        {
            m_area = area;
            t = top, r = right, b = bottom, l = left;
        }
    }
    //qDebug() << "t: " << t << "\tr: " << r << "\tb: " << b << "\tl: " << l;


    return 0;
}

void VISUcallback(QImage* image)
{
    // TEMP
    int height = 200;
    int width = 320;

    double yscale = (double)image->height() / (double)height;
    double xscale = (double)image->width() / (double)width;

    // Start painting video widget
    QPainter p;
    p.begin(image);
    p.setPen(QPen(QColor(Qt::color0)));
    p.setBrush(QBrush(QColor(Qt::color0), Qt::NoBrush));
    for (unsigned int i = 0; i < (num_comps/4); i++)
    {
        int16_t top = 0, right = 0, bottom = 0, left = 0;
        top = int(double(comps[(i*4)+0]) * yscale);
        right = int(double(comps[(i*4)+1]) * xscale);
        bottom = int(double(comps[(i*4)+2]) * yscale);
        left = int(double(comps[(i*4)+3]) * xscale);

        unsigned int addr = (unsigned int)comps;
        unsigned int paddr = (unsigned int)&comps;

        int k = 0, l = 0;
        k += addr;
        l += paddr;

        // Since we aren't rendering the first and last rows/columns, adjust bounding
        // box accordingly
        if (top < 0) top = 0;
        else if (top > (height-3)) top = (height-3);
        if (bottom < 0) bottom = 0;
        else if (bottom > (height-3)) bottom = (height-3);
        if (right < 0) right = 0;
        else if (right > (width-3)) right = (width-3);
        if (left < 0) left = 0;
        else if (left > (width-3)) left = (width-3);

        p.drawRect(QRect(left, top, right-left, bottom-top));

    }
    p.end();
}

void Renderer::handleRL(QImage *image, uint color, int row, int startCol, int len)
{
    unsigned int *line;
    int col;

    line = (unsigned int *)image->scanLine(row);
    for (col=startCol; col<startCol+len; col++)
        line[col] = color;
}

int Renderer::renderCCQ1(uint16_t width, uint16_t height, uint32_t numVals, uint32_t *qVals)
{
    int32_t row;
    uint32_t i, startCol, length;
    uint8_t model;
    QImage img(width, height, QImage::Format_ARGB32);
    unsigned int palette[] = {0x00000000, 0x80ff0000, 0x8000ff00, 0x800000ff, 0x80ffff00, 0x8000ffff, 0x80ff00ff};

    if (qVals[0]!=0xffffffff)
        qDebug() << "error!";
    qVals++; // skip beginning of frame marker
    numVals--;
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
        res = renderBA81(*(uint16_t *)args[0], *(uint16_t *)args[1], *(uint32_t *)args[2], (uint8_t *)args[3]);
    else if (type==FOURCC('V', 'I', 'S', 'U'))    // contains visualization data
        res = renderVISU(*(uint32_t *)args[0], (int16_t *)args[1]);
    else if (type==FOURCC('C','C','Q','1'))
        res = renderCCQ1(*(uint16_t *)args[0], *(uint16_t *)args[1], *(uint32_t *)args[2], (uint32_t *)args[3]);
    else // format not recognized
        return -1;

    emitFlushImage();
    return res;
}

