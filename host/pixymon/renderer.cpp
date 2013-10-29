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

    connect(this, SIGNAL(image(QImage, bool)), m_video, SLOT(handleImage(QImage, bool)));
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

void Renderer::setFilter(int16_t hmin, int16_t hmed, int16_t hmax, uint8_t smin, uint8_t smax, uint8_t vmin, uint8_t vmax, uint8_t cmin, uint8_t cmax)
{
    m_hmin = hmin;
    m_hmed = hmed;
    m_hmax = hmax;
    m_smin = smin;
    m_smax = smax;
    m_vmin = vmin;
    m_vmax = vmax;
    m_cmin = cmin;
    m_cmax = cmax;
}
#define SATMAX8(v, a)   v > 0xff - a ? 0xff : v + a
#define SATMIN8(v, a)   v < a ? 0 : v - a

int Renderer::renderBA81Filter(uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame)
{
    uint16_t x, y;
    uint32_t *line;
    uint32_t r, g1, g2, b;
    uint8_t h, s, v, c;
    bool stateIn, stateOut;
    int count;
    int hbias;
    int n;
    uint color;
    int f0=0, f1=0, f2=0, f3=0, f4=0, f5=0, f6=0, f7=0;

    memcpy(m_frameData, frame, width*height);

    QImage img(width/2, height/2, QImage::Format_RGB32);

    hbias = 0;
    n = 0;
    for (y=1; y<height; y+=2)
    {
        stateIn = stateOut = false;
        count = 0;

        line = (unsigned int *)img.scanLine(y/2);
        for (x=1; x<width; x+=2)
        {
            r = m_frameData[y*width + x];
            g1 = m_frameData[y*width + x - 1];
            g2 = m_frameData[y*width - width + x];
            b = m_frameData[y*width - width + x - 1];

            stateIn = true;
#if 0
            r >>= 3; g >>= 3; b >>= 3;
            unsigned int index = (b<<10) | (g<<5) | r;
            r <<= 3; g <<= 3; b <<= 3;

            if (m_lut && m_lut[index])
            {
                color = 0xffff0000;
                stateIn = false;
            }
#endif

#if 1
            int32_t c1, c2;
            c2 = r-g1;
            c1 = b-g2;
            c1 >>= 1;
            c2 >>= 1;

            // c1 >>= 1;
            unsigned int index = ((uint8_t)c2<<8) | (uint8_t)c1;

#if 0
            volatile int q = 0;
            if (g>r)
                q = 1;
            if (g>b)
                q = 1;
#endif
            if (m_lut && m_lut[index])
            {
                color = 0xffff0000;
                stateIn = false;
            }
#endif

#if 0
            r >>= 3; r <<= 3; g >>= 3; g <<= 3; b >>= 3; b <<= 3;
            hsvc(r, g, b, &h, &s, &v, &c);

            if ((m_hmin>=0 && h<m_hmin) || (m_hmin<0 && (int8_t)h<m_hmin))
            {
                f0++;
                color = 0xffff0000;
                stateIn = false;
            }
            else if (h>m_hmax)
            {
                f1++;
                color = 0xff00ff00;
                stateIn = false;
            }
            else if (s<m_smin)
            {
                f2++;
                color = 0xff0000ff;
                stateIn = false;
            }
            else if (s>m_smax)
            {
                f3++;
                color = 0xffffff00;
                stateIn = false;
            }
            else if (v<m_vmin)
            {
                f4++;
                color = 0x00000000;
                stateIn = false;
            }
            else if (v>m_vmax)
            {
                f5++;
                color = 0xff808080;
                stateIn = false;
            }
            else if (c<m_cmin)
            {
                f6++;
                color = 0xff00ffff;
                stateIn = false;
            }
            else if (c>m_cmax)
            {
                f7++;
                color = 0xffff00ff;
                stateIn = false;
            }

            if (stateIn)
            {
                n++;
                if (h>m_hmed)
                    hbias++;
                else if (h<m_hmed)
                    hbias--;
            }
#endif

#if 0
            if (stateIn!=stateOut)
            {
                count++;
                if (count>2)
                {
                    stateOut = stateIn;
                    count = 0;
                }
            }
            else
                count = 0;
#else
            stateOut = stateIn;
#endif
            if (stateOut)
                *line++ = (0xff<<24) | (r<<16) | (g1<<8) | (b<<0); //0xffffffff;
            else
                *line++ = color;

#if 0
           *line++ = (0xff<<24) | (s<<16) | (s<<8) | (s<<0);
#endif
#if 0
           *line++ = (0xff<<24) | (h<<16) | (h<<8) | (h<<0);
#endif
        }
    }
#if 0
    float fhbias = (float)hbias/n;

    if (n>200 && n<16000)
    {
        if (fhbias>.20)
        {
            m_hmax = SATMAX8(m_hmax, 1);
            m_hmed = SATMAX8(m_hmed, 1);
            m_hmin = SATMAX8(m_hmin, 1);
        }
        else if (fhbias<-.20)
        {
            m_hmax = SATMIN8(m_hmax, 1);
            m_hmed = SATMIN8(m_hmed, 1);
            m_hmin = SATMIN8(m_hmin, 1);
        }
    }
#endif
    qDebug() << "hbias: " << (float)hbias/n << "\t" << n << "\t" << m_hmed << "\t" << m_hmin << "\t" << m_hmax;
    // send image to ourselves across threads
    // from chirp thread to gui thread
    emit image(img, false);
    return 0;
}

void average(uint16_t width, uint16_t height, uint8_t *frame)
{
    int i, j, n=0;
    uint r, g, b, max, min, ravg=0, gavg=0, bavg=0;
    float y, ymax, x, xmax, fh, fs, fravg, fgavg, fbavg;

    int16_t h;
    uint8_t s, v;

    for (i=101; i<111; i+=2)
    {
        for (j=161; j<171; j+=2)
        {
            r = frame[i*width + j];
            g = frame[i*width - width + j];
            b = frame[i*width - width + j - 1];
            ravg += r;
            gavg += g;
            bavg += b;
            n++;
            frame[i*width + j] = 0;
            frame[i*width - width + j] = 0;
            frame[i*width - width + j - 1] = 0;
        }
    }
    ravg /= n;
    gavg /= n;
    bavg /= n;
    ravg >>= 3;
    ravg <<= 3;
    gavg >>= 3;
    gavg <<= 3;
    bavg >>= 3;
    bavg <<= 3;

    fravg = (float)ravg/0x100;
    fgavg = (float)gavg/0x100;
    fbavg = (float)bavg/0x100;
#if 1
    y = fbavg-fgavg;
    x = fravg-fgavg;
    fh = atan2(y, x);
    if (fh<0)
        fh += 2*M_PI;
    h = (int16_t)(fh*360/(2*M_PI));

    n = h/45;
    if (n==0)
    {
        xmax = 1;
        ymax = tan(fh);
    }
    if (n==1)
    {
        xmax = tan(M_PI/2-fh);
        ymax = 1;
    }
    if (n==2)
    {
        xmax = tan(fh-M_PI/2);
        ymax = 1;
    }
    if (n==3)
    {
        xmax = 1;
        ymax = tan(M_PI-fh);
    }
    if (n==4)
    {
        xmax = 1;
        ymax = tan(fh-M_PI);
    }
    if (n==5)
    {
        xmax = tan(3*M_PI/2-fh);
        ymax = 1;
    }
    if (n==6)
    {
        xmax = tan(fh-3*M_PI/2);
        ymax = 1;
    }
    if (n==7)
    {
        xmax = 1;
        ymax = tan(2*M_PI-fh);
    }

    fs = sqrt(y*y+x*x)/sqrt(ymax*ymax+xmax*xmax);
    s = (uint8_t)(fs*0x100);
    max = MAX(MAX(ravg, gavg), bavg);
    min = MIN(MIN(ravg, gavg), bavg);
    v = (ravg + gavg + bavg)/3;
#endif
    uint8_t h2, s2, v2, c;
    RGBtoHSV(ravg, gavg, bavg, &h2, &s2, &v2, &c);
    qDebug() << "h:" << h << "\t" << h2 << "\ts: " << s << "\t" << c << "\t" << s2 << "\tv: " << v << "\t" << v2;
}

int Renderer::renderBA81(uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame)
{
    uint16_t x, y;
    uint32_t *line;
    uint32_t r, g, b;
    uint8_t *frame0;


    frame0 = frame;
    //average(width, height, frame);
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
    emit image(img, false);

    if (m_mode&0x01)
    {
        uint16_t numBlobs;
        uint16_t *blobs;
        uint32_t numQVals;
        uint32_t *qVals;

        m_blobs.process(width, height, frameLen, frame0, &numBlobs, &blobs, &numQVals, &qVals);
        if (m_mode&0x04)
            renderCCQ1(width/2, height/2, numQVals, qVals);
        if (m_mode&0x02)
            renderCCB1(width, height, numBlobs, blobs);
    }
    return 0;
}

int Renderer::renderCCB1(uint16_t width, uint16_t height, uint16_t numBlobs, uint16_t *blobs)
{
    uint16_t i, left, right, top, bottom;
    QImage img(width, height, QImage::Format_ARGB32);
    QPainter p;
    uint8_t model;
    QString str;
    QString *label;

    //qDebug() << "numblobs " << numBlobs;
    img.fill(0x00000000);
    p.begin(&img);
    p.setBrush(QBrush(QColor(0xff, 0xff, 0xff, 0x20)));
    p.setPen(QPen(QColor(0xff, 0xff, 0xff, 0xff)));
    QFont font("verdana", 9);
    font.setStyleStrategy(QFont::NoAntialias);
    p.setFont(font);
    for (i=0; i<numBlobs; i++)
    {
        if (blobs[i*4+0]==0)
            continue;

        model = blobs[i*4+0]&0x07;
        if (model==0)
            break;
        left = blobs[i*4+0]>>3;
        right = blobs[i*4+1]>>3;
        top = blobs[i*4+2]>>3;
        bottom = blobs[i*4+3]>>3;
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
    //deal with coded bobls
    blobs += i*4;
    numBlobs -= i;
    for (i=0; i<numBlobs; i++)
    {
#ifdef RENDER_ANGLE
        int16_t angle;
        model = blobs[i*6+0]>>3;
        left = blobs[i*6+1];
        right = blobs[i*6+2];
        top = blobs[i*6+3];
        bottom = blobs[i*6+4];
        angle = blobs[i*6+5];
#else
        model = blobs[i*5+0]>>3;
        left = blobs[i*5+1];
        right = blobs[i*5+2];
        top = blobs[i*5+3];
        bottom = blobs[i*5+4];
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
    p.end();

    if (m_mode&0x02)
        emit image(img, true);
    else
        emit image(img, false);

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

    //m_video->callMeMaybe(VISUcallback);

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
    unsigned int palette[] = {0x00000000, 0x80ff0000, 0x8000ff00, 0x800000ff, 0x80ffff00, 0x8000ffff, 0x20ff00ff};

    qDebug() << numVals;

    // q val:
    // | 4 bits    | 7 bits      | 9 bits | 9 bits    | 3 bits |
    // | shift val | shifted sum | length | begin col | model  |

    img.fill(0x00000000);
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
    if (m_mode&0x04)
        emit image(img, true);
    else
        emit image(img, false);

    return 0;
}

int Renderer::render(uint32_t type, void *args[])
{
    // choose fourcc for representing formats fourcc.org
    if (type==FOURCC('B','A','8','1'))
        return renderBA81(*(uint16_t *)args[0], *(uint16_t *)args[1], *(uint32_t *)args[2], (uint8_t *)args[3]);
    else if (type==FOURCC('V', 'I', 'S', 'U'))    // contains visualization data
        return renderVISU(*(uint32_t *)args[0], (int16_t *)args[1]);
    else if (type==FOURCC('C','C','Q','1'))
        return renderCCQ1(*(uint16_t *)args[0], *(uint16_t *)args[1], *(uint32_t *)args[2], (uint32_t *)args[3]);
    // format not recognized
    return -1;
}

