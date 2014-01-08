#include <QDebug>
#include <math.h>
#include "blobs.h"

QString code2string(uint16_t code)
{
    QString scode;
    int i;
    uint8_t c;

    for (i=4; i>=0; i--)
    {
        c = (code >> (i*3)) & 0x07;
        if (c)
            scode += c + '0';
    }
    return scode;
}

uint16_t string2code(const QString &code)
{
    int i;
    uint16_t icode;

    for (i=0, icode=0; i<code.length(); i++)
    {
        icode <<= 3;
        icode |= (code[i].toAscii()-'0')&0x07;
    }
    return icode;
}

Blobs::Blobs()
{
    int i;
    //m_qmem = new SSegment[QMEM_SIZE];
    m_qmem = new uint32_t[QMEM_SIZE];
    m_lut = new uint8_t[LUT_SIZE];
    m_clut = new ColorLUT(m_lut);

    m_boxes = new uint16_t[MAX_BLOBS*5];
    for (i=0; i<LUT_SIZE; i++)
        m_lut[i] = 0;

    m_minArea = MIN_AREA;
    m_mergeDist = MAX_MERGE_DIST;
}


Blobs::~Blobs()
{
    delete [] m_qmem;
    delete [] m_lut;
    delete [] m_boxes;
}

void Blobs::process(uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame, uint16_t *numBlobs, uint16_t **blobs, uint32_t *numQVals, uint32_t **qVals)
{
#if 0
    uint16_t boxes[] = {
        1, 20, 30, 10, 20,
        1, 35, 45, 25, 35,
        1, 20, 30, 10, 20,
        1, 21, 22, 11, 12,
        1, 25, 35, 20, 50,
        1, 25, 35, 20, 50,
        1, 25, 35, 20, 50,
        1, 80, 90, 40, 50,
        1, 80, 90, 60, 61
    };

    combine(boxes, 9);
    combine2(boxes, 9);
    compress(boxes, 9);
    return;
#endif
    rls(width, height, frameLen, frame);
    blobify();
    *blobs = m_boxes;
    if (numQVals)
        *numQVals = m_qindex;
    if (qVals)
        *qVals = m_qmem;

    //processCoded();
    *numBlobs = m_numBoxes; // + m_numCodedBoxes;
#if 0 // uncomment if we only want to show structured blobs
    if (m_numCodedBoxes>0)
    {
        int i;
        for (i=0; i<m_numBoxes; i++)
        {
            m_boxes[i*4+0]=0;
            m_boxes[i*4+1]=0;
            m_boxes[i*4+2]=0;
            m_boxes[i*4+3]=0;
        }
    }
#endif
}

void Blobs::rls(uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame)
{
    uint32_t x, y, count, index, startCol, model, lutVal, r, g1, g2, b;
    int32_t c1, c2;
    bool stateIn, stateOut;
    uint32_t prevLutVal=0, prevModel=0;
    m_qindex = 0;

    for (y=1; y<height; y+=2)
    {
        m_qmem[m_qindex++] = 0;
        stateIn = stateOut = false;
        count = 0;
        prevModel = 0;
        startCol = 0;
        for (x=1; x<width; x+=2)
        {
            r = frame[y*width + x];
            g1 = frame[y*width + x - 1];
            g2 = frame[y*width - width + x];
            b = frame[y*width - width + x - 1];
            c2 = r-g1;
            c1 = b-g2;
            c1 >>= 1;
            c2 >>= 1;
            index = ((uint8_t)c2<<8) | (uint8_t)c1;
            lutVal = m_lut[index];

#if 0
            if (lutVal)
            {
                stateIn = true;
                if (count==0)
                    model = lutVal&0x07;
                if ((lutVal&0x07)!=model)
                    stateIn = false;
            }
            else
                stateIn = false;

            if (stateIn!=stateOut)
            {
                count++;
                if (count>=2)
                {
                    stateOut = stateIn;
                    if (stateOut)
                        startCol = (x-2)/2;
                    else
                    {
#if 0
                        m_qmem[m_qindex].model = model;
                        m_qmem[m_qindex].startCol = startCol;
                        m_qmem[m_qindex].endCol = (x-3)/2;
                        m_qindex++;
#else
                        model |= startCol<<3;
                        model |= ((x-2)/2-startCol)<<12;
                        m_qmem[m_qindex++] = model;
#endif
                    }
                }
            }
            else
                count = 0;
#else
            model = lutVal&0x07;

            if (model && prevModel==0)
            {
                startCol = x/2;
            }
            if ((model && prevModel && model!=prevModel) ||
                    (model==0 && prevModel))
            {
                model = prevModel;
                model |= startCol<<3;
                model |= (x/2-startCol)<<12;
                m_qmem[m_qindex++] = model;
                model = 0;
                startCol = 0;
            }
            prevModel = model;
#endif
        }
        if (startCol)
        {
            model = prevModel;
            model |= startCol<<3;
            model |= (x/2-startCol)<<12;
            m_qmem[m_qindex++] = model;
            model = 0;
        }

    }
}

void Blobs::blobify()
{
    SSegment s;
    int32_t row;
    uint32_t qval, i, j;
    CBlob *blob;
    uint16_t *boxesStart;
    uint16_t numBoxesStart, invalid, invalid2;
    uint16_t left, top, right, bottom;


    // q val:
    // | 4 bits    | 7 bits      | 9 bits | 9 bits    | 3 bits |
    // | shift val | shifted sum | length | begin col | model  |

    // start frame
    for (i=0; i<NUM_MODELS; i++)
        m_assembler[i].Reset();

    for (i=0, row=-1; i<m_qindex; i++)
    {
        qval = m_qmem[i];
        if (qval==0)
        {
            row++;
            continue;
        }
        s.model = qval&0x07;
        if (s.model>0)
        {
            s.row = row;
            qval >>= 3;
            s.startCol = qval&0x1ff;
            qval >>= 9;
            s.endCol = (qval&0x1ff) + s.startCol;
            if (m_assembler[s.model-1].Add(s)<0)
                break;
        }
    }


    for (i=0, m_numBoxes=0; i<NUM_MODELS; i++)
    {
        m_assembler[i].EndFrame();
        m_assembler[i].SortFinished();

        for (j=m_numBoxes*5, boxesStart=m_boxes+j, numBoxesStart=m_numBoxes, blob=m_assembler[i].finishedBlobs;
             blob; blob=blob->next)
        {
            if (blob->GetArea()<(int)m_minArea)
                continue;
            blob->getBBox((short &)left, (short &)top, (short &)right, (short &)bottom);
            // shift by 4 because we need to double the parameters (we're processing at 1/4 resolution)
            m_boxes[j + 0] = i+1;
            m_boxes[j + 1] = left<<1;
            m_boxes[j + 2] = right<<1;
            m_boxes[j + 3] = top<<1;
            m_boxes[j + 4] = bottom<<1;
            m_numBoxes++;
            j += 5;

#if 0
            SMomentStats stats;
            blob->moments.GetStats(stats);
            qDebug() << "xcentroid " << stats.centroidX << "ycentroid " << stats.centroidY;
#endif
        }
        invalid = combine(boxesStart, m_numBoxes-numBoxesStart);
        if (1)
        {
            while ((invalid2=combine2(boxesStart, m_numBoxes-numBoxesStart)))
                invalid += invalid2;
        }
        if (invalid)
        {
            invalid2 = compress(boxesStart, m_numBoxes-numBoxesStart);
            m_numBoxes -= invalid2;
        }
    }
}

uint16_t Blobs::compress(uint16_t *boxes, uint16_t numBoxes)
{
    uint16_t i, ii;
    uint16_t *destination, invalid;

    // compress list
    for (i=0, ii=0, destination=NULL, invalid=0; i<numBoxes; i++, ii+=5)
    {
        if (boxes[ii+0]==0)
        {
            if (destination==NULL)
                destination = boxes+ii;
            invalid++;
            continue;
        }
        if (destination)
        {
            destination[0] = boxes[ii+0];
            destination[1] = boxes[ii+1];
            destination[2] = boxes[ii+2];
            destination[3] = boxes[ii+3];
            destination[4] = boxes[ii+4];
            destination += 5;
        }
    }
    return invalid;
}

uint16_t Blobs::combine(uint16_t *boxes, uint16_t numBoxes)
{
    uint16_t i, j, ii, jj, left0, right0, top0, bottom0;
    uint16_t left, right, top, bottom;
    uint16_t invalid;

    // delete blobs that are fully enclosed by larger blobs
    for (i=0, ii=0, invalid=0; i<numBoxes; i++, ii+=5)
    {
        if (boxes[ii+0]==0)
            continue;
        left0 = boxes[ii+1];
        right0 = boxes[ii+2];
        top0 = boxes[ii+3];
        bottom0 = boxes[ii+4];

        for (j=i+1, jj=ii+5; j<numBoxes; j++, jj+=5)
        {
            if (boxes[jj+0]==0)
                continue;
            left = boxes[jj+1];
            right = boxes[jj+2];
            top = boxes[jj+3];
            bottom = boxes[jj+4];

            if (left0<=left && right0>=right &&
                    top0<=top && bottom0>=bottom)
            {
                boxes[jj+0] = 0; // invalidate
                invalid++;
            }
        }
    }

    return invalid;
}

uint16_t Blobs::combine2(uint16_t *boxes, uint16_t numBoxes)
{
    uint16_t i, j, ii, jj, left0, right0, top0, bottom0;
    uint16_t left, right, top, bottom, height, width;
    uint16_t invalid;

    // delete blobs that are fully enclosed by larger blobs
    for (i=0, ii=0, invalid=0; i<numBoxes; i++, ii+=5)
    {
        if (boxes[ii+0]==0)
            continue;
        left0 = boxes[ii+1];
        right0 = boxes[ii+2];
        top0 = boxes[ii+3];
        bottom0 = boxes[ii+4];

        for (j=i+1, jj=ii+5; j<numBoxes; j++, jj+=5)
        {
            if (boxes[jj+0]==0)
                continue;
            left = boxes[jj+1];
            right = boxes[jj+2];
            top = boxes[jj+3];
            bottom = boxes[jj+4];

            width = (right-left)/2;
            height = (bottom-top)/2;

#if 1 // if corners touch....
            if (left<=left0 && left0-right<=m_mergeDist &&
                    ((top0<=top && top<=bottom0) || (top0<=bottom && bottom<=bottom0)))
            {
                boxes[ii+1] = left;
                boxes[jj+0] = 0; // invalidate
                invalid++;
            }
            else if (right>=right0 && left-right0<=m_mergeDist &&
                    ((top0<=top && top<=bottom0) || (top0<=bottom && bottom<=bottom0)))
            {
                boxes[ii+2] = right;
                boxes[jj+0] = 0; // invalidate
                invalid++;
            }
            else if (top<=top0 && top0-bottom<=m_mergeDist &&
                    ((left0<=left && left<=right0) || (left0<=right && right<=right0)))
            {
                boxes[ii+3] = top;
                boxes[jj+0] = 0; // invalidate
                invalid++;
            }
            else if (bottom>=bottom0 && top-bottom0<=m_mergeDist &&
                    ((left0<=left && left<=right0) || (left0<=right && right<=right0)))
            {
                boxes[ii+4] = bottom;
                boxes[jj+0] = 0; // invalidate
                invalid++;
            }
#else // at least half of a side (the smaller adjacent side) has to overlap
            if (left<=left0 && left0-right<=m_mergeDist &&
                    ((top<=top0 && top0<=top+height) || (top+height<=bottom0 && bottom0<=bottom)))
            {
                boxes[ii+1] = left;
                boxes[jj+0] = 0; // invalidate
                invalid++;
            }
            else if (right>=right0 && left-right0<=m_mergeDist &&
                     ((top<=top0 && top0<=top+height) || (top+height<=bottom0 && bottom0<=bottom)))
            {
                boxes[ii+2] = right;
                boxes[jj+0] = 0; // invalidate
                invalid++;
            }
            else if (top<=top0 && top0-bottom<=m_mergeDist &&
                     ((left<=left0 && left0<=left+width) || (left+width<=right0 && right0<=right)))
            {
                boxes[ii+3] = top;
                boxes[jj+0] = 0; // invalidate
                invalid++;
            }
            else if (bottom>=bottom0 && top-bottom0<=m_mergeDist &&
                     ((left<=left0 && left0<=left+width) || (left+width<=right0 && right0<=right)))
            {
                boxes[ii+4] = bottom;
                boxes[jj+0] = 0; // invalidate
                invalid++;
            }
#endif
        }
    }

    return invalid;
}

#define MAX_CODED_DIST  10

bool Blobs::closeby(int a, int b, int dist)
{

    int16_t left0, right0, top0, bottom0;
    int16_t left, right, top, bottom;
    uint8_t model0, model;

    left0 = m_boxes[a*4+0];
    if (left0==0)
        return false;
    model0 = left0&0x07;
    left0 >>= 3;
    right0 = m_boxes[a*4+1]>>3;
    top0 = m_boxes[a*4+2]>>3;
    bottom0 = m_boxes[a*4+3]>>3;

    left = m_boxes[b*4+0];
    if (left==0)
        return false;
    model = left&0x07;
    if (model0==model)
        return false;
    left >>= 3;
    right = m_boxes[b*4+1]>>3;
    top = m_boxes[b*4+2]>>3;
    bottom = m_boxes[b*4+3]>>3;

    if (left0>=left && left0-right<=dist &&
            ((top0<=top && top<=bottom0) || (top0<=bottom && (bottom<=bottom0 || top<=top0))))
        return true;

    if (left>=left0 && left-right0<=dist &&
            ((top0<=top && top<=bottom0) || (top0<=bottom && (bottom<=bottom0 || top<=top0))))
        return true;

    if (top0>=top && top0-bottom<=dist &&
            ((left0<=left && left<=right0) || (left0<=right && (right<=right0 || left<=left0))))
        return true;

    if (top>=top0 && top-bottom0<=dist &&
            ((left0<=left && left<=right0) || (left0<=right && (right<=right0 || left<=left0))))
        return true;

    return false;
}

void Blobs::addCoded(int a, int b)
{
    //static int i=0;
    uint16_t left, right, top, bottom;
    uint8_t model;
    uint16_t codedModel;

    //qDebug() << "***close " << i++;
    QString code;

    model = m_boxes[a*4]&0x07;
    codedModel = model;
    codedModel <<= 3;
    model = m_boxes[b*4]&0x07;
    codedModel |= model;
    codedModel <<= 3; // 0 escape

    left = m_boxes[a*4+0]>>3 < m_boxes[b*4+0]>>3 ? m_boxes[a*4+0]>>3 : m_boxes[b*4+0]>>3;
    right = m_boxes[a*4+1]>>3 > m_boxes[b*4+1]>>3 ? m_boxes[a*4+1]>>3 : m_boxes[b*4+1]>>3;
    top = m_boxes[a*4+2]>>3 < m_boxes[b*4+2]>>3 ? m_boxes[a*4+2]>>3 : m_boxes[b*4+2]>>3;
    bottom = m_boxes[a*4+3]>>3 > m_boxes[b*4+3]>>3 ? m_boxes[a*4+3]>>3 : m_boxes[b*4+3]>>3;

#ifdef RENDER_ANGLE
    // calculate angle
    int acx = ((m_boxes[a*4+1]>>3)+(m_boxes[a*4+0]>>3))/2;
    int acy = ((m_boxes[a*4+3]>>3)+(m_boxes[a*4+2]>>3))/2;
    int bcx = ((m_boxes[b*4+1]>>3)+(m_boxes[b*4+0]>>3))/2;
    int bcy = ((m_boxes[b*4+3]>>3)+(m_boxes[b*4+2]>>3))/2;
    double angle = atan2(acy-bcy, acx-bcx)*180/3.1415;
    //qDebug() << "angle " << angle << " acx " << acx << " acy " << acy << " bcx " << bcx << " bcy " << bcy;

    // add rectangle
    m_boxes[m_numBoxes*4+m_numCodedBoxes*6+0] = codedModel;
    m_boxes[m_numBoxes*4+m_numCodedBoxes*6+1] = left;
    m_boxes[m_numBoxes*4+m_numCodedBoxes*6+2] = right;
    m_boxes[m_numBoxes*4+m_numCodedBoxes*6+3] = top;
    m_boxes[m_numBoxes*4+m_numCodedBoxes*6+4] = bottom;
    m_boxes[m_numBoxes*4+m_numCodedBoxes*6+5] = (int16_t)angle;
#else
    // add rectangle
    m_boxes[m_numBoxes*4+m_numCodedBoxes*5+0] = codedModel;
    m_boxes[m_numBoxes*4+m_numCodedBoxes*5+1] = left;
    m_boxes[m_numBoxes*4+m_numCodedBoxes*5+2] = right;
    m_boxes[m_numBoxes*4+m_numCodedBoxes*5+3] = top;
    m_boxes[m_numBoxes*4+m_numCodedBoxes*5+4] = bottom;
#endif

    m_numCodedBoxes++;

    // invalidate a and b (only if you are
    m_boxes[a*4+0] = 0;
    m_boxes[a*4+1] = 0;
    m_boxes[a*4+2] = 0;
    m_boxes[a*4+3] = 0;
    m_boxes[b*4+0] = 0;
    m_boxes[b*4+1] = 0;
    m_boxes[b*4+2] = 0;
    m_boxes[b*4+3] = 0;
}

void Blobs::processCoded()
{
    int i, j;

    m_numCodedBoxes = 0;
    for (i=0; i<m_numBoxes; i++)
    {
        for (j=i+1; j<m_numBoxes; j++)
        {
            if (closeby(i, j, MAX_CODED_DIST))
                addCoded(i, j);
        }
    }
}

#define WIDTH 320
#define HEIGHT 200

int Blobs::generateLUT(uint8_t model, uint16_t x0, uint16_t y0, uint16_t width, uint16_t height, uint8_t *frame)
{
    ColorModel cmodel;
    if (model>NUM_MODELS)
        return -1;

    m_clut->generate(&cmodel, frame, x0, y0, width, height, WIDTH);
    m_clut->add(&cmodel, model);

    return 0;
}

int Blobs::generateLUT(uint8_t model, const Frame8 &frame, const Point16 &seed, RectA *region)
{
    RectA cregion;
    ColorModel cmodel;

    m_clut->growRegion(&cregion, frame, seed);

    m_clut->generate(&cmodel, frame.m_pixels,
                     cregion.m_xOffset, cregion.m_yOffset, cregion.m_width, cregion.m_height, frame.m_width);
    m_clut->add(&cmodel, model);

    if (region)
        *region = cregion;

    return 0;
}

int Blobs::setLabel(uint32_t model, const QString &label)
{
    unsigned int i;

    for (i=0; i<m_labels.size(); i++)
    {
        if (m_labels[i].first==model)
        {
            m_labels[i].second = label;
            return 0;
        }
    }
    m_labels.push_back(LabelPair(model, label));
    return 0;
}

int Blobs::setLabel(const QString &model, const QString &label)
{
    uint16_t imodel;

    imodel = string2code(model);

    return setLabel(imodel, label);
}

QString *Blobs::getLabel(uint32_t model)
{
    unsigned int i;
    for (i=0; i<m_labels.size(); i++)
    {
        if (m_labels[i].first==model)
            return &m_labels[i].second;
    }
    return NULL;
}


