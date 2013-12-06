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

    for (i=0; i<LUT_SIZE; i++)
        m_lut[i] = 0;
}


Blobs::~Blobs()
{
    delete [] m_qmem;
}

void Blobs::process(uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame, uint16_t *numBlobs, uint16_t **blobs, uint32_t *numQVals, uint32_t **qVals)
{
    rls(width, height, frameLen, frame);
    blobify();
    //clean();
    //while(clean2());
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
            m_assembler[s.model-1].Add(s);
        }
    }


    for (i=0, m_numBoxes=0; i<NUM_MODELS; i++)
    {
        m_assembler[i].EndFrame();
        m_assembler[i].SortFinished();

        for (j=m_numBoxes*5, blob=m_assembler[i].finishedBlobs; blob; blob=blob->next)
        {
            if (blob->GetArea()<MIN_AREA)
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
    }
    qDebug() << "blobs " << m_numBoxes;

}

void Blobs::compress()
{
    uint16_t i, j;
    uint16_t left0;

    // compress list
    for (i=0; i<m_numBoxes; i++)
    {
        left0 = m_boxes[i*4+0];

        if (left0==0)
        {
            for (j=i+1; j<m_numBoxes; j++)
            {
                left0 = m_boxes[i*4+0];
                if (left0!=0)
                { // copy j to i
                    m_boxes[i*4+0] = m_boxes[j*4+0];
                    m_boxes[i*4+1] = m_boxes[j*4+1];
                    m_boxes[i*4+2] = m_boxes[j*4+2];
                    m_boxes[i*4+3] = m_boxes[j*4+3];
                    m_boxes[j*4+0] = 0; // invalidate
                    break;
                }
            }
            if (j==m_numBoxes)
            {
                m_numBoxes = i;
                break;
            }
        }
    }

}

void Blobs::clean()
{
    uint16_t i, j, left0, right0, top0, bottom0;
    uint16_t left, right, top, bottom;
    uint8_t model0, model;

    // delete blobs that are fully enclosed by larger blobs
    for (i=0; i<m_numBoxes; i++)
    {
        left0 = m_boxes[i*4+0];
        if (left0==0)
            continue;
        model0 = left0&0x07;
        left0 >>= 3;
        right0 = m_boxes[i*4+1]>>3;
        top0 = m_boxes[i*4+2]>>3;
        bottom0 = m_boxes[i*4+3]>>3;


        for (j=i+1; j<m_numBoxes; j++)
        {
            left = m_boxes[j*4+0];
            model = left&0x07;
            if (model0!=model)
                break;

            left >>= 3;
            right = m_boxes[j*4+1]>>3;
            top = m_boxes[j*4+2]>>3;
            bottom = m_boxes[j*4+3]>>3;

            if (left0<=left && right0>=right &&
                    top0<=top && bottom0>=bottom)
            {
                m_boxes[j*4+0] = 0; // invalidate
                m_boxes[j*4+1] = 0; // invalidate
                m_boxes[j*4+2] = 0; // invalidate
                m_boxes[j*4+3] = 0; // invalidate
            }
        }
    }

    //compress();
}

int Blobs::clean2()
{
    int16_t i, j, left0, right0, top0, bottom0;
    int16_t left, right, top, bottom;
    uint8_t model0, model;
    int n = 0;

    for (i=0; i<m_numBoxes; i++)
    {
        left0 = m_boxes[i*4+0];
        if (left0==0)
            continue;
        model0 = left0&0x07;
        left0 >>= 3;
        right0 = m_boxes[i*4+1]>>3;
        top0 = m_boxes[i*4+2]>>3;
        bottom0 = m_boxes[i*4+3]>>3;


        for (j=i+1; j<m_numBoxes; j++)
        {
            left = m_boxes[j*4+0];
            model = left&0x07;
            if (model0!=model)
                break;
            left >>= 3;
            right = m_boxes[j*4+1]>>3;
            top = m_boxes[j*4+2]>>3;
            bottom = m_boxes[j*4+3]>>3;

            if (left<=left0 && left0-right<=MAX_MERGE_DIST &&
                    ((top0<=top && top<=bottom0) || (top0<=bottom && bottom<=bottom0)))
            {
                m_boxes[i*4+0] = (left<<3) | model;
                m_boxes[j*4+0] = 0; // invalidate
                m_boxes[j*4+1] = 0; // invalidate
                m_boxes[j*4+2] = 0; // invalidate
                m_boxes[j*4+3] = 0; // invalidate
                n++;
            }
            if (right>=right0 && left-right0<=MAX_MERGE_DIST &&
                    ((top0<=top && top<=bottom0) || (top0<=bottom && bottom<=bottom0)))
            {
                m_boxes[i*4+1] = (right<<3) | model;
                m_boxes[j*4+0] = 0; // invalidate
                m_boxes[j*4+1] = 0; // invalidate
                m_boxes[j*4+2] = 0; // invalidate
                m_boxes[j*4+3] = 0; // invalidate
                n++;
            }
            if (top<=top0 && top0-bottom<=MAX_MERGE_DIST &&
                    ((left0<=left && left<=right0) || (left0<=right && right<=right0)))
            {
                m_boxes[i*4+2] = (top<<3) | model;
                m_boxes[j*4+0] = 0; // invalidate
                m_boxes[j*4+1] = 0; // invalidate
                m_boxes[j*4+2] = 0; // invalidate
                m_boxes[j*4+3] = 0; // invalidate
                n++;
            }
            if (bottom>=bottom0 && top-bottom0<=MAX_MERGE_DIST &&
                    ((left0<=left && left<=right0) || (left0<=right && right<=right0)))
            {
                m_boxes[i*4+3] = (bottom<<3) | model;
                m_boxes[j*4+0] = 0; // invalidate
                m_boxes[j*4+1] = 0; // invalidate
                m_boxes[j*4+2] = 0; // invalidate
                m_boxes[j*4+3] = 0; // invalidate
                n++;
            }
        }
    }

//    if (n)
//        compress();

    return n;
}

#define MAX_CODED_DIST  10

bool Blobs::closeby(int a, int b, int dist)
{

    int16_t left0, right0, top0, bottom0;
    int16_t left, right, top, bottom;
    uint8_t model0, model;
    int n = 0;

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
    unsigned int i;
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


