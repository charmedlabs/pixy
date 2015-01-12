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

#include "processblobs.h"
#include "interpreter.h"

ProcessBlobs::ProcessBlobs(Interpreter *interpreter)
{
    m_interpreter = interpreter;
    m_qq = new Qqueue();
#ifdef DEFER
    m_blobs = new Blobs(m_qq);
#endif
    m_qMem = new uint32_t[0x10000];

    connect(m_interpreter, SIGNAL(paramChange()), this, SLOT(handleParamChange()));
}

ProcessBlobs::~ProcessBlobs()
{
#ifdef DEFER
    delete m_blobs;
#endif
    delete m_qq;
    delete [] m_qMem;
}

void ProcessBlobs::process(const Frame8 &frame, uint32_t *numBlobs, BlobA **blobs, uint32_t *numCCBlobs, BlobB **ccBlobs, uint32_t *numQvals, Qval **qMem)
{
#if 0
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

    rls(frame);
    m_blobs->blobify();
    m_blobs->getBlobs(blobs, numBlobs, ccBlobs, numCCBlobs);
    *numQvals = m_numQvals;
    *qMem = m_qMem;

    //processCoded();
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
#endif
}

void ProcessBlobs::rls(const Frame8 &frame)
{
#if 0
    uint32_t x, y, count, index, startCol, model, lutVal, r, g1, g2, b;
    int32_t c1, c2;
    uint32_t prevModel=0;

    for (y=1, m_numQvals=0; y<(uint32_t)frame.m_height; y+=2)
    {
        // new lime
        m_qq->enqueue(0);
        m_qMem[m_numQvals++] = 0;

        count = 0;
        prevModel = 0;
        startCol = 0;
        for (x=1; x<(uint32_t)frame.m_width; x+=2)
        {
            r = frame.m_pixels[y*frame.m_width + x];
            g1 = frame.m_pixels[y*frame.m_width + x - 1];
            g2 = frame.m_pixels[y*frame.m_width - frame.m_width + x];
            b = frame.m_pixels[y*frame.m_width - frame.m_width + x - 1];
            c2 = r-g1;
            c1 = b-g2;
            c1 >>= 1;
            c2 >>= 1;
            index = ((uint8_t)c2<<8) | (uint8_t)c1;
            lutVal = m_blobs->m_lut[index];

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
                m_qq->enqueue(model);
                m_qMem[m_numQvals++] = model;
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
            m_qq->enqueue(model);
            m_qMem[m_numQvals++] = model;
            model = 0;
        }

    }
    // indicate end of frame
    m_qq->enqueue(0xffffffff);
    m_qMem[m_numQvals++] = 0xffffffff;
#endif
}

void ProcessBlobs::handleParamChange()
{
    const QVariant *variant;

#ifdef DEFER
    // read from parameter database
    if ((variant=m_interpreter->m_pixyParameters.value("Max blocks")))
        m_maxBlobs = variant->toUInt();
    if ((variant=m_interpreter->m_pixyParameters.value("Max blocks per signature")))
        m_maxBlobsPerModel = variant->toUInt();
    if ((variant=m_interpreter->m_pixyParameters.value("Min block area")))
        m_minArea = variant->toUInt();
    if ((variant=m_interpreter->m_pixyParameters.value("Color code mode")))
        m_ccMode = variant->toUInt();
#endif

#ifdef DEFER
    // update
    m_blobs->setParams(m_maxBlobs, m_maxBlobsPerModel, m_minArea, (ColorCodeMode)m_ccMode);
#endif
}

