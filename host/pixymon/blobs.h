#ifndef BLOBS_H
#include <QString>
#include <stdint.h>
#include <vector>
#include <utility>
#include "blob.h"

#define NUM_MODELS      7
#define MAX_BLOBS       256
#define MAX_MERGE_DIST  5
#define MIN_AREA        1

#define QMEM_SIZE       0x4000
#define LUT_SIZE        0x10000
class Renderer;

typedef std::pair<uint32_t, QString> LabelPair;

QString code2string(uint16_t code);
uint16_t string2code(const QString &code);

#define RENDER_ANGLE

class Blobs
{
public:
    Blobs();
    ~Blobs();

    void process(uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame,  uint16_t *numBlobs, uint16_t **blobs, uint32_t *numQVals=NULL, uint32_t **qVals=NULL);
    uint8_t *getLut()
    {
        return m_lut;
    }
    int setLabel(uint32_t model, const QString &label);
    int setLabel(const QString &model, const QString &label);
    QString *getLabel(uint32_t model);

    friend class Renderer;
private:
    void rls(uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame);
    void blobify();
    void compress();
    void clean();
    int clean2();

    bool closeby(int a, int b, int dist);
    void addCoded(int a, int b);
    void processCoded();


    CBlobAssembler m_assembler[NUM_MODELS];
    //SSegment *m_qmem;
    uint32_t *m_qmem;
    uint8_t *m_lut;
    uint32_t m_qindex;
    uint16_t m_boxes[5*MAX_BLOBS];
    uint16_t m_numBoxes;
    uint16_t m_numCodedBoxes;
    std::vector<LabelPair> m_labels;
};

#define BLOBS_H

#endif // BLOBS_H
