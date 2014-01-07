#ifndef RENDERER_H
#define RENDERER_H
#include <QObject>
#include <QImage>
#include "pixytypes.h"
#include "blobs.h"


class VideoWidget;

class Renderer : public QObject
{
    Q_OBJECT

public:
    Renderer(VideoWidget *video);
    ~Renderer();

    int render(uint32_t type, void *args[]);
    Frame8 m_rawFrame;

    int renderRect(uint16_t width, uint16_t height, const RectA &rect);

    int renderBA81(uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame);
    int renderCCB1(uint16_t width, uint16_t height, uint16_t numBlobs, uint16_t *blobs);
    void emitFlushImage();
    // experimental
    void setFilter(int16_t hmin, int16_t hmed, int16_t hmax, uint8_t smin, uint8_t smax, uint8_t vmin, uint8_t vmax, uint8_t cmin, uint8_t cmax);
    void setMode(uint32_t mode)
    {
        m_mode = mode;
    }

    Blobs m_blobs;

signals:
    void image(QImage image);
    void flushImage();

private:
    inline void interpolateBayer(unsigned int width, unsigned int x, unsigned int y, unsigned char *pixel, unsigned int &r, unsigned int &g, unsigned int &b);

    int renderCCQ1(uint16_t width, uint16_t height, uint32_t numVals, uint32_t *qVals);
    int renderVISU(uint32_t cc_num, int16_t* c_components);

    void emitImage(const QImage &image);

    int renderBA81Filter(uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame);

    void handleRL(QImage *image, uint color, int row, int startCol, int len);

    VideoWidget *m_video;
    bool m_backgroundFrame; // our own copy because we're in a different thread (not gui thread)

    // experimental
    int16_t m_hmin;
    int16_t m_hmed;
    int16_t m_hmax;
    uint8_t m_smin;
    uint8_t m_smax;
    uint8_t m_vmin;
    uint8_t m_vmax;
    uint8_t m_cmin;
    uint8_t m_cmax;

    uint8_t *m_lut;
    uint32_t m_mode;
};

#endif // RENDERER_H

