#ifndef RENDERER_H
#define RENDERER_H
#include <QObject>
#include <QImage>
#include "pixytypes.h"
#include "blobs.h"

class Interpreter;

class VideoWidget;

class Renderer : public QObject
{
    Q_OBJECT

public:
    Renderer(VideoWidget *video);
    ~Renderer();

    int render(uint32_t type, void *args[]);
    int renderBackground();
    int renderRect(uint16_t width, uint16_t height, const RectA &rect);
    void emitFlushImage();

    void setMode(uint32_t mode)
    {
        m_mode = mode;
    }

    Frame8 m_rawFrame;
    Blobs m_blobs;


signals:
    void image(QImage image);
    void flushImage();

private:
    inline void interpolateBayer(unsigned int width, unsigned int x, unsigned int y, unsigned char *pixel, unsigned int &r, unsigned int &g, unsigned int &b);

    int renderCCQ1(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t numVals, uint32_t *qVals);
    int renderBA81(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame);
    int renderCCB1(uint8_t renderFlags, uint16_t width, uint16_t height, uint32_t numBlobs, uint16_t *blobs);

    void emitImage(const QImage &image);

    int renderBA81Filter(uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame);

    void handleRL(QImage *image, uint color, int row, int startCol, int len);

    VideoWidget *m_video;
    bool m_backgroundFrame; // our own copy because we're in a different thread (not gui thread)
    QImage m_background;

    uint32_t m_mode;
};

#endif // RENDERER_H

