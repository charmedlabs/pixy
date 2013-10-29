#ifndef VIDEOWIDGET_H
#define VIDEOWIDGET_H

#include <QWidget>

#define VW_ASPECT_RATIO   ((float)1280/(float)800)

// Define the callback used for inside of paint event
typedef void (*paintCallback)(QImage* image);

class UsbCam;
class MainWindow;

class VideoWidget : public QWidget
{
    Q_OBJECT

public:
    VideoWidget(MainWindow *main);
    ~VideoWidget();

    void handleImage(void **args);
    void clear();

protected:
    void paintEvent(QPaintEvent *event);
    virtual int heightForWidth(int w) const;
    virtual void mouseMoveEvent(QMouseEvent *event);
    virtual void mousePressEvent(QMouseEvent *event);
    virtual void mouseReleaseEvent(QMouseEvent *event);
    virtual void resizeEvent(QResizeEvent *event);

    void callPaintCallbacks(QImage* image);

signals:
    void selection(int x0, int y0, int width, int height);

public slots:
    void handleImage(QImage image, bool blend);
    void acceptInput(uint type);

private slots:

private:
    void blend(QImage *foreground);

    QImage *m_background;

    QPixmap *m_pm;
    MainWindow *m_main;

    int m_xOffset;
    int m_yOffset;
    int m_x0;
    int m_y0;
    int m_sbWidth;
    int m_sbHeight;
    float m_scale;
    bool m_drag;
    bool m_selection;

    std::list<paintCallback> paintCallbacks;
};

#endif // VIDEOWIDGET_H
