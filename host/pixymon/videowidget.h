#ifndef VIDEOWIDGET_H
#define VIDEOWIDGET_H

#include <QWidget>
#include <QMutex>

#define VW_ASPECT_RATIO   ((float)1280/(float)800)

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

    int activeWidth();
    int activeHeight();

protected:
    void paintEvent(QPaintEvent *event);
    virtual int heightForWidth(int w) const;
    virtual void mouseMoveEvent(QMouseEvent *event);
    virtual void mousePressEvent(QMouseEvent *event);
    virtual void mouseReleaseEvent(QMouseEvent *event);
    virtual void resizeEvent(QResizeEvent *event);


signals:
    void selection(int x0, int y0, int width, int height);

public slots:
    void handleImage(QImage image);
    void handleFlush();
    void acceptInput(uint type);

private slots:

private:
    QImage *m_background;

    QPixmap *m_pm;
    MainWindow *m_main;

    QImage g_foreground;
    QPixmap g_fpm;

    QMutex m_mutex;

    int m_width;
    int m_height;
    int m_xOffset;
    int m_yOffset;
    int m_x0;
    int m_y0;
    int m_sbWidth;
    int m_sbHeight;
    float m_scale;
    bool m_drag;
    bool m_selection;
    bool m_backgroundFrame;
};

#endif // VIDEOWIDGET_H
