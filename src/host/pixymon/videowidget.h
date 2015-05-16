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

#ifndef VIDEOWIDGET_H
#define VIDEOWIDGET_H

#include <QWidget>
#include <QImage>

#define VW_ASPECT_RATIO   ((float)1280/(float)800)

class UsbCam;
class MainWindow;

class VideoWidget : public QWidget
{
    Q_OBJECT

public:
	VideoWidget(QWidget *main);
    ~VideoWidget();

    void handleImage(void **args);
    void clear();

    int activeWidth();
    int activeHeight();

    enum InputMode
    {
        NONE, POINT, REGION
    };

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
    void handleImage(QImage image, uchar renderFlags);
    void acceptInput(VideoWidget::InputMode mode); // need the VideoWidget qualifier, otherwise it won't recognize the metatype!

private slots:

private:
	QWidget *m_main;

    std::vector<QImage> m_images;
    std::vector<QImage> m_renderedImages;

	int m_width = 0;
	int m_height = 0;
	int m_xOffset = 0;
	int m_yOffset = 0;
	int m_x0 = 0;
	int m_y0 = 0;
	int m_sbWidth = 0;
	int m_sbHeight = 0;
	float m_scale = 1;
	bool m_drag = false;
    InputMode m_inputMode;
	bool m_selection = false;
};


#endif // VIDEOWIDGET_H
