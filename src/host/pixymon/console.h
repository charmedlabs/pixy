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

#ifndef CONSOLE_H
#define CONSOLE_H

#include <QPlainTextEdit>
#include <QMutex>
#include <QWaitCondition>

#define CW_SCROLLHEIGHT     10000
#define CW_DEFAULT_COLOR    Qt::black

class MainWindow;

class ConsoleWidget : public QPlainTextEdit
{
    Q_OBJECT

public:
    ConsoleWidget(MainWindow *main);
    ~ConsoleWidget();

    void emptyLine();

    QMutex m_mutexPrint;
    QWaitCondition m_waitPrint;

public slots:
    void print(QString text, QColor color=CW_DEFAULT_COLOR);
    void error(QString text);
    void prompt(QString text);
    void type(QString text);
    void acceptInput(bool accept);

signals:
    void textLine(const QString &line);
    void controlKey(Qt::Key key);

protected:
    //virtual void mouseReleaseEvent(QMouseEvent *event);
    virtual void keyPressEvent(QKeyEvent *event);

private:
    void handleColor(const QColor &color=CW_DEFAULT_COLOR);

    MainWindow *m_main;
    QString m_prompt;
    QColor m_color;
    QString m_lastLine;
    bool m_suppress;
};

#endif // CONSOLE_H
