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
#include <QTimer>

#define CW_SCROLLHEIGHT     10000
#define CW_DEFAULT_COLOR    Qt::black
#define CW_TIMEOUT          100
#define CW_MAXHIST          100

class MainWindow;

class ConsoleWidget : public QPlainTextEdit
{
    Q_OBJECT

public:
    ConsoleWidget(MainWindow *main);
    ~ConsoleWidget();

    QMutex m_mutexPrint;
    QWaitCondition m_waitPrint;

public slots:
    void print(QString text, QColor color=CW_DEFAULT_COLOR);
    void command(QString text);
    void error(QString text);
    void prompt(QString text);
    void prompt();
    void type(QString text);
    void acceptInput(bool accept);

private slots:
    void handleTimer();

signals:
    void textLine(const QString &line);
    void controlKey(Qt::Key key);

protected:
    //virtual void mouseReleaseEvent(QMouseEvent *event);
    virtual void keyPressEvent(QKeyEvent *event);

private:
    void handleColor(const QColor &color=CW_DEFAULT_COLOR);
    void handleHistory(bool down);

    MainWindow *m_main;
    QString m_prompt;
    QString m_lastLine;
    bool m_suppress;
    QTimer m_timer;
    QStringList m_history;
    int m_histIndex;
};

#endif // CONSOLE_H
