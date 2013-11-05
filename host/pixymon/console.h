#ifndef CONSOLE_H
#define CONSOLE_H

#include <QPlainTextEdit>
#include <QMutex>
#include <QWaitCondition>

#define CW_SCROLLHEIGHT     10000

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
    void print(const QString &text);
    void error(const QString &text);
    void prompt(const QString &text);
    void type(const QString &text);
    void acceptInput(bool accept);

signals:
    void textLine(const QString &line);
    void controlKey(Qt::Key key);

protected:
    //virtual void mouseReleaseEvent(QMouseEvent *event);
    virtual void keyPressEvent(QKeyEvent *event);

private:
    MainWindow *m_main;
    QString m_prompt;

};

#endif // CONSOLE_H
