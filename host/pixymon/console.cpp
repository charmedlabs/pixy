#include <QDebug>
#include <QKeyEvent>
#include <QTextCursor>
#include <QTextBlock>
#include <QScrollBar>
#include "console.h"


ConsoleWidget::ConsoleWidget(MainWindow *main) : QPlainTextEdit((QWidget *)main)
{
    m_main = main;
    m_color = CW_DEFAULT_COLOR;
    // a block is a line, so this is the maximum number of lines to buffer
    setMaximumBlockCount(CW_SCROLLHEIGHT);
    acceptInput(false);
}

ConsoleWidget::~ConsoleWidget()
{
    m_mutexPrint.lock();
    m_waitPrint.wakeAll();
    m_mutexPrint.unlock();
}

void ConsoleWidget::handleColor(const QColor &color)
{
    if (color!=m_color)
    {
        QTextCharFormat tf = currentCharFormat();
        tf.setForeground(color);
        setCurrentCharFormat(tf);
        m_color = color;
    }
}

void ConsoleWidget::print(QString text, QColor color)
{
    moveCursor(QTextCursor::End);
    handleColor(color);
    m_mutexPrint.lock();
    insertPlainText(text);
    m_waitPrint.wakeAll();
    m_mutexPrint.unlock();
}

void ConsoleWidget::error(QString text)
{
    print("error: " + text, Qt::red);
}

void ConsoleWidget::prompt(QString text)
{
    QString text2 = text;

    QTextCursor cursor = textCursor();
    moveCursor(QTextCursor::End);

    handleColor(); // change to default color

    // add space because it looks better
    text2 += " ";

    // go to new line if line isn't empty
    if (cursor.block().text()!="")
        insertPlainText("\n");
    insertPlainText(text2);

    // if we have trouble keeping viewport
    QScrollBar *sb = verticalScrollBar();
    sb->setSliderPosition(sb->maximum());

    m_prompt = text2;
}

void ConsoleWidget::type(QString text)
{
}

void ConsoleWidget::acceptInput(bool accept)
{
    setReadOnly(!accept);
}


void ConsoleWidget::keyPressEvent(QKeyEvent *event)
{
    QString line;

    moveCursor(QTextCursor::End);

    if (event->key()==Qt::Key_Return)
    {
        QTextCursor cursor = textCursor();
        line = cursor.block().text();

        line.remove(0, m_prompt.size()); // get rid of prompt (assume it's just the first character)
        // propagate newline before we send text
        QPlainTextEdit::keyPressEvent(event);
        // send text
        emit textLine(line);
        return;

    }
    else if (event->key()==Qt::Key_Up)
    {
        emit controlKey(Qt::Key_Up);
        return;
    }
    else if (event->key()==Qt::Key_Down)
    {
        emit controlKey(Qt::Key_Down);
        return;
    }
    else if (event->key()==Qt::Key_Backspace)
    {
        QTextCursor cursor = textCursor();

        line = cursor.block().text();
        // don't propagate backspace if it means we're going to delete the prompt
        if (line.size()<=m_prompt.size())
            return;
    }
    else if (event->matches(QKeySequence::Copy)) // break key
        emit controlKey(Qt::Key_Escape);

    QPlainTextEdit::keyPressEvent(event);
}

#if 0
void ConsoleWidget::mouseReleaseEvent(QMouseEvent *event)
{
    moveCursor(QTextCursor::End);

    QPlainTextEdit::mousePressEvent(event);
}
#endif



