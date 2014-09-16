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
    m_suppress = false;
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
    emptyLine();
    moveCursor(QTextCursor::End);
    handleColor(color);
    m_mutexPrint.lock();
    if (text==m_lastLine)
    {
        if (!m_suppress)
        {
            insertPlainText("...\n");
            m_suppress = true;
        }
    }
    else
    {
        insertPlainText(text);
        m_suppress = false;
    }
    m_lastLine = text;
    m_waitPrint.wakeAll();
    m_mutexPrint.unlock();
}

void ConsoleWidget::error(QString text)
{
    emptyLine();
    print("error: " + text, Qt::red);
}

void ConsoleWidget::emptyLine()
{
    QTextCursor cursor = textCursor();
    if (cursor.block().text()!="")
        insertPlainText("\n");
}

void ConsoleWidget::prompt(QString text)
{
    QString text2 = text;

    moveCursor(QTextCursor::End);

    handleColor(); // change to default color

    // add space because it looks better
    text2 += " ";

    // go to new line if line isn't empty
    emptyLine();
    insertPlainText(text2);
    m_lastLine = "";
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

    if (!isReadOnly())
    {
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
        else
        {
            // make sure when we're typing, there's a prompt
            QTextCursor cursor = textCursor();

            line = cursor.block().text();
            if (line.left(m_prompt.size())!=m_prompt)
                prompt(m_prompt);
        }
    }

    QPlainTextEdit::keyPressEvent(event);
}

#if 0
void ConsoleWidget::mouseReleaseEvent(QMouseEvent *event)
{
    moveCursor(QTextCursor::End);

    QPlainTextEdit::mousePressEvent(event);
}
#endif



