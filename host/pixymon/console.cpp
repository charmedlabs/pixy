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
    connect(&m_timer, SIGNAL(timeout()), this, SLOT(handleTimer()));
}

ConsoleWidget::~ConsoleWidget()
{
    m_mutexPrint.lock();
    m_waitPrint.wakeAll();
    m_mutexPrint.unlock();
}

void ConsoleWidget::handleTimer()
{
    if (!isReadOnly())
        prompt();
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
    m_mutexPrint.lock();
    if (text==m_lastLine)
    {
        if (!m_suppress)
        {
            handleColor(color);
            insertPlainText("...\n");
            m_suppress = true;
        }
    }
    else
    {
        QTextCursor cursor = textCursor();
        if (cursor.block().text()==m_prompt)
        {
            int i;
            for (i=0; i<m_prompt.size(); i++)
                cursor.deletePreviousChar();
        }
        handleColor(color);
        insertPlainText(text);

        m_suppress = false;
    }
    m_lastLine = text;
    m_waitPrint.wakeAll();
    m_mutexPrint.unlock();
    m_timer.start(CW_TIMEOUT);
}

void ConsoleWidget::error(QString text)
{
    emptyLine();
    print("error: " + text, Qt::red);
}

void ConsoleWidget::emptyLine()
{
    QTextCursor cursor = textCursor();
    QString line = cursor.block().text();
    if (line!="" && line!=m_prompt)
        insertPlainText("\n");
}

void ConsoleWidget::prompt(QString text)
{
    // add space because it looks better
    text += " ";
    m_prompt = text;

    //prompt();
}

void ConsoleWidget::prompt()
{
    moveCursor(QTextCursor::End);
    QTextCursor cursor = textCursor();
    if (cursor.block().text()!=m_prompt)
    {
        m_timer.stop();
        if (cursor.block().text()!="")
            insertPlainText("\n");
        handleColor(); // change to default color
        insertPlainText(m_prompt);
    }
    m_lastLine = "";
    // if we have trouble keeping viewport
    QScrollBar *sb = verticalScrollBar();
    sb->setSliderPosition(sb->maximum());
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

    m_timer.stop();
    if (!isReadOnly())
    {
        moveCursor(QTextCursor::End);

        if (event->key()==Qt::Key_Return)
        {
            QTextCursor cursor = textCursor();
            line = cursor.block().text();

            line.remove(0, m_prompt.size()); // get rid of prompt
            // propagate newline before we send text
            QPlainTextEdit::keyPressEvent(event);
            // send text
            m_timer.start();
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



