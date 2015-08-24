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

#ifndef CONFIGDIALOG_H
#define CONFIGDIALOG_H

#include <QDialog>
#include <QLineEdit>
#include <QLabel>
#include <QTableWidget>
#include "chirpmon.h"
#include "parameters.h"
#include "ui_configdialog.h"

#define SLIDER_SIZE   125.0f

class Interpreter;

class ConfigDialog : public QDialog
{
    Q_OBJECT

public:
    ConfigDialog(QWidget *parent, Interpreter *interpreter);
    ~ConfigDialog();

signals:

public slots:
    void load();

protected slots:
    void apply(QAbstractButton *button);
    void handleChangeClicked();
    void handleCheckBox();
    void handleSlider(int position);
    void handleComboBox(int index);
    virtual void accept();
    virtual void reject();

protected:

private:
    int updateDB();
    int updateDB(ParameterDB *data);

    void render(ParameterDB *data, QTabWidget *tabs);

    QWidget *findCategory(const QString &category, QTabWidget *tabs);

    Ui::ConfigDialog *m_ui;
    QTabWidget *m_pixyTabs;
    QTabWidget *m_pixymonTabs;
    Interpreter *m_interpreter;
};

#endif // CONFIGDIALOG_H
