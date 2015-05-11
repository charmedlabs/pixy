#ifndef DEBUGTESTDIALOG_H
#define DEBUGTESTDIALOG_H

#include <QMainWindow>

namespace Ui {
class DebugTestDialog;
}

class Renderer;

class DebugTestDialog : public QMainWindow
{
	Q_OBJECT

public:
	explicit DebugTestDialog(QWidget *parent = 0);
	~DebugTestDialog();

private slots:
	void on_pushButton_clicked();
	void update();

	void on_horizontalSlider_sliderMoved(int position);

	void on_spinBox_2_valueChanged(int arg1);

private:
	Ui::DebugTestDialog *ui;

	std::vector<Renderer*> renderer;
};

#endif // DEBUGTESTDIALOG_H
