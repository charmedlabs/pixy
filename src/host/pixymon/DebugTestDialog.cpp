#include "DebugTestDialog.h"
#include "ui_DebugTestDialog.h"

#include <QVBoxLayout>

#include <robomodule.h>
#include <renderer.h>

DebugTestDialog::DebugTestDialog(QWidget *parent) :
	QMainWindow(parent),
	ui(new Ui::DebugTestDialog)
{
	ui->setupUi(this);

	RoboParameters params;
	ui->horizontalSlider->setValue(params.limit);
	ui->spinBox_2->setValue(params.xLimit);
}

DebugTestDialog::~DebugTestDialog()
{
	delete ui;
}

void DebugTestDialog::on_pushButton_clicked()
{
	update();
}

void DebugTestDialog::update()
{
	RoboParameters params;
	params.limit = ui->horizontalSlider->value();
	params.xLimit = ui->spinBox_2->value();
	static uint32_t num = 0;
	num += 1;
	num %= 34;
	if (num == 11 || num == 27 || num == 30 || num == 0)
	{
		num += 1;
	}
	auto frame = RawFrame<uint8_t>::readFromFile(num);
	ui->spinBox->setValue(params.limit);

	size_t counter = 0;
	auto createRenderer = [&]()->Renderer*
	{
		Renderer* pRenderer;
		if (renderer.size() <= counter)
		{
			VideoWidget* widget = new VideoWidget(this);
			ui->horizontalLayout->addWidget(widget);

			pRenderer = new Renderer(widget, nullptr);
			renderer.push_back(pRenderer);
		}
		else
		{
			pRenderer = renderer[counter];
		}
		++counter;
		return pRenderer;
	};

	RoboModule::debugRender(createRenderer, createRenderer(), frame, params);
}

void DebugTestDialog::on_horizontalSlider_sliderMoved(int position)
{
	update();
}

void DebugTestDialog::on_spinBox_2_valueChanged(int arg1)
{
	update();
}
