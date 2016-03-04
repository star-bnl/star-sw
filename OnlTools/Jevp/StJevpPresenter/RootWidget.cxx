#include "RootWidget.h"

RootWidget::RootWidget(char *name, QWidget *parent) : QWidget(parent)
{
    QVBoxLayout *layout = new QVBoxLayout(this);
    displayWidget = new QWidget(this);
    displayWidget->setAttribute(Qt::WA_PaintOnScreen, true);
    displayWidget->setAttribute(Qt::WA_OpaquePaintEvent, true);

    layout->addWidget(displayWidget);
    setLayout(layout);
    canvas = NULL;
    this->name = new std::string(name);
}

TCanvas *RootWidget::getCanvas() {
    if(!canvas) {
	wid = gVirtualX->AddWindow(displayWidget->winId(), width(), height());
	canvas = new TCanvas(name->c_str(), width(), height(), wid);
    }    
    return canvas;
}

void RootWidget::resizeEvent(QResizeEvent *) {
    if(canvas) {
	canvas->Resize();
	canvas->Update();
    }
}

void RootWidget::paintEvent(QPaintEvent *) {
    if(canvas) {
	canvas->Resize();
	canvas->Update();
    }
}	
