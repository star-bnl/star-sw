#include "jmlQtGui.h"
#include "ZoomWidget.h"
#include "TLine.h"
#include "TText.h"
#include "EditDialog.h"
#include <q3popupmenu.h>
#include "RootWidget.h"




ZPlotDisplay::~ZPlotDisplay() {
  printf("In plot display destructor");
}

ZPlotDisplay::ZPlotDisplay(ZoomWidget *parent, JevpPlot *plot) : QFrame() {

    this->plot = plot;
    psizex=1150;
    psizey=750;

    this->parent = parent;

    QVBoxLayout *layout = new QVBoxLayout;

    QScrollArea *sa = new QScrollArea;

    myrootwidget = new RootWidget("zoomer", this);
    myrootwidget->setMinimumSize(psizex,psizey);
    // myrootwidget->setEnabled(0);

    sa->setWidget(myrootwidget);
    sa->setMinimumHeight(800);
    sa->setMinimumWidth(1200);

    layout->addWidget(sa);
    setLayout(layout);

    myrootwidget->getCanvas()->cd();
    if(plot) {
	LOG("DBG", "Draw");
	plot->draw();
    }
    else {
	LOG("DBG", "No plot?");
    }
}

void ZPlotDisplay::gotclick()
{
  //emit selected(this);

  LOG("DBG", "ZPlotDIsplay: gotclick()");
}

void ZPlotDisplay::invalidate()  // ie... we changed the plot...
{
  myrootwidget->getCanvas()->Clear();
  myrootwidget->getCanvas()->cd();

  if(plot) {
    LOG("DBG", "invalidate...");
    plot->draw();
  }
  else {
    LOG("DBG", "No plot?");
  }
  myrootwidget->getCanvas()->Update();
}

void ZPlotDisplay::mousePressEvent(QMouseEvent *e)
{
  if(e->button() & Qt::RightButton) {
    LOG("DBG", "Right button:");
  }

  if(e->button() & Qt::LeftButton) {
    LOG("DBG", "Left button:");
  }
}
 


void ZPlotDisplay::dragEnterEvent(QDragEnterEvent *event)
{
  LOG("DBG", "dragEnter");
}

void ZPlotDisplay::dropEvent(QDropEvent *event)
{
  LOG("DBG", "Dropt");
}

void ZPlotDisplay::zoomin()
{ 
  psizex *= 2;
  psizey *= 2;
  setnewsize();
}

void ZPlotDisplay::zoomout()
{  
  psizex /= 2;
  psizey /= 2;
  setnewsize();
}

void ZPlotDisplay::resetsize()
{
  psizex = 1150;
  psizey = 750;
  setnewsize();
}

void ZPlotDisplay::setnewsize()
{
  // myrootwidget->setMinimumSize(psizex,psizey);
  // myrootwidget->setMaximumSize(psizex,psizey);
  myrootwidget->resize(psizex,psizey);
  invalidate();
}




ZoomWidget::ZoomWidget(JevpGui *logic, char *name) : QDialog()
{
  JevpPlot *plot;
 
  this->logic = logic;
  this->histoname = name;

  QVBoxLayout *layout = new QVBoxLayout();
  char error[256];

  // The Plot!
  plot = logic->jl_getPlotFromServer(name,error);
  zplot = new ZPlotDisplay(this, plot);
  layout->addWidget(zplot);

  // The OK button!
  QDialogButtonBox *buttonBox = new QDialogButtonBox(QDialogButtonBox::Ok);
  buttonBox->addButton("Expand", QDialogButtonBox::ActionRole);
  buttonBox->addButton("Original Size", QDialogButtonBox::ActionRole);
  
  connect(buttonBox, SIGNAL(clicked(QAbstractButton *)), this, SLOT(buttonPressed(QAbstractButton *)));
  connect(buttonBox, SIGNAL(accepted()), this, SLOT(accept()));
  layout->addWidget(buttonBox);

  setLayout(layout);
  setWindowTitle(tr(name));
}

void ZoomWidget::buttonPressed(QAbstractButton *button)
{
  LOG("DBG", "ButtonPressed: %s",(const char *)button->text());

  
  if(strcmp((const char *)button->text(), "Expand") == 0) {
    zplot->zoomin();
  }
  if(strcmp((const char *)button->text(), "Zoom Out") == 0) {
    zplot->zoomout();
  }
  if(strcmp((const char *)button->text(), "Original Size") == 0) {
    zplot->resetsize();
  }
}

void ZoomWidget::mousePressEvent(QMouseEvent *e)
{
  printf("ZoomWidget::::   got a mouse press...\n");
}

void ZoomWidget::dragEnterEvent(QDragEnterEvent *event)
{
  printf("Ref: drag enter event...\n");
}
