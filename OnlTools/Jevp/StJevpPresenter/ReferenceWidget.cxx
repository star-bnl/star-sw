#include "jmlQtGui.h"
#include "ReferenceWidget.h"
#include "TLine.h"
#include "TText.h"
#include "EditDialog.h"
#include <q3popupmenu.h>

#define PD_LARGE_CUR 1
#define PD_LARGE_REF 2
#define PD_THUMB 3

PlotDisplay::~PlotDisplay() {
  //  printf("In plot display destructor %d 0x%x\n",refid,plot);
  
  if(plot && (refid != 1))
    delete plot;
  //printf("leaving it...\n");
}

void PlotDisplay::CrossOfDeath(TCanvas *screen) {
  TLine* a = new TLine(0.,0.,1.,1.);
  TLine* b = new TLine(0.,1.,1.,0.);
  TText* t = new TText(0.5,0.5,"No Plot");

  // This is how we free the memory...
  a->SetBit(kCanDelete);
  b->SetBit(kCanDelete);
  t->SetBit(kCanDelete);
  //screen->addPlot(a);
  //screen->addPlot(b);
  //screen->addPlot(t);
  
  a->SetLineColor(2);
  b->SetLineColor(2);
  t->SetTextColor(3);
  t->SetTextAlign(22);

  // Already cd()'d to proper pad...
  a->Draw();
  b->Draw();
  t->Draw();

  return;
}

// Null plot results in cross of death...
PlotDisplay::PlotDisplay(ReferenceWidget *refwidget, JevpPlot *plot, int refid, int type) : QFrame() {

  prev = NULL;
  next = NULL;
  if(refid != 0) 
    setAcceptDrops(true);

  this->refwidget = refwidget;

  this->plot = plot;
  this->refid = refid;

  switch(type) {
  case PD_LARGE_CUR:
  case PD_LARGE_REF:
    {
      char boxtext[100];

      if(plot) {
	if(type == PD_LARGE_CUR) {
	  sprintf(boxtext, "Current run: #%d",plot->run);
	}
	else {
	  sprintf(boxtext, "Reference run: #%d",plot->run);
	}
      }
      else {
	sprintf(boxtext, "Unknown run");
      }

      QVBoxLayout *layout = new QVBoxLayout;
      myrootwidget = new RootWidget("referencewidget", this);
  
      char *ctext = (char *)"No description available";
      if(plot && plot->refcomment) 
	ctext = plot->refcomment;

      QScrollArea *sa = new QScrollArea;
      comment = new QLabel(ctext);
     
      sa->setWidget(comment);
      // twidget->setReadOnly(1);
      sa->setMinimumHeight(40);
      sa->setMaximumHeight(40);
      layout->addWidget(myrootwidget);
      layout->addWidget(sa);

      myrootwidget->GetCanvas()->cd();
      if(plot) {
	LOG("JEFF", "Draw %s",ctext);
	plot->draw();
      }
      else {
	CrossOfDeath(myrootwidget->GetCanvas());
      }
      myrootwidget->setMinimumSize(400,400);
      myrootwidget->setEnabled(0);
      setLayout(layout);

      // connect(twidget, SIGNAL(clicked(int, int)), this, SLOT(gotclick()));

      //setMinimumSize(400,400);
    }
    break;
  case PD_THUMB:
    {
      comment = NULL;
      myrootwidget = new RootWidget("thumb", this);
      QVBoxLayout *layout = new QVBoxLayout;
      layout->addWidget(myrootwidget);
      setLayout(layout);

      myrootwidget->GetCanvas()->cd();
      myrootwidget->setEnabled(0);
      if(plot) {
	LOG("JEFF", "Draw thumb");
	plot->draw();
      }
      else {
	CrossOfDeath(myrootwidget->GetCanvas());
      }
      
      setMinimumSize(100,100);
      setMaximumSize(100,100);
      if(plot) {
	setToolTip(plot->refcomment ? plot->refcomment : "No description available");
      }
    }
    break;
  default:
    printf("No valid description for PlotDisplay=%d\n",type);
  }  

  // Add a popup menu...
  pop = new Q3PopupMenu(this);
  pop->insertItem("Edit Comments",NULL,1,1);
  pop->insertItem("Delete",NULL,2,2);
  
}

void PlotDisplay::gotclick()
{
  emit selected(this);
}

void PlotDisplay::invalidate()  // ie... we changed the plot...
{
  myrootwidget->GetCanvas()->Clear();
  myrootwidget->GetCanvas()->cd();
  if(plot) {
    LOG("JEFF", "invalidate...");
    plot->draw();
  }
  else CrossOfDeath(myrootwidget->GetCanvas());

  myrootwidget->GetCanvas()->Update();
}

void PlotDisplay::mousePressEvent(QMouseEvent *e)
{
  if(e->button() & Qt::RightButton) {

    int ret = pop->exec(QCursor::pos());
    if(ret == 1) {   // edit
      QString *str = EditDialog::run();
      if(str) {
	plot->setRefComment(str->toAscii().data());
	replaceData(plot);

	refwidget->logic->jl_saveExistingPlot(plot);
      }
    }
    if(ret == 2) {
      refwidget->logic->jl_deletePlot(plot);
      refwidget->removePlot(this);
    }
  }

  if(e->button() & Qt::LeftButton) {
    QPixmap newpix(this->size());
    //    newpix.grabWidget(this);
    this->render(&newpix);

    setAcceptDrops(false);
    QDrag *drag = new QDrag(this);
    QMimeData *mimeData = new QMimeData;
    
    char text[10];
    if(this->plot)
      sprintf(text, "%d", this->plot->refid);
    else 
      sprintf(text, "-1");

    mimeData->setText(text);

    drag->setMimeData(mimeData);
    drag->setPixmap(newpix);

    drag->exec();     // exec is like a blocking call until the drop is accomplished...

    if(refid != 0)
      setAcceptDrops(true);

    e->accept();
    
  }
    
  emit selected(this);
}
 
void PlotDisplay::replaceData(JevpPlot *plot) 
{
  printf("Replacing data for %d with %d\n",refid,plot ? plot->refid : -1);

  this->plot = plot;
  char *ctext = (char *)"No comment available";
  if(plot && plot->refcomment) ctext = plot->refcomment;

  printf("Comment\n");

  if(comment) {
    comment->setText(ctext);
    comment->adjustSize();
  }

  printf("Tool tip\n");
  setToolTip(ctext);

  printf("cd...\n");
  // myrootwidget->GetCanvas()->Clear();
  myrootwidget->GetCanvas()->cd();
  if(plot) {
    printf("Draw\n");
    LOG("JEFF", "Draw ctext=%s",ctext);
    plot->draw();
  }
  else {
    printf("Else\n");
    myrootwidget->GetCanvas()->Clear();
    CrossOfDeath(myrootwidget->GetCanvas());
  }
  
  printf("Done\n");

  if(plot) plot->refid = true_refid();

  printf("Update: \n");

  myrootwidget->GetCanvas()->Update(); 
  printf("...9ing\n");
  update();
  printf("Updated\n");
}

void PlotDisplay::dragEnterEvent(QDragEnterEvent *event)
{
  event->acceptProposedAction();
}

void PlotDisplay::dropEvent(QDropEvent *event)
{
  if(!event->mimeData()) {
    printf("No mimedata?\n");
  }

  PlotDisplay *src = (PlotDisplay *)event->source();
  
  if(src->refid == 0) {     // Save current plot as a reference plot
    // A.  Get comment text
    QString *str = EditDialog::run();
    
    // B.  Save this plot on server with id=x (make save function move existing plot automatically)

    
    printf("here\n");
    
    JevpPlot *nplot = new JevpPlot(*(src->plot));

    if(str) {
      nplot->setRefComment(str->toAscii().data());
    }
    else {
      nplot->setRefComment((char *)"No description available");
    }

    printf("through...\n");

    nplot->refid = true_refid();

    printf("nplot->refid  %d\n", nplot->refid);

    //printf("refwidget->logic = 0x%x 0x%x\n",refwidget->logic, nplot);

    JevpPlot *srcplot = src->plot;
    printf("plotname sorce %s\n",srcplot->GetPlotName());

    printf("plotname to write %s\n",nplot->GetPlotName());

    refwidget->logic->jl_writePlotToServer(nplot);
    
    printf("here2\n");
    // Replace main ref plot if applicable...
    if(true_refid() == 1) {
      refwidget->mainRefPlot->replaceData(nplot);
    }

    printf("here3\n");
    // add plot we are replacing to menu, and update existing one...
    PlotDisplay *dest_menu = NULL;
    if(true_refid() == 1) {
      dest_menu = refwidget->firstPlot;
    }
    else dest_menu = this;

    refwidget->addPlotToMenu(dest_menu, nplot);
  }
  
  if(src->refid > 0) {
    // Swap the plots on the display
    PlotDisplay *dest_menu = NULL;
    PlotDisplay *src_menu = NULL;


    if(true_refid() == 1) {
      dest_menu = refwidget->firstPlot;
    }
    else {
      dest_menu = this;
    }

    if(src->true_refid() == 1) {
      src_menu = refwidget->firstPlot;
    }
    else {
      src_menu = src;
    }



    // swap the menu plots...
    JevpPlot *tmp_disp = plot;
    JevpPlot *tmp_move = src->plot;
    int src_id = src->true_refid();
    int dst_id = true_refid();

    JevpPlot *newbigplot = NULL;
    if(true_refid() == 1) {
      newbigplot = tmp_move;
    }
    if(src->true_refid() == 1) {
      newbigplot = tmp_disp;
    }

    // change the big plot
    if(newbigplot) {
      refwidget->mainRefPlot->replaceData(newbigplot);
    }

    dest_menu->replaceData(tmp_move);
    src_menu->replaceData(tmp_disp);
    
    // Swap the plots on server...
    refwidget->logic->jl_swapRefsOnServer(plot->GetPlotName(), dst_id, src_id);     
  }
 
  event->acceptProposedAction();
}


void ReferenceWidget::dragEnterEvent(QDragEnterEvent *event)
{
  //printf("Ref: drag enter event...\n");
  // event->acceptProposedAction();
}

void ReferenceWidget::addPlotToMenu(PlotDisplay *display, JevpPlot *plot) {

  // Now create a new lastplot...
  PlotDisplay *pd = new PlotDisplay(this, NULL, lastPlot->menu_refid()+1, PD_THUMB);
  lastPlot->next = pd;
  pd->prev = lastPlot;
  lastPlot = pd;
  allRefLayout->addWidget(lastPlot);
  allRefBox->adjustSize();
  pd = pd->prev;
  
  // Shuffle plots up (down to display...)
  while((pd != display) && pd) {
    if(pd->prev) {
      pd->replaceData(pd->prev->plot);
      pd->plot->refid = pd->true_refid();
    }
    
    pd = pd->prev;
  }

  assert(pd != NULL);
 
  if(plot) {
    plot->refid = pd->true_refid();
  }
  pd->replaceData(plot);
}

ReferenceWidget::ReferenceWidget(JevpGui *logic, char *name) : QDialog()
{
  JevpPlot *plot;
  PlotDisplay *pd;

  this->logic = logic;
  this->histoname = name;

  //setAcceptDrops(true);

  QDialogButtonBox *buttonBox = new QDialogButtonBox(QDialogButtonBox::Ok);
  connect(buttonBox, SIGNAL(accepted()), this, SLOT(accept()));

  QVBoxLayout *layout = new QVBoxLayout;

  // Histos box
  QGroupBox *histosBox = new QGroupBox(tr("Compare to Reference"));
  QHBoxLayout *histosLayout = new QHBoxLayout;
  histosBox->setLayout(histosLayout);
  char error[256];

  // Get current data plot...
  plot = logic->jl_getPlotFromServer(name,error);
  pd = new PlotDisplay(this, plot, 0, PD_LARGE_CUR);
  histosLayout->addWidget(pd);

  // Get default reference plot...
  char refargs[100];
  sprintf(refargs, "%s;refid=1",name);
  plot = logic->jl_getPlotFromServer(refargs, error);

  pd = new PlotDisplay(this, plot, 1, PD_LARGE_REF);
  mainRefPlot = pd;
  histosLayout->addWidget(pd);
  layout->addWidget(histosBox);

  allRefBox = new QGroupBox(tr("All References"));
  allRefBox->setMinimumWidth(850);
  allRefLayout = new QHBoxLayout;
  allRefBox->setLayout(allRefLayout);
  PlotDisplay *prev = NULL;
  for(int i=1;;i++) {
    sprintf(refargs, "%s;refid=%d",name, i);
    
    if(i != 1) {   // if i==1, use plot from largeref...
      plot = logic->jl_getPlotFromServer(refargs,error);
    }

    pd = new PlotDisplay(this, plot, i+100, PD_THUMB);
    allRefLayout->addWidget(pd);
    pd->prev = prev;
    if(prev) prev->next = pd;
 
    lastPlot = pd;
    if(i==1) firstPlot = pd;

    prev = pd;
    if(!plot) break;   // deliberately after new plot display (want an extra one at end..)
  }
  

  QScrollArea *sa = new QScrollArea;
  sa->setMinimumHeight(170);
  sa->setMaximumHeight(170);
  sa->setWidget(allRefBox);
  
  layout->addWidget(sa);

  // 
  // Buttons box
  //QGroupBox *bottomBox = new QGroupBox(tr("Buttons Box"));
  layout->addWidget(buttonBox);

  setLayout(layout);
  setWindowTitle(tr(name));
}

void ReferenceWidget::mousePressEvent(QMouseEvent *e)
{
  //printf("Reference Widget::::   got a mouse press...\n");
}

void ReferenceWidget::removePlot(PlotDisplay *pd) 
{
  if(pd->refid == 1) {   // never be deleting the main ref
    pd = firstPlot;
  }

  // replace plots with next plot on up...

  if(pd->true_refid() == 1) {
    JevpPlot *np = pd->next ? pd->next->plot : NULL;
    mainRefPlot->replaceData(np);
    pd->prev = NULL;
  }
  
  PlotDisplay *curr = pd;
  
  while(curr->next) {
    curr->replaceData(curr->next->plot);
    curr = curr->next;
  }

  lastPlot = curr->prev;
  curr->prev->next = NULL;
  allRefLayout->remove(curr);
  allRefBox->adjustSize();
  delete curr;
}
