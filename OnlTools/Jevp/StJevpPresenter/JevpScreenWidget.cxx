#include "JevpScreenWidget.h"

#include <QPushButton>
#include <QLayout>
#include <QTimer>
#include <QPaintEvent>
#include <QResizeEvent>
#include <QMouseEvent>
#include <QMessageBox>
#include <q3popupmenu.h>

#include <TClass.h>

#include <rtsLog.h>

#include <EvpMain.h>
#include <JevpGui.h>
#include <ReferenceWidget.h>
#include <ZoomWidget.h>
#include <Jevp/StJevpPlot/DisplayDefs.h>
#include <RTS/include/SUNRT/clockClass.h>

extern JevpGui *gJevpGui;

JevpScreenWidget::JevpScreenWidget(char *name, u_int combo_index, QWidget *parent) : QWidget(parent) {
    //setAttribute(Qt::WA_NativeWindow, true);
    setAttribute(Qt::WA_PaintOnScreen, true);
    setAttribute(Qt::WA_OpaquePaintEvent, true);
    setMinimumSize(600, 400);
    //setUpdatesEnabled(kFALSE);
    //setMouseTracking(kTRUE);

    plotItems = new TList();
    jevpPlots = new THashTable();
    
    this->combo_index = combo_index;
    LOG(DBG,"Creating JevpScreenWidget for %s\n",name);
    this->name = new std::string(name);
    npads = 0;
    cleanTime = 0;
}

void JevpScreenWidget::init() {
    ULong_t w = winId();
    //    LOG("JEFF", "init[%d]: winId=%lu", combo_index, w);
    wid = gVirtualX->AddWindow(w, 600, 400);
    tcanvas = new TCanvas(name->c_str(), width(), height(), wid);
    tcanvas->SetDoubleBuffer(0);
}

JevpPlot *JevpScreenWidget::getJevpPlot(char *name) {
    
    JevpPlot *plot = (JevpPlot *)jevpPlots->FindObject(name);
    
    //LOG("JEFF", "getJevpPlot[%s] --> %p", name, plot);
    
    //THashTableIter next(jevpPlots);
    //JevpPlot *p;
    //while((p = (JevpPlot *)next())) {
    // 	LOG("JEFF","      name=%s\n",p->GetPlotName());
    //}

    return plot;
}

JevpScreenWidget::~JevpScreenWidget() {
    //LOG("JEFF","Deleting JevpScreenWidget for %s\n",name->c_str());
    
    if(tcanvas) delete tcanvas;
    gVirtualX->RemoveWindow(wid);
    if(name) delete name;
}

void JevpScreenWidget::mousePressEvent(QMouseEvent *e)
{
    if(e->buttons() & Qt::RightButton) {
	int wide, deep;
	DisplayNode *node = gJevpGui->jl_getCanvasDescriptor(combo_index);
    
	const char *tmp = node->parent->getProperty("wide");
	if(tmp) wide = atoi(tmp);
	else wide = 1;
    
	tmp = node->parent->getProperty("deep");
	if(tmp) deep = atoi(tmp);
	else deep = 1;
    
	double xx = (double)e->x() / (double)width();
	xx *= wide;
	int x = (int)xx;
    
	double yy = (double)e->y() / (double)height();
	yy *= deep;
	int y = (int)yy; 
    
	int nn = y * wide + x;
    
	if(nn < 0) return;
	if(nn > node->nSiblings()) return;
    
	DisplayNode *hnode = node;
	for(int i=0;i<nn;i++) {
	    hnode = hnode->next;
	}


#ifdef USE_QTIP
	QToolTip::showText(e->globalPos(), hnode->name);
#else

	// Add a popup menu...
	Q3PopupMenu *pop = new Q3PopupMenu(this);
	pop->insertItem(hnode->name,NULL,1,1);
	pop->insertItem("Reference Plot...",NULL,2,2);
	pop->insertItem("Zoom Plot...",3,3);
    
	int ret = pop->exec(QCursor::pos());
	{
	    if(ret == 1) {
		LOG("JEFF", "Do nothing...");
	    }
	    else if (ret == 2) {
		LOG("JEFF", "Do reference plots");
		if(hnode->name) {
		    CP;
		    ReferenceWidget *ref = new ReferenceWidget(gJevpGui, hnode->name);
		    CP;
		    ref->exec();
		    CP;
		    LOG(DBG,"Returned....\n");
		    delete ref;
		}
	    }
	    else if (ret == 3) {
		LOG("JEFF", "Do zoom plot");
		if(hnode->name) {
		    CP;
		    ZoomWidget *ref = new ZoomWidget(gJevpGui, hnode->name);
		    CP;
		    ref->exec();
		    CP;
		    LOG(DBG,"Returned....\n");
		    delete ref;
		}
	    }
	}

	delete pop;
#endif

	//LOG("JEFF","Got a mouse press event...%d %d  %s:\n",x,y,hnode->name);
    }
}

void JevpScreenWidget::mouseDoubleClickEvent(QMouseEvent *e)
{
    int wide, deep;
    DisplayNode *node = gJevpGui->jl_getCanvasDescriptor(combo_index);
  
    const char *tmp = node->parent->getProperty("wide");
    if(tmp) wide = atoi(tmp);
    else wide = 1;

    tmp = node->parent->getProperty("deep");
    if(tmp) deep = atoi(tmp);
    else deep = 1;
 
    double xx = (double)e->x() / (double)width();
    xx *= wide;
    int x = (int)xx;

    double yy = (double)e->y() / (double)height();
    yy *= deep;
    int y = (int)yy; 
  
    int nn = y * wide + x;

    if(nn < 0) return;
    if(nn > node->nSiblings()) return;
  
    DisplayNode *hnode = node;
    for(int i=0;i<nn;i++) {
	hnode = hnode->next;
    }
  
    LOG(DBG,"Got a mouse press event...%d %d  %s:\n",x,y,hnode->name);
  
    if(hnode->name) {
	ReferenceWidget *ref = new ReferenceWidget(gJevpGui, hnode->name);
	ref->exec();
	LOG(DBG,"Returned....\n");
	delete ref;
    }
}

TCanvas *JevpScreenWidget::GetCanvas() {
    return tcanvas;
}

void JevpScreenWidget::addJevpPlot(JevpPlot *mplot) {  
    mplot->GetPlotName();
    //LOG("JEFF", "Added plot %s", mplot->GetPlotName());
    jevpPlots->Add(mplot); 
}

void JevpScreenWidget::addPlotItem(TObject *mplot) {
    //mplot->SetBit(kCanDelete); 
    plotItems->Add(mplot); 
    LOG(DBG, "plots has %d entries",plotItems->GetSize());
}

void JevpScreenWidget::DeleteItems() {

    LOG(DBG, "Clearing Screen Widget...");
    TListIter next(plotItems);
    TObject *o;

    while((o = (TObject *)next())) {
	LOG(DBG, "Deleting an object... %s can %d must %d",o->GetName(),o->TestBit(kCanDelete),o->TestBit(kMustCleanup));
	delete o;
    }

    plotItems->Clear(); 
    jevpPlots->Clear();

    LOG(DBG, "Done Clearing Screen Widget");
}

void JevpScreenWidget::resizeEvent(QResizeEvent *) {
    //printf("Resize event\n");
    if(tcanvas) {
	tcanvas->Resize();
	tcanvas->Update();
    }
}

void JevpScreenWidget::paintEvent(QPaintEvent *) {
    //printf("Paint event\n");
    if(tcanvas) {
	tcanvas->Resize();
	tcanvas->Update();
    }
}

void JevpScreenWidget::DownloadPlotFromServer(TSocket *server, char *name) {
    RtsTimer_root clock;
    clock.record_time();

    // Ask server for plot...
    EvpMessage msg;
    msg.setCmd("getplot");
    msg.setArgs(name);
    TMessage mess(kMESS_OBJECT);
    mess.WriteObject(&msg);
    server->Send(mess);
    

    TMessage *rmsg;
    int ret = server->Recv(rmsg);
    
    double t1 = clock.record_time();
    
    if(ret == 0) {  // disconnect
	LOG(ERR,"Server disconnected?\n");
	return;
	//return NULL;
    }

    LOG(DBG, "Message class: %s",rmsg->GetClass()->GetName());
    if(strcmp(rmsg->GetClass()->GetName(), "EvpMessage") == 0) {
	// There was no valid object...
	EvpMessage *msg = (EvpMessage *)rmsg->ReadObject(rmsg->GetClass());
	LOG(ERR, "No valid plot for %s", name);
	
	delete msg;
	delete rmsg;
	return;
	//return NULL;
    }

    if(strcmp(rmsg->GetClass()->GetName(), "JevpPlot") == 0) {
	JevpPlot *plot = (JevpPlot *)rmsg->ReadObject(rmsg->GetClass());

	double t2 = clock.record_time() + t1;

	//if(t2 > .25)
	//  LOG("JEFF", "Download plot %s took %lf seconds (%lf for ethernet)",name, t2, t1);
	
	delete rmsg;
	
	//LOG("JEFF", "Got plot: %s", name);

	addJevpPlot(plot);
	return;
	//return plot;
    }

    LOG(ERR,"Invalid message type: (%s)\n",rmsg->GetClass()->GetName());
    delete rmsg;
    //return NULL;
}

//    DisplayNode *displayTab = displayFile->getTab(combo_index);
void JevpScreenWidget::DownloadAllPlotsFromServer(TSocket *server, DisplayNode *displayTab)  {
    //LOG("JEFF", "Download all plots from server (%d)", combo_index);
    assert(displayTab->leaf);

    DeleteItems();

    int nplots=0;

    RtsTimer_root clock;   
    clock.record_time();
  
    while(displayTab) {
	nplots++;
       	DownloadPlotFromServer(server, displayTab->name);
	displayTab = displayTab->next;
    }

    double t = clock.record_time();
    //LOG("JEFF", "Downloaded all plots[%d].  %d plots in %lf seconds", combo_index, nplots, t);
}

DisplayNode *JevpScreenWidget::getDisplayTab(DisplayFile *display) {
    return display->getTab(combo_index);
}

void JevpScreenWidget::DrawOnScreen(DisplayNode *displayTab) {

    //LOG("JEFF", "DrawOnScreen[%d] winid = %lu",combo_index, winId());
    CP;

    //tcanvas->setUpdatesEnabled(false);

    assert(displayTab->leaf);
    
    CP;
    DisplayNode *originalTab = displayTab;

    RtsTimer_root clock2;
    RtsTimer_root clock;   
    clock.record_time();
    
    CP;
    int nplots = displayTab->nSiblings() + 1;  
    int wide = displayTab->getIntParentProperty("wide");
    int deep = displayTab->getIntParentProperty("deep");
    int scaley = displayTab->getIntParentProperty("scaley");
    double maxY = 0;

    CP;
    // If scaling all plots the same, calculate the maxy
    //LOG("JEFF", "Scaley=%d",scaley);
    if(scaley>0) {
	while(displayTab) {
	    CP;
	    JevpPlot *plot = getJevpPlot(displayTab->name);
	    
	    if(!plot) {
		continue;
	    }

	    CP;
	    double my = plot->getMaxY();
	    CP;
	    if(my > maxY) maxY = my;
	    CP;
	    displayTab = displayTab->next;
	    CP;
	}
    }
 
    
    CP;
    //LOG("JEFF", "wide = %d deep = %d scaley = %d (%lf) nplots=%d", wide, deep, scaley, maxY, nplots);

    if(npads == 0) {
	tcanvas->Clear();
	tcanvas->Divide(wide, deep);
	npads = wide*deep;
    }

    CP;

    displayTab = originalTab;


    LOG(NOTE, "screenwidget prep: %lf", clock.record_time());

    for(int pad=1;pad <= wide*deep; pad++) {
	tcanvas->cd(pad);

	//gPad->Clear();
	
	CP;
	if(!displayTab) {
	    drawEmptySpace();
	    continue;          // don't try to go to the next one!
	}

	CP;
       	JevpPlot *plot = getJevpPlot(displayTab->name);

	if(!plot) {
	    LOG(NOTE, "Didn't get plot %s", displayTab->name);
	}
	else {
	    //LOG(NOTE, "%s: %p %d %d", displayTab->name, plot, plot->needsdata, plot->isDataPresent());
	}
	if(plot && (!plot->needsdata || plot->isDataPresent())) {
	    CP;
	    plot->draw();
	}
	else {
	    CP;
	    char tmp[256];
	    sprintf(tmp, "No data for plot: %s", displayTab->name);
	    drawNoDataPresent(tmp);
	}
	
	displayTab = displayTab->next;
    }
	
    LOG(NOTE, "screenwidget draw: %lf", clock.record_time());

    CP;

    //tcanvas->setUpdatesEnabled(true);
    clock2.record_time();
    tcanvas->Resize();
    LOG(NOTE, "resize: %lf", clock2.record_time());
    tcanvas->Modified();
    LOG(NOTE, "mod: %lf", clock2.record_time());
    tcanvas->Update();
    LOG(NOTE, "upd: %lf", clock2.record_time());
    
    LOG(NOTE, "screenwidget update: %lf", clock.record_time());
    CP;

    double t = clock.record_time();
    cleanTime = time(NULL);
    
    //LOG(NOTE, "Draw idx=%d. %d plots in %lf seconds\n", combo_index, nplots, t);
}


void JevpScreenWidget::drawEmptySpace()
{
    TText *t = new TText(0.5, 0.5, "Empty Space");
    
    addPlotItem(t);
    t->SetTextColor(3);
    t->SetTextAlign(22);
    t->Draw();
    return;
}

void JevpScreenWidget::drawNoDataPresent(char *name) {
    TText* t = new TText(0.5, 0.5, name);
    
    addPlotItem(t);
    t->SetTextColor(3);
    t->SetTextAlign(22);
    t->Draw();
}

void JevpScreenWidget::drawCrossOfDeath(char *name) {
    TLine* a = new TLine(0.,0.,1.,1.);
    TLine* b = new TLine(0.,1.,1.,0.);
    TText* t = new TText(0.5,0.5,name);
    
    addPlotItem(a);
    addPlotItem(b);
    addPlotItem(t);

    a->SetLineColor(2);
    b->SetLineColor(2);
    t->SetTextColor(3);
    t->SetTextAlign(22);

    a->Draw();
    b->Draw();
    t->Draw();
}

