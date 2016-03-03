#ifndef _JEVPSCREENWIDGET_H_
#define _JEVPSCREENWIDGET_H_

#include <QWidget>
#include <QTimer>
#include <QVBoxLayout>
#include <QPushButton>

class QPaintEvent;
class QResizeEvent;
class QMouseEvent;
class QPushButton;
class QTimer;
class QTabWidget;

#include <TSystem.h>
#include <TList.h>
#include <TCanvas.h>
#include <TStyle.h>
#include <THashTable.h>
#include <TSocket.h>
#include <TLine.h>

#include <rtsLog.h>

#include "Jevp/StJevpPlot/JevpPlot.h"
#include "Jevp/StJevpPlot/DisplayDefs.h"

class JevpScreenWidget : public QWidget {
    Q_OBJECT

 private:
    int wid;
    TCanvas *tcanvas;

    // The jevpPlots & plotItems
    // are just lists of Items to delete when we are done with them!
    THashTable *jevpPlots;
    TList *plotItems;   

    int npads;
 public:
    u_int combo_index;
    std::string *name;

    JevpScreenWidget(char *name, u_int combo_index, QWidget *parent);
    void init();
    TCanvas *GetCanvas();

    virtual void mouseDoubleClickEvent(QMouseEvent *e);
    virtual void mousePressEvent(QMouseEvent *e);

    virtual void paintEvent(QPaintEvent *e);
    virtual void resizeEvent(QResizeEvent *e);
    
    void addJevpPlot(JevpPlot *mplot);
    JevpPlot *getJevpPlot(char *name);
    void addPlotItem(TObject *mplot);

    // Removes all items from jevpPlots and plotItems...
    void DeleteItems();

    // Draw
    DisplayNode *getDisplayTab(DisplayFile *display);

    void DrawOnScreen(DisplayNode *displayTab);

    void DownloadPlotFromServer(TSocket *server, char *name);
    void DownloadAllPlotsFromServer(TSocket *server, DisplayNode *displayTab);
    void drawEmptySpace();
    void drawNoDataPresent(char *name);
    void drawCrossOfDeath(char *name);
   
    virtual ~JevpScreenWidget();
  
};


class MetaWidget : public QWidget {
    Q_OBJECT
public:
    JevpScreenWidget *widget;
    TH1F *histo;
    JevpPlot *plot;

    MetaWidget(QWidget *parent) : QWidget(parent) {
	histo = NULL;
	plot = NULL;
    }
    
    void init() {
	QVBoxLayout *l = new QVBoxLayout(this);
	setMinimumSize(600, 500);

	widget = new JevpScreenWidget((char *)"plotname", 10102, this);
	l->addWidget(widget);
	l->addWidget(new QPushButton((char *)"Blah", this));
	
	QTimer *fRootTimer = new QTimer(this);
	QObject::connect(fRootTimer, SIGNAL(timeout()), this, SLOT(updates()));
	fRootTimer->start(5000);
    }

public slots:
    void handle_root_events() {
	gSystem->ProcessEvents();
    }

    void updates() {
	if(plot) delete plot;

	histo = new TH1F("blah", "abcd", 1000, -5, 5);
	histo->FillRandom("gaus", 1000);

	
	plot = new JevpPlot(histo);

	widget->GetCanvas()->cd();
	plot->draw();
	widget->GetCanvas()->Resize();
	widget->GetCanvas()->Modified();
	widget->GetCanvas()->Update();
       
    }
};
#endif
