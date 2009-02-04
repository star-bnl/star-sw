#include "PresenterConnect.h"

#include <qfiledialog.h>

#include "TH1.h"
#include "TCanvas.h"

//class TCanvas;
class QWidget;

PresenterConnect::PresenterConnect(PresenterGui* gui, EvpPresenter* pre) :
  mGui(gui), mPresenter(pre) {

  connect(mGui,SIGNAL(live()), this, SLOT(live()) ); 
  connect(mGui,SIGNAL(file()), this, SLOT(file()) ); 
  connect(mGui,SIGNAL(update(TCanvas*, int, int )), this, SLOT(update(TCanvas*, int, int )) ); 
  connect(mGui,SIGNAL(update(TCanvas*, const char* )), this, SLOT(update(TCanvas*, const char*)) ); 
  connect(mGui,SIGNAL(update()), this, SLOT(update()) ); 
  connect(mGui,SIGNAL(save()), this, SLOT(save()) ); 
  connect(mGui,SIGNAL(saveAs()), this, SLOT(saveAs()) ); 
  connect(mGui,SIGNAL(print()), this, SLOT(print()) ); 
  connect(mGui,SIGNAL(openReference()), this, SLOT(openReference()) ); 

  connect(mGui,SIGNAL( tab(int) ),         this, SLOT( setTab(int)) ); 
  connect(mGui,SIGNAL( subTab(int) ),      this, SLOT( setSubTab(int)) ); 
  connect(mGui,SIGNAL( canvas(TCanvas*) ), this, SLOT( setCanvas(TCanvas*)) ); 

  connect(this, SIGNAL( signalEventInfo(int,int,int,int, unsigned int, unsigned int,unsigned int, unsigned int) ), mGui, SLOT( setEventInfo(int,int,int,int, unsigned int, unsigned int,unsigned int, unsigned int) ) ); 
  connect(this, SIGNAL( signalServerInfo(ServerStatus*) ), mGui, SLOT( setServerInfo(ServerStatus*) ) ); 
  connect(this, SIGNAL( updateRequest() ), mGui, SLOT( updateRequest() ) );


  connect(mPresenter, SIGNAL( addGroupTab(const char*) ), mGui, SLOT( addGroupTab(const char*) ) );
  connect(mPresenter, SIGNAL( addGroup(const char*) ), mGui, SLOT( addGroup(const char*) ) );
  connect(mPresenter, SIGNAL( removeGroupTabs()), mGui, SLOT( removeGroupTabs()) );
  connect(mPresenter, SIGNAL( setEnabled(bool)) , mGui, SLOT( setEnabled(bool) ) );

  // Gui --> Presenter
  connect(mGui,SIGNAL(printAll(const char*)), mPresenter, SLOT(printAll(const char*)) );
  connect(mGui,SIGNAL(nextEvent()), mPresenter, SLOT(NextEvent()) );
//  connect(qApp,SIGNAL(lastWindowClosed()),mPresenter,ClosePresenter()));
  mCanvas = 0;
}


void PresenterConnect::save() {
  mPresenter->Save("");
}

void PresenterConnect::saveAs() {
  QString dir(EvpUtil::GetOutputPath());
  QString filter("*.root");
  QFileDialog dialog( dir, filter, mGui, "", true );
  dialog.exec();
  if (!dialog.selectedFile().isEmpty()) {
    mPresenter->Save( dialog.selectedFile().ascii() );
  }
}


void PresenterConnect::live() {
  //cout << "liveButton" << endl;
  mPresenter->Stop();   // will restart automatically
  mPresenter->SetSource();
}

void PresenterConnect::file() {
  //cout << "fileButton" << endl;
  QString dir(EvpUtil::GetOutputPath());
  QString caption("File dialog");
  QFileDialog dialog(dir, QString(), mGui,caption);
  dialog.setCaption(caption);
  dialog.addFilter("*.root");
  //  dialog.addFilter("*.map");
  dialog.exec();

  QString file = dialog.selectedFile();
  QString mapFile = file;
  int iret = 0;
//  if ( file.find(".map") < 0 ) {   // must be root file, only *.root and *.map are allowed
//    mapFile.replace(".root",".map");
//    iret = EvpUtil::Root2Map(file,mapFile);
//  } 

  if (iret) {
    cerr << "### error ### Can not open file : " << mapFile << endl;
    return;
  }

  mPresenter->SetSource( mapFile.ascii() );
  emit updateRequest();
}

void PresenterConnect::openReference() {
  cout << "Opening reference" << endl;
  PresenterGui* gui2 = new PresenterGui();
  gui2->resize(500,500);
  gui2->show();
//  EvpUtil::ReadCanvasDefinitions();
  EvpPresenter* presenter2 = new EvpPresenter(EvpUtil::mReference);
  PresenterConnect* con2 = new PresenterConnect(gui2,presenter2);

//  QString file = "/home/dkettler/test/run10029077.root";
//  QString mapFile = file;
//  mapFile.replace(".root",".map");
//  int iret = EvpUtil::Root2Map(file,mapFile);
//  
//  if (iret) {
//    cerr << "### error ### Can not open file : " << mapFile << endl;
//    return;
//  }

//  QString mapFile = "/a/pplot/histos/run10031084.map";
//  presenter2->SetSource( EvpUtil::mReference );
}
   
void PresenterConnect::update() {
  if ( mPresenter->serverStatus()->diffTimeInSec() >120.) {
    if ( mPresenter->Status() ) {
      live();
      cout << "live again "<< endl;
    }
  }
  emit signalEventInfo(mPresenter->run(),mPresenter->event(),mPresenter->counter(),mPresenter->token(),mPresenter->triggerBits(),mPresenter->detectorBits(),mPresenter->triggerBitsRun(),mPresenter->detectorBitsRun());
  emit signalServerInfo(mPresenter->serverStatus());
}


void PresenterConnect::update(TCanvas* canvas, int tab, int subTab) {
  update();
  if (canvas) mPresenter->Draw(canvas,mTab,mSubTab);
}

void PresenterConnect::update(TCanvas* canvas, const char* name) {
  update();
  if (canvas) mPresenter->Draw(canvas, name );
}

void PresenterConnect::print() {
  //cout << "print" << endl;
  int tab = mGui->GetTabId();
  int subTab = mGui->GetSubTabId();
  TCanvas* cc = mGui->GetCanvas();
  mPresenter->Print(cc,tab,subTab);
}
   
   
void PresenterConnect::setTab(int t) { mTab = t;} 
void PresenterConnect::setSubTab(int t) { mSubTab = t;} 
void PresenterConnect::setCanvas(TCanvas* t) { mCanvas = t;} 
