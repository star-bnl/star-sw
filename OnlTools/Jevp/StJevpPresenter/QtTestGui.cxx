#include "QtTest.h"
#include "QtTestGui.h"
#include "TQtRootSlot.h"
#include "TROOT.h"
#include "TRint.h"
#include <TSocket.h>
#include <TClass.h>
#include <TLine.h>
#include "TEnv.h"
#include "TQtWidget.h"
#include "TQtZoomPadWidget.h"
#include <qinputdialog.h>

#include "qapplication.h"
#include "qtabwidget.h"
#include "qpushbutton.h"
#include "qmenubar.h"
#include "qlayout.h"
#include "qtooltip.h"
#include "qpixmap.h"
#include "qlineedit.h"
#include "qlabel.h"
#include "qcursor.h"
#include "qtimer.h"
#include <mcheck.h>
#include <RTS/include/SUNRT/clockClass.h>
#include <rtsLog.h>
#include <q3filedialog.h>
#include "q3vbox.h"
#include "q3hbox.h"
#include "q3mainwindow.h"
#include "q3popupmenu.h"
#include "qicon.h"
#include "q3progressbar.h"
#include "q3toolbar.h"
#include <QCustomEvent>
#include <Q3Frame>
#include <stack>



QtTestGui *gQtTestGui;

// Suspend signal emitting until this class is deleted...
class  PresenterSuspend {
private:
  std::stack<QObject *> fWidgets;

public:
  PresenterSuspend(){};
  
  ~PresenterSuspend(){    
    while (!fWidgets.empty()) {
      fWidgets.top()->blockSignals(false);
      fWidgets.pop();
    }
  }
  
  void operator<<(QObject *o) {
    o->blockSignals(true); 
    fWidgets.push(o);
  }
};

//------------------------------------------------------------------------
QtTestGui::QtTestGui() : Q3MainWindow(), mWidth(900), mHight(500)
{
  gQtTestGui = this;

  refreshTimer = new QTimer(this);
  connect(refreshTimer, SIGNAL(timeout()), this, SLOT(refreshTimerFired()));
  refreshTimer->start(2500);
}

//----------------------------------------------------------------
QtTestGui::~QtTestGui()
{
}

//----------------------------------------------------------------
void QtTestGui::CloseWindow()
{
    qApp->closeAllWindows();
}

void QtTestGui::refreshTimerFired()
{ 
    LOG("JEFF", "Timer fired...");  
}


void QtTestGui::init()
{
    CP;
    LOG("JEFF", "Presenter pid=%d",getpid());

    CP;
    setAttribute(Qt::WA_DeleteOnClose);
    setWindowModality(Qt::WindowModal);

    CP;
    PresenterSuspend blockWidgets;
    blockWidgets << this;

    mCentralWidget = new Q3HBox(this);
    blockWidgets << mCentralWidget;

    mCentralWidget->setMargin(0);
    mCentralWidget->setSpacing(0); 
    setCentralWidget(mCentralWidget);
    
    //
    // Define Tabs
    //
    // main tab holding
    CP;
     
    connect(qApp,SIGNAL(lastWindowClosed()),TQtRootSlot::CintSlot(),SLOT(TerminateAndQuit()));
    connect(qApp,SIGNAL(lastWindowClosed()),this, SLOT(jl_ClosePresenter()));
    resize(mWidth,mHight);

    //SetWindowName("Live...");

    show();
 

  
    connect(this,SIGNAL(update()), this, SLOT(update()) ); 

    pc_mCanvas = 0;
}

void QtTestGui::UpdatePlots() {
    cout << "update plots...";
}
//______________________________________________________________________________ 
void QtTestGui::jl_ClosePresenter() 
{
  LOG("JEFF", "Exitint....");
  // Qt [slot] to terminate the application
  gApplication->Terminate();
}

