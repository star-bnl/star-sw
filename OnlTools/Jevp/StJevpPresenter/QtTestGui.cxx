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


// QtTestGui implementation

QtTestGui::QtTestGui() : Q3MainWindow(), mWidth(900), mHight(500)
{
    refreshTimer = new QTimer(this);
    connect(refreshTimer, SIGNAL(timeout()), this, SLOT(refreshTimerFired()));
    refreshTimer->start(1000);
}

QtTestGui::~QtTestGui()
{
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
    QTabWidget *rootTab = new QTabWidget();
    CP;
    rootTab->resize(mWidth, mHight);
    CP;
   
    rootTab->addTab(new QLabel(tr("Tab 1")), tr("AAAA 1"));
    rootTab->addTab(new QLabel(tr("Tab 2")), tr("BBBB 2"));
    rootTab->addTab(new QLabel(tr("Tab 3")), tr("CCCC 3"));

    CP;
    ScreenWidget *sw = new ScreenWidget((char *)"boo", rootTab);
    CP;

    rootTab->addTab(sw, tr("Screen Widget"));
 
    
    // This connects the tab to the window...
    CP;
    rootTab->setParent(this);
    CP;
     
    QWidget *cw = centralWidget();
    printf("%p  vs %p\n", cw, rootTab);

    // Notifies currentChangedSlot() when a tab is changed...
    connect(rootTab, SIGNAL(currentChanged(int)), this, SLOT(currentChangedSlot(int)));
    connect(qApp,SIGNAL(lastWindowClosed()),this, SLOT(ClosePresenter()));
    resize(mWidth,mHight);

    setCaption("Test...");

    show();
}

void QtTestGui::ClosePresenter() 
{
  LOG("JEFF", "Exit....");
  // Qt [slot] to terminate the application
  gApplication->Terminate();
}

void QtTestGui::currentChangedSlot(int x) {
    printf("changed tab to %d\n", x);
}


// Screen Widget Implementation
ScreenWidget::ScreenWidget(char *name, QTabWidget *menu) {
    parentMenu = menu;
}

void ScreenWidget:: mouseDoubleClickEvent(QMouseEvent *e) {
    printf("double click\n");
}

void ScreenWidget::mousePressEvent(QMouseEvent *e) {
    printf("mouse press\n");
}
