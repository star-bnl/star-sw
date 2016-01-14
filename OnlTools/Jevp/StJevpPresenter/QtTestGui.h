#ifndef _QTTESTGUI_H_
#define _QTTESTGUI_H_

#include <iostream>
#include <stdlib.h>
#include <stdio.h>

#include <TString.h>
#include <TObjString.h>
#include <TObjArray.h>
#include <TRegexp.h>
#include <string>
#include <fstream>
#include <sstream>

#include <TROOT.h>
#include <TSystem.h>
#include <TMessage.h>

#include <TQtRootAction.h>
#include <TSocket.h>

#include <TText.h>

#include <qobject.h> 
#include <TQtWidget.h>
#include <qtabwidget.h>


#  include <q3intdict.h> 
#  include <q3mainwindow.h>
#  include <Q3Frame>
#include "q3hbox.h"

using namespace std;


//class ScreenWidget : public TQtWidget {
class ScreenWidget : public QWidget {
 public:
    QTabWidget *parentMenu;
    ScreenWidget(char *name, QTabWidget *menu);
    void mouseDoubleClickEvent(QMouseEvent *e);
    void mousePressEvent(QMouseEvent *e);
};


// ************************************************************************
class QtTestGui : public Q3MainWindow {
     
    Q_OBJECT

 private:
    QTimer *refreshTimer;

    int mWidth;
    int mHight;
  
    Q3HBox *mCentralWidget;
    Int_t   fGuiRefreshRate; // msec.
    
 public:
    QtTestGui();
    virtual ~QtTestGui();
    void init(); 


 public slots:
    void ClosePresenter();
    void refreshTimerFired();
    void currentChangedSlot(int);

 signals:

};
//************************************************************************




#endif


