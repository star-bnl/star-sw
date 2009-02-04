#ifndef PresenterConnect_h
#define PresenterConnect_h

#include "EvpPresenter.h"
#include "PresenterGui.h"


#include <qwidget.h>
#include <qobject.h>
#include <qthread.h>
#include <qmutex.h>
#include "Updater.h"

class ServerStatus;

class PresenterConnect : public QObject {

  Q_OBJECT

 private:
  PresenterGui* mGui;
  EvpPresenter* mPresenter;

  int mTab;
  int mSubTab;
  TCanvas* mCanvas;
 public:

  PresenterConnect(PresenterGui* gui, EvpPresenter* pre);
  

 public slots:
 void save();
 void saveAs();
 void live();
 void file();
 void update();
 void update(TCanvas* cc, int tab, int subTab);
 void update(TCanvas*, const char*);
 void print();
 void setTab(int);
 void setSubTab(int);
 void setCanvas(TCanvas*);
 void openReference();
 
 signals:
 void signalEventInfo(int, int, int, int, unsigned int, unsigned int, unsigned int, unsigned int);
 void signalServerInfo(ServerStatus*);
 void updateRequest();
};


#endif
