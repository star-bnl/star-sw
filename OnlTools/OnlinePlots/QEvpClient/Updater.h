#ifndef Updater_h
#define Updater_h

#include <qwidget.h>
#include <qobject.h>
#include <qthread.h>
#include <qmutex.h>
//#include <iostream.h>
#include <qapplication.h>

#include "UpdateEvent.h"

class Updater :  public QObject, public QThread 
{

  Q_OBJECT
  
 private:
    bool mRun;
  QWidget* mReciever;
 public: 
  Updater(QWidget* r) : mRun(false), mReciever(r) {};
  void on()  { mRun = true;  if ( !running() ) start(); }
  void off() { mRun = false; if (  running() ) terminate(); }

  void run() {
    while (true) {
      //cout << "updater" << endl;
      if (mRun) { 
	QApplication::postEvent(mReciever, new UpdateEvent() );
	sleep(1); 
      }
      sleep(4);
    }
  }
  
 public slots:
   void autoUpdate() {
   mRun  ? off() : on();
 }
};




#endif
