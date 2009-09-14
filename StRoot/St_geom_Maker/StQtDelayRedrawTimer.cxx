#include "StQtDelayRedrawTimer.h"
//________________________________________________________________
StQtDelayRedrawTimer::StQtDelayRedrawTimer(QObject *parent) : QTimer (parent)
,fObj(0),fExpanded(false)
{
   this->stop();
   this->setSingleShot(true);
   connect(this,SIGNAL(timeout()),this,SLOT(DrawObject()));
}
//________________________________________________________________
 void StQtDelayRedrawTimer::DrawObject(int delay, TObject *obj,bool expanded) 
 {
    fObj = obj;
    fExpanded = expanded;
    // reset  delay time
    this->stop();
    this->start(delay);
 }

//________________________________________________________________
 void StQtDelayRedrawTimer::DrawObject()
 { 
    if (fObj) {
      emit DrawObjectSignal(fObj,fExpanded);
    }
 }
