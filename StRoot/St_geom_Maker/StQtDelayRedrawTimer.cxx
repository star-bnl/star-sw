// Author: Valeri Fine   15/09/2009
// ****************************************************************************
// ** $Id: StQtDelayRedrawTimer.cxx,v 1.2 2009/09/17 17:40:49 fine Exp $
#include "StQtDelayRedrawTimer.h"
#include <QtCore/QTimer>
#include <QDebug>
//________________________________________________________________
StQtDelayRedrawTimer::StQtDelayRedrawTimer(QObject *parent) : QObject (parent)
, fComplexVolumeTimer(new QTimer(this)),fSingeVolumeTimer(new QTimer(this))
, fObj(0)
{
   fComplexVolumeTimer->stop(); fComplexVolumeTimer->setSingleShot(true);
   fSingeVolumeTimer  ->stop(); fSingeVolumeTimer  ->setSingleShot(true);
   
   connect(fComplexVolumeTimer,SIGNAL(timeout()),this,SLOT(DrawObject()));
   connect(fSingeVolumeTimer,  SIGNAL(timeout()),this,SLOT(DrawObject()));
}
//________________________________________________________________
 void StQtDelayRedrawTimer::DrawObject(TObject *obj,bool expanded,int depth) 
 {
    // Delay rendering for a few seconds to allow user to change his/her mind / selections 
    fObj = obj;
    QTimer *timer = expanded ? fComplexVolumeTimer : fSingeVolumeTimer;
    // reset  delay time
    if ( timer->isActive() ) timer->stop();
    int delay = 600; // msec
         if ( !expanded ) delay +=    0;
    else if ( depth < 3 ) delay +=   50;
    else if ( depth < 5 ) delay +=  350;
    else if ( depth < 7 ) delay +=  700;
    else                  delay += 1200;
    timer->start(delay);
 }

//________________________________________________________________
void StQtDelayRedrawTimer::DrawObject()
{
   if (fObj) {
      emit DrawObjectSignal(fObj,sender() == fComplexVolumeTimer);
   }
}
//________________________________________________________________
bool StQtDelayRedrawTimer::IsActive(bool expanded) const
{
    return expanded ? fComplexVolumeTimer->isActive() : fSingeVolumeTimer->isActive();
}
