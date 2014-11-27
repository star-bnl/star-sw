#ifndef ROOT_TQUserEvent
#define ROOT_TQUserEvent

#include <QtGlobal>
#include "GuiTypes.h"

class TQUserEvent : public 
   QEvent 
{
private:
   Event_t *fEvent;
public:
   Event_t *data() const { return fEvent;}
   void setData(Event_t *ev) { delete data(); fEvent=ev;}
   TQUserEvent(const Event_t &pData) : QEvent(Type(QEvent::User+Type(1))),fEvent(0)
   {   setData(new Event_t); *(Event_t *)data() = pData;  }
   ~TQUserEvent() { delete (Event_t *)data(); }
   void getData( Event_t &user) const { user = *(Event_t*)data(); }
   static Type Id() { return Type(QEvent::User + Type(1) /*kQTGClientEvent*/) ;}
};

#endif
