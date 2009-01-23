#ifndef UpdateEvent_h
#define UpdateEvent_h

#include <qevent.h>

 class UpdateEvent : public QEvent {
    public:
        UpdateEvent()
            : QEvent( (Type)1000 ) {}
    private:
};


#endif
