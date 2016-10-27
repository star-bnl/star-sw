#ifndef VecBosMcEvent_h
#define VecBosMcEvent_h

#include "TObject.h"

#include "StMcEvent/StMcEvent.hh"
#include "StMcEvent/StMcTrack.hh"


class VecBosMcEvent : public TObject
{
public:

   VecBosMcEvent();
   VecBosMcEvent(StMcEvent &stMcEvent);

protected:

   StMcEvent *mStMcEvent;

	ClassDef( VecBosMcEvent, 1 )
};

#endif
