#ifndef ZBosMcEvent_h
#define ZBosMcEvent_h

#include "TLorentzVector.h"
#include "TObject.h"

#include "VecBosMcEvent.h"


class ZBosMcEvent : public VecBosMcEvent
{
public:

   TLorentzVector mP4ZBoson;
   TLorentzVector mP4Electron;
   TLorentzVector mP4Positron;

   ZBosMcEvent();

	ClassDef( ZBosMcEvent, 1 )
};

#endif
