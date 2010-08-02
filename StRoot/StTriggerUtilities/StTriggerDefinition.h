// -*- mode:c++ -*-

#ifndef StTriggerDefinition_h
#define StTriggerDefinition_h

#include <cstdio>
#include "TObject.h"
#include "TString.h"

struct StTriggerDefinition : public TObject {
  Int_t   triggerIndex;		// trigger index
  TString name;			// trigger name
  Int_t   triggerId;		// trigger ID
  Int_t   onbits;	        // bits required to be ON at input of TCU
  Int_t   offbits;	        // bits required to be OFF at input of TCU
  Int_t   onbits1;
  Int_t   onbits2;
  Int_t   onbits3;
  Int_t   offbits1;
  Int_t   offbits2;
  Int_t   offbits3;

  void print();

  ClassDef(StTriggerDefinition,1)
};

void StTriggerDefinition::print()
{
  printf("triggerIndex=%d name=%s triggerId=%d onbits=0x%04x offbits=0x%04x\n",
	 triggerIndex,name.Data(),triggerId,onbits,offbits);
}

#endif	// StTriggerDefinition_h
