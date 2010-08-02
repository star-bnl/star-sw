// -*- mode:c++ -*-

#ifndef StTriggerDefinition_h
#define StTriggerDefinition_h

#include <cstdio>
#include "TObject.h"
#include "TString.h"

struct StTriggerDefinition : public TObject {
  int     triggerIndex;		// trigger index
  TString name;			// trigger name
  int     triggerId;		// trigger ID
  int     onbits;	        // bits required to be ON at input of TCU
  int     offbits;	        // bits required to be OFF at input of TCU
  int     onbits1;
  int     onbits2;
  int     onbits3;
  int     offbits1;
  int     offbits2;
  int     offbits3;

  void print();

  ClassDef(StTriggerDefinition,1)
};

void StTriggerDefinition::print()
{
  printf("triggerIndex=%d name=%s triggerId=%d onbits=0x%04x offbits=0x%04x\n",
	 triggerIndex,name.Data(),triggerId,onbits,offbits);
}

#endif	// StTriggerDefinition_h
