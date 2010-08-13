//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 19 Feb 2010
//

#ifndef TRIGGER_DEFINITION_HH
#define TRIGGER_DEFINITION_HH

struct TriggerDefinition {
  int  triggerIndex;
  char name[64];
  int  triggerId;
  int  onbits;
  int  offbits;
  int  onbits1;
  int  onbits2;
  int  onbits3;
  int  offbits1;
  int  offbits2;
  int  offbits3;
};

#endif	// TRIGGER_DEFINITION_HH
