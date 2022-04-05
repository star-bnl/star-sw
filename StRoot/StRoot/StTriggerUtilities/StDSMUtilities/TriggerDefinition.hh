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
  unsigned int  onbits;
  unsigned int  offbits;
  unsigned int  onbits0;
  unsigned int  onbits1;
  unsigned int  onbits2;
  unsigned int  onbits3;
  unsigned int  offbits0;
  unsigned int  offbits1;
  unsigned int  offbits2;
  unsigned int  offbits3;
};

#endif	// TRIGGER_DEFINITION_HH
