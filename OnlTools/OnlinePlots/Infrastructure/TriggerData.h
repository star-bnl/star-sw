#ifndef TriggerData_h
#define TrigegrData_h

#include "StEvent/StTriggerData.h"
#ifdef NEW_DAQ_READER
  class daqReader;
  typedef daqReader evpReader;
  typedef unsigned int UINT32;
#else
  class evpReader   ; // new  2007 DAQ file reader
#endif

class TriggerData{
public:
  static StTriggerData* Instance(char *datap);

protected:
  TriggerData();

private:
  static StTriggerData* trgdata;
  static int run_old;
  static int event_old;

};

#endif
