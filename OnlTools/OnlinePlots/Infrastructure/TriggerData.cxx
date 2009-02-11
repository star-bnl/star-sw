#include "TriggerData.h"

#include <stdio.h>

#ifdef NEW_DAQ_READER
#   include "DAQ_READER/daqReader.h"
#   include "DAQ_READER/daq_dta.h"
#   include "DAQ_READER/daq_det.h"
#   include "StDaqLib/TRG/trgStructures2009.h"
#   include "StEvent/StTriggerData2009.h"
#endif

StTriggerData* TriggerData::trgdata=0;
int TriggerData::run_old=0;
int TriggerData::event_old=0;

TriggerData::TriggerData(){}

StTriggerData* TriggerData::Instance(char *datap){
  if(datap == 0) return 0;
  daqReader* daqr = (daqReader*)datap;
  int run = daqr->run;
  int event = daqr->event_number;
  printf("Run=%d  event=%d   %d   %d\n",run,event,run_old,event_old);
  if(trgdata != 0 && run==run_old && event==event_old){    
    return trgdata;
  }else{
    if(trgdata != 0) delete trgdata;
    trgdata = 0;    
    daq_dta *dd= daqr->det("trg")->get("raw");
    if (dd && dd->iterate()) {
      char* td = (char*)dd->Void;
      printf("TRG RAW: version = _%02x_%02x_%02x_%02x_\n",td[0],td[1],td[2],td[3]);
      if(td[3] == 0x40){
	TriggerDataBlk2009* trgdata2009 = (TriggerDataBlk2009*)td;
	StTriggerData2009* trgd = new StTriggerData2009(trgdata2009,run);
	trgdata = (StTriggerData*)trgd;
	run_old = run;
	event_old = event;
      }else{	
	fprintf(stderr,"TRG RAW: version missmatch, skipping trigger data\n");
      }
    }else{	
      fprintf(stderr,"TRG RAW: trigger data doesn't exist\n");
    }
  }
  return trgdata;
}
