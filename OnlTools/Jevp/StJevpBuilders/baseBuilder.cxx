#include <stdio.h>
#include <stdlib.h>

#include "Jevp/StJevpPlot/JevpPlotSet.h"
#include "DAQ_READER/daqReader.h"
#include "Jevp/StJevpPlot/RunStatus.h"

#include <TH1I.h>
#include <TH2F.h>

#include <math.h>
#include "baseBuilder.h"
#include <RTS/include/rtsLog.h>

// This is the one PlotSet that is guarenteed to always exist
// It's main purpose is to provide the run information 
// To the server...
//
// It has no plots (currently)
//


ClassImp(baseBuilder);
  
void baseBuilder::initialize(int argc, char *argv[]) {
  LOG(DBG, "Initialize");
  run = 0;
  //  status.setNumPlotSets(1);

  for(int i=1;i<argc;i++) {
//     if(memcmp(argv[i],"-numplotsets", 11) == 0) {
//       i++;
//       status.setNumPlotSets(atoi(argv[i+1]));
//     }
  }
}
  
void baseBuilder::startrun(daqReader *rdr) {
  LOG(DBG, "startrun #%d",rdr->run);
//   run = rdr->run;
//   LOG(NOTE, "starting");
//   status.setRunNumber(rdr->run);
//   LOG(NOTE, "again");

//   status.setEndOfRun(0);        // What are the values?
//   LOG(NOTE, "again");
//   status.setTime(rdr->evt_time); 
//   LOG(NOTE, "again");
//   status.setTriggerBitsRun(rdr->evpgroupsinrun);   // which triggers are in the run?
//   LOG(NOTE, "again");
//   status.setDetectorBitsRun(rdr->detsinrun);  // which detectors are in the run?
//   LOG(NOTE, "again");
  
//   send((TObject *)&status);
 
//   LOG(NOTE, "again");
}

void baseBuilder::stoprun(daqReader *rdr) {
  LOG(DBG, "stoprun #%d",run);
//   printf("Stopping run #%d\n",run);
//   status.setEndOfRun(1);
//   send((TObject *)&status);
}

void baseBuilder::event(daqReader *rdr) {
  LOG(DBG, "event #%d",rdr->seq);
}

void baseBuilder::main(int argc, char *argv[])
{
  baseBuilder me;
  
  me.Main(argc, argv);
}

