#include <stdio.h>
#include <stdlib.h>

#include "DAQ_READER/daqReader.h"
#include "Jevp/StJevpPlot/RunStatus.h"

#include <TH1I.h>
#include <TH2F.h>

#include <math.h>
#include "baseBuilder.h"
#include <RTS/include/rtsLog.h>
#include <rtsSystems.h>
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

  printf("base:  Confdatadir = %s\n",confdatadir);

  
}
  
void baseBuilder::startrun(daqReader *rdr) {
  LOG("JEFF", "startrun #%d",rdr->run);
  
  first_event = 0;
}

void baseBuilder::stoprun(daqReader *rdr) {
  LOG(DBG, "stoprun #%d",run);
}

void baseBuilder::event(daqReader *rdr) {
  LOG(DBG, "event #%d",rdr->seq);
  char tmp[20];

  if(first_event == 0) {
    first_event = 1;

    // Set tags for detectors in run...
    int mask = rdr->detsinrun;
    //LOG("JEFF", "Detector mask = 0x%x",mask);
    for(int i=0;i<32;i++) {
      if(mask & 1<<i) {
	char *det = (char *)rts2name(i);
	if(!det) det = (char *)"xxx";

	strcpy(tmp, det);
	det = tmp;
	while(*det) {
	  *det = tolower(*det);
	  det++;
	}
       
	char tmp2[20];
	sprintf(tmp2, "|%s|",tmp);
	addServerTags(tmp2);
    
	LOG("JEFF", "adding server tag: det[%d] = %s",i, tmp2);
      }
    }
  }

}

void baseBuilder::main(int argc, char *argv[])
{
  baseBuilder me;
  
  me.Main(argc, argv);
}

