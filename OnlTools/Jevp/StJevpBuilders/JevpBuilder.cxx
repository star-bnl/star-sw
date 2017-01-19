#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include <RTS/include/rtsLog.h>
#include <rtsSystems.h>
#include "DAQ_READER/daqReader.h"
#include <DAQ_READER/daq_dta.h>
#include <DAQ_READER/daq_det.h>
#include "StEvent/StTriggerData2009.h"
#include "StEvent/StTriggerData2012.h"
#include "StEvent/StTriggerData2013.h"
#include "StEvent/StTriggerData2016.h"
#include "StEvent/StTriggerData2017.h"
//#include "Jevp/StJevpPlot/RunStatus.h"

// Provides the data interface to the builders...



ClassImp(JevpBuilder);
  
// Helper for getting data
StTriggerData *JevpBuilder::getStTriggerData(daqReader *rdr)
{
    if(!rdr) {
	LOG(ERR, "No reader?");
	return NULL;
    }

    StTriggerData *trgd = NULL;
    int run = rdr->run;
  
    daq_dta *dd = rdr->det("trg")->get("raw");
    if(dd && dd->iterate()) {
	char *td = (char *)dd->Void;
	
	if(!td) {
	    LOG(ERR, "td does not exist: evt=%d", rdr->seq);
	    return NULL;
	}
	
	if(td[3] == 0x40) {
	    TriggerDataBlk2009 *trgdatablock2009 = (TriggerDataBlk2009 *)td;
	    StTriggerData2009 *trgd2009 = new StTriggerData2009(trgdatablock2009, run);
	    trgd = (StTriggerData *)trgd2009;
	}
	else if(td[3] == 0x41) {
	    TriggerDataBlk2012 *trgdatablock2012 = (TriggerDataBlk2012 *)td;
	    StTriggerData2012 *trgd2012 = new StTriggerData2012(trgdatablock2012, run);
	    trgd = (StTriggerData *)trgd2012;
	}
	else if(td[3] == 0x42) {
	    TriggerDataBlk2013 *trgdatablock2013 = (TriggerDataBlk2013 *)td;
	    StTriggerData2013 *trgd2013 = new StTriggerData2013(trgdatablock2013, run);
	    trgd = (StTriggerData *)trgd2013;
	}
	else if(td[3] == 0x43) {
	    TriggerDataBlk2016 *trgdatablock2016 = (TriggerDataBlk2016 *)td;
	    StTriggerData2016 *trgd2016 = new StTriggerData2016(trgdatablock2016, run);
	    trgd = (StTriggerData *)trgd2016;	
	}
	else if(td[3] == 0x44) {
	    TriggerDataBlk2017 *trgdatablock2017 = (TriggerDataBlk2017 *)td;
	    StTriggerData2017 *trgd2017 = new StTriggerData2017(trgdatablock2017, run);

	    trgd = (StTriggerData *)trgd2017;
	}
	else {
	    LOG("ERR", "TRG RAW: version mismatch 0x%2x-0x%2x-0x%2x-0x%2x.  Add case statement for new data!", td[0], td[1], td[2], td[3]);
	
	    trgd = (StTriggerData *)NULL;
	}

	return trgd;
    }


    LOG(DBG, "No trigger data exists: evt=%d tkn=%d", rdr->seq, rdr->token);
    return NULL;
}
