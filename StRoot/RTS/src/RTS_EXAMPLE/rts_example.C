#include <stdio.h>
#include <getopt.h>
#include <sys/types.h>

#include <rtsLog.h>

// this needs to be always included
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>

// only the detectors we will use need to be included
#include <DAQ_SC/daq_sc.h>
//#include <DAQ_ESMD/daq_esmd.h>
//#include <DAQ_BTOW/daq_btow.h>

int main(int argc, char *argv[])
{
	extern char *optarg ;
	extern int optind ;
	int c ;

	rtsLogOutput(RTS_LOG_STDERR) ;
	rtsLogLevel(WARN) ;


	while((c = getopt(argc, argv, "d:h")) != EOF) {
		switch(c) {
		case 'd' :
			rtsLogLevel(optarg) ;
			break ;
		default :
			break ;
		}
	}

	class daqReader *evp ;			// tha main guy
	evp = new daqReader(argv[optind]) ;	// create it with the filename argument..

	// create all the detectors we need 
	// and assign them to our "driver" class daqReader
	new daq_sc(evp) ;

	while(evp->get(0,0)) {	// keep getting new events
		daq_dta *dd ;	// generic data pointer; reused all the time


		// get me the "legacy" bank of the "sc" detector
		dd = evp->det("sc")->get("legacy") ;

		// Check to see if the det AND the bank exist in this event...
		// ... and iterate through its data if it does.
		// NOTE: check for "dd != 0" MUST come before the call to dd->iterate(), obviously

		if(dd && dd->iterate()) {	
			sc_t *sc = (sc_t *) dd->Void ;	// cast to the appropriate struct defined in daq_sc.h

			LOG(INFO,"Found SC: field is %f",sc->mag_field) ;	// just an example...
		}





	}

	return 0 ;
}


	
