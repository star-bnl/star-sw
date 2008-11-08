#include <stdio.h>
#include <getopt.h>
#include <sys/types.h>

#include <rtsLog.h>

#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>

// detectors we will need...
#include <DAQ_SC/daq_sc.h>

//#include <DAQ_ESMD/daq_esmd.h>
//#include <DAQ_BTOW/daq_btow.h>

int main(int argc, char *argv[])
{
	extern char *optarg ;
	extern int optind ;
	int c ;

	class daqReader *evp ;

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

	evp = new daqReader(argv[optind]) ;

	// create all the detectors we need 
	// and assign them to our "driver" class daqReader
	new daq_sc(evp) ;

//	new daq_esmd(evp) ;
//	new daq_btow(evp) ;

	while(evp->get(0,0)) {
		daq_dta *dd ;


		dd = evp->det("sc")->get("legacy") ;

		if(dd && dd->iterate()) {	
			sc_t *sc = (sc_t *) dd->Byte ;	// cast the appropriate struct defined in daq_sc.h
			LOG(INFO,"Found SC: field is %f",sc->mag_field) ;
		}

#if 0
		dd = evp->det("esmd")->get("adc") ;
		if(dd && dd->iterate()) {
			LOG(INFO,"Found ESMD: ADC") ;
		}
				
		dd = evp->det("btow")->get("adc") ;
		if(dd && dd->iterate()) {
			LOG(INFO,"Found BTOW: ADC") ;
		}

		dd = evp->det("bsmd")->get("adc") ;
		if(dd && dd->iterate()) {

		};
	
#endif						

	}

	

	return 0 ;
}


	
