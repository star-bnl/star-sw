#include <sys/types.h>
#include <getopt.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <rtsLog.h>


#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>
#include <DAQ_TPX/daq_tpx.h>
#include <DAQ_TPX/tpxGain.h>



int main(int argc, char *argv[])
{
	extern char *optarg ;
	extern int optind ;
	int c ;
	daqReader *r ;
	tpxGain *tpx_gains[25] ;

	char do_print ;
	char do_check ;
	char do_usage ;
	char do_summary ;
	char do_help ;
	int do_sector = -1 ;

	u_int num_evts = 0xFFFFFFFF ;	// a lot...

	rtsLogOutput(RTS_LOG_STDERR) ;
	rtsLogLevel(WARN) ;

	do_help = do_print = do_check = do_usage = do_summary = 0 ;


	daq_dta *dta ;	// daq_dta class grabs the data out of a DET bank...


	while((c = getopt(argc, argv,"S:d:sn:")) != EOF) {
		switch(c) {
		case 'd' :
			rtsLogLevel(optarg) ;
			break ;
		case 's' :
			do_summary = 1 ;
			break ;
		case 'n' :
			num_evts = atoi(optarg) ;
			break ;
		case 'S' :
			do_sector = atoi(optarg) ;
			break ;
		default :
			do_usage = 1 ;
			break ;
		}
	}


	if(do_usage) {
		fprintf(stderr,"Usage: %s [-d loglevel] [-p (to print on stdout)] [-c (to do checking)] files...\n",argv[0]) ;
		return -1 ;
	}

	

	// we need a filename here:
	if(optind >= argc) {
		fprintf(stderr,"Usage: %s [-d loglevel] [-p (to print on stdout)] [-c (to do checking)] files...\n",argv[0]) ;
		return -1 ;
	}


	int s_start, s_stop ;
	if(do_sector <= 0) {	
		s_start = 1 ;
		s_stop = 24 ;
	}
	else {
		s_start = s_stop = do_sector ;
	}

	// pick up the filenames
	r = new daqReader(argv[optind]) ;

	new daq_tpx(r) ;

	memset(tpx_gains,0,sizeof(tpx_gains)) ;

	while(r->get(0,0)) {
		for(int s=s_start;s<=s_stop;s++) {
			dta = r->det("tpx")->get("raw",s) ;

			while(dta && dta->iterate()) {

				if((tpx_gains[s] == 0)) {
					tpx_gains[s] = new tpxGain ;
					tpx_gains[s]->init(s) ;
				}


				//printf("sec %d, rdo %d: %d bytes\n",dta->sec,dta->rdo,dta->ncontent) ;

				tpx_gains[s]->accum((char *)dta->Void, dta->ncontent) ;


			}

			if(tpx_gains[s]) tpx_gains[s]->ev_done() ;
		}
	}

	for(int s=s_start;s<=s_stop;s++) {
		char fname[80] ;

		if(!tpx_gains[s]) continue ;

		tpx_gains[s]->calc() ;

		sprintf(fname,"/tmp/tpx_gains_%02d.sum",s) ;
		tpx_gains[s]->summarize(fname, 0) ;
	}


	return 0 ;
}
