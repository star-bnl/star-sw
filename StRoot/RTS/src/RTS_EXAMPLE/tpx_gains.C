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


	char do_usage ;
	char do_summary ;
	char do_compare ;
	int do_sector = -1 ;
	int do_output ;

	int num_evts = 0x7FFFFFFF ;	// a lot...

	rtsLogOutput(RTS_LOG_STDERR) ;
	rtsLogLevel(WARN) ;

	do_output = do_compare = do_usage = do_summary = 0 ;


	daq_dta *dta ;	// daq_dta class grabs the data out of a DET bank...


	while((c = getopt(argc, argv,"S:d:scon:")) != EOF) {
		switch(c) {
		case 'd' :
			rtsLogLevel(optarg) ;
			break ;
		case 's' :
			do_summary = 1 ;
			break ;
		case 'c' :
			do_compare = 1 ;
			break ;
		case 'o' :
			do_output = 1 ;
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
	
	if(do_usage || (optind >= argc)) {
		fprintf(stderr,"Usage: %s [-d loglevel] [-c] [-n evts] [-S sector] [-s] file\n",argv[0]) ;
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


	memset(tpx_gains,0,sizeof(tpx_gains)) ;

	while(r->get(0,0)) {
		for(int s=s_start;s<=s_stop;s++) {
			dta = r->det("tpx")->get("raw",s) ;

			while(dta && dta->iterate()) {

				if((tpx_gains[s] == 0)) {	// create if it didn't exist already...
					tpx_gains[s] = new tpxGain ;
					tpx_gains[s]->init(s) ;
				}


				tpx_gains[s]->accum((char *)dta->Void, dta->ncontent) ;


			}

			if(tpx_gains[s]) tpx_gains[s]->ev_done() ;
		}

		num_evts-- ;
		if(num_evts <= 0) break ;
	}

	for(int s=s_start;s<=s_stop;s++) {


		if(!tpx_gains[s]) continue ;

		tpx_gains[s]->calc() ;	// do the calculation of the means, gains etc.

		if(do_summary) {	// dump extended summary
			char fname[80] ;
			sprintf(fname,"/tmp/tpx_gains_%02d.sum",s) ;
			int really_bad = tpx_gains[s]->summarize(fname, 0) ;		// dump the extended results to this file
			LOG(INFO,"Sector %d: really bad pads %d",s,really_bad) ;	// not counting the edge pads...
		}

		if(do_compare) {	// compare to canonical
			// summarize MUST happen before!
			tpx_gains[s]->summarize(0, 0) ;		
			tpx_gains[s]->compare("/RTS/conf/tpx/tpx_gains.txt",s) ;
		}


		if(do_output) {		// dump the calculated gains in the canonical form to stdout
			// summarize MUST happen before!
			tpx_gains[s]->summarize(0, 0) ;
			tpx_gains[s]->to_file("stdout") ;

		}
	}


	return 0 ;
}
