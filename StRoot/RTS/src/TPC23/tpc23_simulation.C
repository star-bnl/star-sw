#include <stdio.h>
#include <sys/types.h>
#include <stdlib.h>
#include <pthread.h>
#include <sys/mman.h>
#include <getopt.h>
#include <sys/prctl.h>
#include <sys/stat.h>
#include <errno.h>
#include <string.h>

#include <rtsLog.h>	// for my LOG() logging

#include <DAQ_READER/daq_dta_structs.h>
#include <DAQ_TPX/tpxFCF_flags.h>

#include <TPC23/tpx23.h>
#include <TPC23/itpc23.h>


int main(int argc, char *argv[])
{
	const char *det = "itpc" ;	// default
	int events = 10 ;		// default
	int log_level = 0 ;

	const char *fname ;
	tpc23_base *tpc ;
	int c ;

	rtsLogOutput(RTS_LOG_STDERR) ;

	while((c=getopt(argc,argv,"e:D:l:"))!=EOF) {
	switch(c) {
	case 'e' :
		events = atoi(optarg) ;
		break ;
	case 'D' :
		det = optarg ;
		break ;
	case 'l' :
		log_level = atoi(optarg) ;
		break ;
	}
	}

	// choose flavor: only makes a difference to row count and pad extents
	if(strstr(det,"tpx")) {
		tpc = new tpx23 ;
		fname = "/RTS/conf/tpx/tpx_gains.txt.12Sep19.1" ;
	}
	else {
		tpc = new itpc23 ;
		fname = "/RTS/conf/itpc/itpc_gains.txt.11Sep19.1" ;
	}

	tpc->log_level = log_level ;	// keep it at 0 (default) normaly

	// override to NO gain corrections for this test
	fname = "none" ;

	tpc->gains_from_cache(fname) ;	// REQUIRED even if no gain correction


	// Does some initialization: REQUIRED at least once at the start of everyhing
	tpc->run_start() ;

	// prepare some test data: MUST be 512 timebins!
	static short adc[512] ;	
	static int track[512] ;	// note it's an int

	// simple test pattern
	for(int t=100;t<=110;t++) {
		adc[t] = 10 ;
		track[t] = 123 ;
	}


	for(int i=0;i<events;i++) {

		tpc->sim_evt_start() ;	// prepare start of event

		// fill in the data: ADC and Track Id: Just an example
		for(int row=12;row<=13;row++) {
			for(int pad=10;pad<=20;pad++) {
				tpc->sim_do_pad(row,pad,adc,track) ;
			}
		}

		// allocate output storage based upon the count of found sequences
		tpc->s2_max_words = tpc->sequence_cou*2 + 1024 ;
		tpc->s2_start = (u_int *) malloc(tpc->s2_max_words*4) ;
			
		// this actually runs the clusterfinder
		tpc->evt_stop() ;

		
		if(tpc->s2_words) {	// if anything found..
			u_int *p_buff = tpc->s2_start ;
			u_int *end_buff = p_buff + tpc->s2_words ;

			printf("*** Event %d: sequences found %d\n",i+1,tpc->sequence_cou) ;

			while(p_buff < end_buff) {
				u_int row = *p_buff++ ;
				u_int version = *p_buff++ ;
				u_int int_cou = *p_buff++ ;

				int ints_per_cluster = (row>>16) ;
				row &= 0xFFFF ;

				int clusters = int_cou / ints_per_cluster ;

				for(int i=0;i<clusters;i++) {
					daq_sim_cld_x dc ;

					tpc->fcf_decode(p_buff,&dc,version) ;

					// nice flags printout
					char c_flags[128] ;
					c_flags[0] = 0 ;

					if(dc.cld.flags & FCF_ONEPAD) strcat(c_flags,"one+") ;
					if(dc.cld.flags & FCF_MERGED) strcat(c_flags,"merge+") ;
					if(dc.cld.flags & FCF_DEAD_EDGE) strcat(c_flags,"dead+") ;
					if(dc.cld.flags & FCF_ROW_EDGE) strcat(c_flags,"edge+") ;						if(dc.cld.flags & FCF_ONEPAD) strcat(c_flags," one") ;
					if(dc.cld.flags & FCF_BROKEN_EDGE) strcat(c_flags,"small+") ;
					if(dc.cld.flags & FCF_BIG_CHARGE) strcat(c_flags,"charge+") ;

					if(strlen(c_flags)) {
						c_flags[strlen(c_flags)-1] = 0 ;
					}

					printf("row %d: %f %d %d %f %d %d %d 0x%02X[%s]\n",row,
					       dc.cld.pad,dc.cld.p1,dc.cld.p2,
					       dc.cld.tb,dc.cld.t1,dc.cld.t2,
					       dc.cld.charge,
					       dc.cld.flags,c_flags) ;
					printf("   track_id %u, quality %d, pixels %d, max_adc %d\n",
					       dc.reserved[0],
					       dc.quality,
					       dc.pixels,
					       dc.max_adc) ;

					p_buff += ints_per_cluster ;
				}
			}

			// free allocated storage
			free(tpc->s2_start) ;
			tpc->s2_start = 0 ;

			
		}

		// end of event

	}


	tpc->run_stop() ;	// dumps some statistics...

	delete tpc ;		// to be a nice citizen

	return 0 ;
}
