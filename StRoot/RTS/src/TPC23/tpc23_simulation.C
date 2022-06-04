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

#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>
#include <DAQ_READER/daq_det.h>
#include <DAQ_READER/daq_dta_structs.h>
#include <DAQ_TPX/tpxFCF_flags.h>

#include <TPC23/tpx23.h>
#include <TPC23/itpc23.h>


static struct rp_dta_t {
	short adc[512] ;
} rp_dta[46][183] ;

int main(int argc, char *argv[])
{
	int events = 10 ;		// default
	int log_level = 0 ;

	const char *tpx_fname ;
	const char *itpc_fname ;
	tpc23_base *tpc[2] ;
	int c ;

	rtsLogOutput(RTS_LOG_STDERR) ;

	while((c=getopt(argc,argv,"e:l:"))!=EOF) {
	switch(c) {
	case 'e' :
		events = atoi(optarg) ;
		break ;
	case 'l' :
		log_level = atoi(optarg) ;
		break ;
	}
	}

	// choose flavor: only makes a difference to row count and pad extents
	tpc[0] = new tpx23 ;
	tpx_fname = "/RTS/conf/tpx/tpx_gains.txt.12Sep19.1" ;

	tpc[1] = new itpc23 ;
	itpc_fname = "/RTS/conf/itpc/itpc_gains.txt.11Sep19.1" ;


	tpc[0]->log_level = log_level ;	// keep it at 0 (default) normaly
	tpc[1]->log_level = log_level ;	// keep it at 0 (default) normaly

	// override to NO gain corrections for this test
	// tpx_fname = "none" ;
	tpc[0]->gains_from_cache(tpx_fname) ;	// REQUIRED even if no gain correction


	// itpc_fname = "none" ;
	tpc[1]->gains_from_cache(itpc_fname) ;	// REQUIRED even if no gain correction

	// Does some initialization: REQUIRED at least once at the start of everyhing
	tpc[0]->run_start() ;
	tpc[1]->run_start() ;


	// prepare some test data: MUST be 512 timebins!
	static short adc[512] ;	
	static int track[512] ;	// note it's an int

	// simple test pattern
	for(int t=100;t<=110;t++) {
		adc[t] = 10 ;
		track[t] = 123 ;
	}



	restart:;

	while(optind<argc) {	// loop over files


	LOG(INFO,"Using file %s [%d/%d]",argv[optind],optind,argc) ;


	daqReader *rdr = new daqReader((char *)argv[optind]) ;


	for(int i=0;i<events;i++) {	// loop over events in the file

	if(rdr->get(0,EVP_TYPE_ANY)==0) {	// no more events...
		optind++ ;
		delete rdr ;
		goto restart ;			// end-of-file, try next file
	}



	for(int det=0;det<2;det++) {	// loop over TPX and then iTPC
	for(int sector=1;sector<=24;sector++) {
		daq_dta *dd ;

		if(det==0) {	// TPX
			dd = rdr->det("tpx")->get("adc",sector) ;
		}
		else {	// ITPC
			dd = rdr->det("itpc")->get("adc",sector) ;
		}

		if(dd==0) continue ;




		// load data from DAQ file into sector structure
		int pad_max = 0 ;	// for simplicity...

		memset(rp_dta,0,sizeof(rp_dta)) ;	// clear it all

		while(dd->iterate()) {
			if(dd->pad>pad_max) pad_max = dd->pad ;	// for later use

			if(dd->row<0 || dd->row>45) LOG(ERR,"row") ;	// sanity
			if(dd->pad<0 || dd->pad>182) LOG(ERR,"pad") ;	// sanity

			//if(log_level>=2 && dd->ncontent) LOG(TERR,"sector %d, row %d, pad %d, content %d",sector,dd->row,dd->pad,dd->ncontent) ;

			for(u_int i=0;i<dd->ncontent;i++) {
				int tb = dd->adc[i].tb ;

				if(tb<0 || tb>=512) LOG(ERR,"tb") ;	// sanity

				rp_dta[dd->row][dd->pad].adc[tb] = dd->adc[i].adc ;

			}
		}


		// issue start of simulation
		tpc[det]->sim_evt_start(sector) ;	// "start" event when changing the sector...

		// and run the simulation pad by pad
		for(int row=1;row<=45;row++) {	// just assume all rows...
			for(int pad=1;pad<=pad_max;pad++) {
				// input to the simulation: NOTE that track id is nonsense in my example!!!
				tpc[det]->sim_do_pad(row,pad,rp_dta[row][pad].adc,track) ;
			}
		}


		//LOG(TERR,"evt %d: det %s, sector %d: pad_max %d, sequences %d",i,det?"ITPC":"TPX",sector,pad_max,tpc[det]->sequence_cou) ;

		// allocate output storage based upon the count of found sequences
		const int words_per_cluster = 5 ;	// 5 for simulation, 2 normally

		tpc[det]->s2_max_words = tpc[det]->sequence_cou*words_per_cluster + 2000 ;	// and add a bit more...
		tpc[det]->s2_start = (u_int *) malloc(tpc[det]->s2_max_words*4) ;
			
		// this actually runs the clusterfinder; s2_start MUST have been allocated
		tpc[det]->evt_stop() ;

		
		if(tpc[det]->s2_words) {	// if anything found..
			u_int *p_buff = tpc[det]->s2_start ;
			u_int *end_buff = p_buff + tpc[det]->s2_words ;


			if(tpc[det]->s2_words >= tpc[det]->s2_max_words) {
				LOG(ERR,"Whoa -- lots of words %d/%d: event %d, det %s, sector %d",tpc[det]->s2_words,tpc[det]->s2_max_words,
				    i,det?"ITPC":"TPX",sector) ;
			}


//			printf("*** Event %d: det %s, sector %d: sequences found %d, words used %d/%d\n",i+1,
//			       det?"ITPC":"TPX",sector,tpc[det]->sequence_cou,tpc[det]->s2_words,tpc[det]->s2_max_words) ;

			while(p_buff < end_buff) {
				u_int row ;
				u_int version ;
				u_int int_cou ;
				int ints_per_cluster ;

				// TPX and iTPC have slightly different formats; maintained compatibility
				if(det==0) {	// TPX
					row = *p_buff++ ;
					int_cou = *p_buff++ ;

					version = (row>>16) ;

					ints_per_cluster = 5 ;	// 5 for sim, 2 for real
				}
				else {	
					row = *p_buff++ ;
					version = *p_buff++ ;
					int_cou = *p_buff++ ;

					ints_per_cluster = (row>>16) ;
					
				}

				//printf("... 0x%X 0x%X; version 0x%X\n",row,int_cou,version) ;

				row &= 0xFFFF ;

				int clusters = int_cou / ints_per_cluster ;

				for(int i=0;i<clusters;i++) {
					daq_sim_cld_x dc ;

					tpc[det]->fcf_decode(p_buff,&dc,version) ;

					
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

					//printf("row %d: %f %d %d %f %d %d %d 0x%02X[%s]\n",row,
					//       dc.cld.pad,dc.cld.p1,dc.cld.p2,
					//       dc.cld.tb,dc.cld.t1,dc.cld.t2,
					//       dc.cld.charge,
					//       dc.cld.flags,c_flags) ;
					//printf("   track_id %u, quality %d, pixels %d, max_adc %d\n",
					//       dc.reserved[0],
					//       dc.quality,
					//       dc.pixels,
					//       dc.max_adc) ;

					p_buff += ints_per_cluster ;
				}
			}

			// free allocated storage
			free(tpc[det]->s2_start) ;
			tpc[det]->s2_start = 0 ;

			
		}
	}	// sector loop
	}	// detector loop
	}	// event loop

	delete rdr ;
	optind++ ;

	}	// DAQ file loop

	tpc[0]->run_stop() ;	// dumps some statistics...
	tpc[1]->run_stop() ;	// dumps some statistics...

	delete tpc[0] ;		// to be a nice citizen
	delete tpc[1] ;		// to be a nice citizen

	return 0 ;
}
