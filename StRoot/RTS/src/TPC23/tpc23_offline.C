#include <stdio.h>
#include <sys/types.h>
#include <stdlib.h>
#include <pthread.h>
#include <sys/mman.h>
#include <getopt.h>
#include <sys/prctl.h>
#include <sys/stat.h>


#include <rtsLog.h>

#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>
#include <DAQ_READER/daq_det.h>

#include <DAQ_ITPC/daq_itpc.h>
#include <DAQ_TPX/daq_tpx.h>

#include <TPC23/tpx23.h>
#include <TPC23/itpc23.h>

static tpc23_base *tpc_instance[24] ;	// reserve a bunch

static u_int events ;
static u_int events_max ;


int main(int argc, char *argv[])
{
	const char *det = "tpx" ;
	events_max = 1 ;
	events = 0 ;
	int sector_only = 0 ;
	u_int sector_mask = 0 ;
	int rdo_only = 0 ;
	int rb_mask = 0 ;
	int c ;

	rtsLogOutput(RTS_LOG_STDERR) ;

	class daq_itpc *daq_itpc = new class daq_itpc ;
	class daq_tpx *daq_tpx = new class daq_tpx ;

	while((c=getopt(argc,argv,"e:D:S:R:r:"))!=EOF) {
	switch(c) {
	case 'e' :
		events_max = atoi(optarg) ;
		break ;
	case 'D' :
		det = optarg ;
		break ;
	case 'S' :
		sector_only = atoi(optarg) ;
		break ;
	case 'R' :
		rdo_only = atoi(optarg) ;
		rb_mask = (1<<(rdo_only-1)) ;
		break ;
	case 'r' :
		sscanf(optarg,"0x%X",&rb_mask) ;
		break ;
	}
	}
	
	// 24 sectors!
	for(int i=0;i<24;i++) {
		if(sector_only) {
			if(sector_only==(i+1)) ;
			else continue ;
		}

		if(strstr(det,"tpx")) {
			tpc_instance[i] = new tpx23 ;
		}
		else {
			tpc_instance[i] = new itpc23 ;
		}
		
		tpc_instance[i]->id = i ;		// make it a sector
		tpc_instance[i]->sector1 = i+1 ;	// this is important!
	}

	// load gains from file or at least initialize them correctly
	tpc_instance[0]->gains_from_cache() ;

	daqReader *rdr = 0 ;

	if(sector_only) {
		tpc_instance[0]->load_replay("st_physics_adc_20192001_raw_5500002.daq",sector_only) ;
	}
	else {
		rdr = new daqReader((char *)argv[optind]) ;
	}

	// important! emulate run_start
	for(int i=0;i<24;i++) {
		if(sector_only) {
			if(sector_only==(i+1)) ;
			else continue ;
		}

		sector_mask |= (1<<i) ;
		tpc_instance[i]->run_start() ;
	}	


	LOG(INFO,"Starting: sector_mask 0x%06X, rb_mask 0x%02X",sector_mask,rb_mask) ;

	for(;;) {
		int got_any = 0 ;

		if(rdr->get(0,EVP_TYPE_ANY)) ;
		else {
			break ;
		}

		for(int i=0;i<24;i++) {
			int sector = i+1 ;
			int got_sec = 0 ;

			if(sector_only) {
				if(sector_only==(i+1)) ;
				else continue ;
			}


			daq_dta *dd = rdr->det(det)->get("raw",sector) ;
		
			if(dd) {	// got a sector
				LOG(TERR,"EVT %d: got sector %d",events,sector) ;

				tpc_instance[i]->evt_start() ;
				got_sec = 1 ;
				got_any = 1 ;
			}

			while(dd && dd->iterate()) {
				if(rb_mask) {
					if(rb_mask & (1<<(dd->rdo-1))) ;
					else continue ;
				}

				//if(rdo_only) {
				//	if(rdo_only==dd->rdo) ;
				//	else continue ;
				//}

				tpc_instance[i]->sector1 = dd->sec ;
				tpc_instance[i]->rdo1 = dd->rdo ;

				LOG(TERR,"EVT %d: S%02d:%d: %d bytes",events,dd->sec,dd->rdo,dd->ncontent) ;

				struct daq_trg_word trg_d[64] ;
				int trg_cou ;

				if(strstr(det,"itpc")) {
					trg_cou = daq_itpc->get_l2((char *)dd->Void,dd->ncontent/4,trg_d,dd->rdo) ;
				}
				else {
					trg_cou = daq_tpx->get_l2((char *)dd->Void,dd->ncontent/4,trg_d,dd->rdo) ;
				}

				LOG(TERR," get_l2: trg_cou %d: T %d(%d,%d)",trg_cou,trg_d[0].t,trg_d[0].trg,trg_d[0].daq) ;

				int n_words = tpc_instance[i]->from22to23((char *)dd->Void,dd->ncontent/4) ;



				u_int tmp = tpc_instance[i]->get_token_s((char *)dd->Void,n_words) ;
				int t = tmp&0xFFF ;
				int trg = (tmp>>16)&0xF ;
				int daq = (tmp>>12)&0xF ;

				LOG(TERR,"  get_token: T%d(%d,%d)",t,trg,daq) ;

				tpc_instance[i]->rdo_scan((char *)dd->Void,n_words) ;
			}	

			if(got_sec) {
				tpc_instance[i]->evt_stop() ;
			}
		}

		if(got_any) {
			events++ ;
			if(events==events_max) break ;
		}
	}

	// dump statistics, etc.
	for(int i=0;i<24;i++) {
		if(sector_only) {
			if(sector_only==(i+1)) ;
			else continue ;
		}

		tpc_instance[i]->run_stop() ;
	}

	LOG(INFO,"Done.") ;
	return 0 ;
}

