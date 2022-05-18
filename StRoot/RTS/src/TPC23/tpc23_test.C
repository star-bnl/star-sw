#include <stdio.h>
#include <sys/types.h>
#include <stdlib.h>
#include <pthread.h>
#include <sys/mman.h>
#include <getopt.h>
#include <sys/prctl.h>
#include <sys/stat.h>
#include <errno.h>

#include <rtsLog.h>
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>
#include <DAQ_READER/daq_det.h>

#include <DAQ_ITPC/itpcFCF.h>
#include <DAQ_TPX/tpxFCF_flags.h>

//#include <DAQ_TPX/daq_tpx.h>
//#include <DAQ_ITPC/daq_itpc.h>



#include <DDB/ddb.h>

#include "support.h"
#include "tpx23.h"
#include "itpc23.h"


t_ave ave("main") ;


struct ddb_t ddb ;

static tpc23_base *tpc ;

int main(int argc, char *argv[])
{
	extern char *optarg ;
	extern int optind ;

	const char *det = "itpc" ;
	int events_max = 64 ;
	int c ;
	int real_rdo[4] ;
	int real_sec[4] ;
	int use_rdr = 1 ;	// default YES
	int s_start, s_stop ;	
	daqReader *rdr = 0 ;
	int fmt_version ;
	int log_level ;

	rtsLogOutput(RTS_LOG_STDERR) ;


	memset(&ddb,0,sizeof(ddb)) ;

	fmt_version = 0 ;
	log_level = 0 ;

	ddb.run_type = 3 ;
	ddb.rb_mask = 0x4 ;
	ddb.sector = 1 ;

	while((c=getopt(argc,argv,"e:D:S:r:R:E3l:"))!=EOF) {
	switch(c) {
	case 'e' :
		events_max = atoi(optarg) ;
		break ;
	case 'D' :
		det = optarg ;
		break ;
	case 'S' :
		ddb.sector = atoi(optarg) ;
		break ;
	case 'r' :
		sscanf(optarg,"0x%X",&ddb.rb_mask) ;
		break ;
	case 'R' :
		ddb.rb_mask = (1<<(atoi(optarg)-1)) ;
		break ;
	case 'E' :
		use_rdr = 0 ;
		break ;
	case '3' :
		fmt_version = 23 ;
		break ;
	case 'l' :
		log_level = atoi(optarg) ;
		break ;

	}
	}
	

	if(strstr(det,"tpx")) {
		ddb.rts_id = TPX_ID ;
		tpc = new tpx23 ;


		if(ddb.sector<=24) {
			real_rdo[0] = 3 ;
			real_rdo[1] = 4 ;
			real_rdo[2] = 5 ;
			real_rdo[3] = 6 ;

			for(int i=0;i<4;i++) real_sec[i] = ddb.sector ;

			ddb.rb_mask &= 0xF ;
		}
		else {
			int sec = (ddb.sector-25)*2+1 ;
			real_rdo[0] = 5 ;
			real_rdo[1] = 6 ;
			real_sec[0] = sec ;
			real_sec[1] = sec ;

			real_rdo[2] = 5 ;
			real_rdo[3] = 6 ;
			real_sec[2] = sec+1 ;
			real_sec[3] = sec+1 ;

			ddb.rb_mask &= 0xF ;
		}
	}
	else {
		ddb.rts_id = ITPC_ID ;
		tpc = new itpc23 ;



		for(int i=0;i<4;i++) real_rdo[i] = i+1 ;
		for(int i=0;i<4;i++) real_sec[i] = ddb.sector ;

		ddb.rb_mask &= 0xF ;
	}

	LOG(INFO,"Starting: %s, sector %2d, rb_mask 0x%X, fmt_version %d, log_level %d",det,ddb.sector,ddb.rb_mask,fmt_version,log_level) ;

	tpc->run_type = 3 ;
	tpc->log_level = log_level ;

	if(ddb.rts_id==ITPC_ID) {
		tpc->gains_from_cache("/RTS/conf/itpc/itpc_gains.txt.11Sep19.1") ;
	}
	else {
		tpc->gains_from_cache("/RTS/conf/tpx/tpx_gains.txt.12Sep19.1") ;
	}
	
	if(use_rdr==0) {
		const char *fname ;

		if(optind >= argc) {
			fname = "/RTS/TONKO/data/st_physics_adc_20192001_raw_5500002.daq" ;
		}
		else {
			fname = argv[optind] ;
		}

		LOG(INFO,"Using FIFO replay file %s",fname) ;

		tpc->load_replay(fname,ddb.sector) ;
		s_start = 1 ;	// not really THE sector but anyway...
		s_stop = 1 ;
	}
	else {
		const char *fname ;

		if(optind >=argc) {
			fname = "/RTS/TONKO/data/st_physics_adc_20192001_raw_5500002.daq" ;
		}
		else {
			fname = argv[optind] ;
		}

		LOG(INFO,"Using DAQ Reader file %s",fname) ;

		rdr = new daqReader((char *)fname) ;

		if(ddb.sector) {
			s_start = s_stop = ddb.sector ;
		}
		else {
			s_start = 1 ;	// all sectors...
			s_stop = 24 ;
		}
	}


				   

	// Run Start
	tpc->run_start() ;

	for(int i=0;i<events_max;i++) {
		int fifo = i%64 ;

		if(rdr) {
			char *c_ret = rdr->get(0,EVP_TYPE_ANY) ;
			if(c_ret==0) goto run_done ;	
			
			fifo = 0 ;

			if(rdr->token>=4096) continue ;
		}



		for(int sec=s_start;sec<=s_stop;sec++) {
			if(tpc->log_level>=1) LOG(INFO,"EVT %d: evt_start: fifo %d",i,fifo) ;

			tpc->evt_start() ;

			for(int r=0;r<6;r++) {
			
				if(ddb.rb_mask & (1<<r)) ;
				else continue ;

				if(rdr) {
					
					daq_dta *dd = rdr->det(det)->get("raw",sec,real_rdo[r]) ;
					

					if(dd==0) continue ;


					if(dd->iterate()==0) continue ;

					LOG(INFO,"Evt %d: in rdr data: SR%02d:%d: T %d, bytes %d",i,dd->sec,dd->rdo,rdr->token,dd->ncontent) ;


					tpc->sector1 = dd->sec ;
					tpc->rdo1 = dd->rdo ;

					if(fmt_version < 23) tpc->from22to23((char *)dd->Void,dd->ncontent/4) ;

					tpc->sim_dta[0].rb[r].mem = (char *)dd->Void ;
					tpc->sim_dta[0].rb[r].bytes = dd->ncontent ;

					real_sec[r] = dd->sec ;
					real_rdo[r] = dd->rdo ;
				}

				tpc->set_rdo(real_sec[r],real_rdo[r]) ;

				char *c_addr = tpc->sim_dta[fifo].rb[r].mem ;
				int words = tpc->sim_dta[fifo].rb[r].bytes/4 ;


				if(tpc->log_level>=1) LOG(INFO,"EVT %d, RB %d (SR %d:%d): words %d",i,r,tpc->sector1,tpc->rdo1,words) ;

				u_int t_tmp = tpc->get_token_s(c_addr,words) ;
				int t_t = t_tmp & 0xFFF ;
				int t_daq = (t_tmp>>12)&0xF ;
				int t_trg = (t_tmp>>16)&0xF ;

				if(tpc->log_level>=1) LOG(TERR,"EVT %d: fifo %d: T %d(%d,%d): words %d",i,fifo,t_t,t_trg,t_daq,words) ;

				tpc->rdo_scan(c_addr,words) ;

				if(rdr) {	// I need to nix it since I manipulated it
					tpc->sim_dta[0].rb[r].mem = 0 ;
				}
			}

			
			// allocate storage here based upon somehing from stage0 (rdo_scan)?
			tpc->s2_max_words = tpc->sequence_cou*2 + 1024 ;
			tpc->s2_start = (u_int *) malloc(tpc->s2_max_words*4) ;
			
			int ret = tpc->evt_stop() ;


			LOG(TERR,"EVT %d: sequences %d, words %d, s2_words %d",i,tpc->sequence_cou,ret,tpc->s2_words) ;
			printf("sequence %d, words %d, s2_words %d\n",tpc->sequence_cou,ret,tpc->s2_words) ;

			if(tpc->s2_start) {
				itpc_fcf_c *fcf ;

				u_int *p_buff = tpc->s2_start ;
				u_int *end_buff = p_buff + tpc->s2_words ;


				while(p_buff < end_buff) {
					u_int row = *p_buff++ ;
					u_int version = *p_buff++ ;
					u_int int_cou = *p_buff++ ;

					int ints_per_cluster = (row>>16) ;
					row &= 0xFFFF ;

					int clusters = int_cou / ints_per_cluster ;

					for(int i=0;i<clusters;i++) {
						daq_cld dc ;

						fcf->fcf_decode(p_buff,&dc,version) ;

						char c_flags[128] ;
						c_flags[0] = 0 ;

						if(dc.flags & FCF_ONEPAD) strcat(c_flags,"one+") ;
						if(dc.flags & FCF_MERGED) strcat(c_flags,"merge+") ;
						if(dc.flags & FCF_DEAD_EDGE) strcat(c_flags,"dead+") ;
						if(dc.flags & FCF_ROW_EDGE) strcat(c_flags,"edge+") ;						if(dc.flags & FCF_ONEPAD) strcat(c_flags," one") ;
						if(dc.flags & FCF_BROKEN_EDGE) strcat(c_flags,"small+") ;
						if(dc.flags & FCF_BIG_CHARGE) strcat(c_flags,"charge+") ;

						if(strlen(c_flags)) {
							c_flags[strlen(c_flags)-1] = 0 ;
						}

						printf("row %d: %f %d %d %f %d %d %d 0x%02X[%s]\n",row,
						       dc.pad,dc.p1,dc.p2,
						       dc.tb,dc.t1,dc.t2,
						       dc.charge,
						       dc.flags,c_flags) ;

						p_buff += ints_per_cluster ;
					}
				}


				free(tpc->s2_start) ;
				tpc->s2_start = 0 ;
			}

			
			if(tpc->log_level>=1) LOG(INFO,"EVT %d: evt_stop: cluster words %d(%d)",i,ret,tpc->s2_words) ;

		}
	}

	run_done: ;
	tpc->run_stop() ;


	delete tpc ;

	return 0 ;
}
