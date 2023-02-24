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



//#include <DDB/ddb.h>
#include "ddb.h"
#include "support.h"
#include <DAQ_TPC23/tpx23.h>
#include <DAQ_TPC23/itpc23.h>


t_ave ave("main") ;
struct ddb_t ddb ;

static itpcPed *ped_c ;
static pthread_mutex_t peds_mutex ;

static tpc23_base *tpc ;

static int sim_d_cou[46] ;
static int file_d_cou[46] ;


static struct sim_t {
	float pad ;
	float tb ;
	u_short charge ;
	char flags ;
	char match ;
	short p1,p2 ;
	short t1,t2 ;
} sim_d[46][1000], file_d[46][1000] ;

int main(int argc, char *argv[])
{
	extern char *optarg ;
	extern int optind ;
	const char *fname ;
	const char *det = "itpc" ;
	int events_max = 1 ;
	int c ;
	int real_rdo[4] ;
	int real_sec[4] ;
	int use_rdr = 1 ;	// default YES
	int s_start, s_stop ;	
	daqReader *rdr = 0 ;
	int fmt_version ;
	int log_level ;
	int offline_mode ;

	rtsLogOutput(RTS_LOG_STDERR) ;
//	rtsLogLevel(NOTE) ;

	memset(&ddb,0,sizeof(ddb)) ;

	fmt_version = 0 ;
	log_level = 0 ;

	offline_mode = 0 ;	// do it directly fr

	ddb.run_type = 3 ;
	ddb.rb_mask = 0x4 ;	// RDO3 -- so that the default is valid for TPX and iTPC
	ddb.sector = 1 ;

	ddb.rb_mask = 0xF ;

	while((c=getopt(argc,argv,"t:e:D:S:r:R:E3l:O"))!=EOF) {
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
	case 't' :
		ddb.run_type = atoi(optarg) ;
		break ;
	case 'l' :
		log_level = atoi(optarg) ;
		break ;
	case 'O' :
		offline_mode = 1 ;
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

	LOG(INFO,"Starting: %s, sector %2d, rb_mask 0x%X, fmt_version %d, log_level %d; %s %s",det,ddb.sector,ddb.rb_mask,fmt_version,log_level,
	    __DATE__,__TIME__) ;

	tpc->online = 1 ;
	tpc->run_type = ddb.run_type ;
	tpc->log_level = log_level ;

	if(ddb.run_type==1) {
		ped_c = new itpcPed ;
		pthread_mutex_init(&peds_mutex,0) ;

		if(ddb.rts_id==ITPC_ID) {
			itpc23 *it = (itpc23 *)tpc ;


			it->data_c = ped_c ;

			it->data_c->run_type = ddb.run_type ;
			it->data_c->sector_id = ddb.sector ;

			ped_c->clear() ;
			ped_c->init(ddb.sector,1,0xFFFF) ;
		}
	}
	else tpc->data_c = 0 ;

	if(ddb.rts_id==ITPC_ID) {
		tpc->gains_from_cache("/RTS/conf/itpc/itpc_gains.txt.11Sep19.1") ;	// for the 20192 run
//		tpc->gains_from_cache("/RTS/conf/itpc/itpc_gains.txt.Feb20_20.bak") ;	// for the old 9.8 GeV
	}
	else {
		tpc->gains_from_cache("/RTS/conf/tpx/tpx_gains.txt.12Sep19.1") ;	// for the 20192 run
	}
	
	if(use_rdr==0) {	// for locally replayed file -- not used
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
		if(optind >=argc) {
			fname = "/RTS/TONKO/data/st_physics_adc_20192001_raw_5500002.daq" ;	// cannonical file!
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

	int event = 0 ;
				   

	// Run Start
	LOG(INFO,"Run Start; using %s; events %d",fname,events_max) ;
	tpc->run_start() ;

	for(int i=0;i<events_max;i++) {
		int fifo = i%64 ;

		if(rdr) {
			char *c_ret = rdr->get(0,EVP_TYPE_ANY) ;
			if(c_ret==0) goto run_done ;	
			
			fifo = 0 ;

			LOG(TERR,"event %d: token %d",i+1,rdr->token) ;

			if(rdr->token>=4096) continue ;
		}


		event++ ;

		printf("EVENT %d/%d\n",event,i+1) ;

		for(int sec=s_start;sec<=s_stop;sec++) {
			if(tpc->log_level>=1) LOG(INFO,"EVT %d: evt_start: fifo %d",i,fifo) ;

			memset(file_d_cou,0,sizeof(file_d_cou)) ;
			memset(sim_d_cou,0,sizeof(sim_d_cou)) ;

			// get the CLDs from file and stash them
			if(rdr) {
				daq_dta *dd = rdr->det(det)->get("cld",sec) ;

				while(dd && dd->iterate()) {
					int r = dd->row ;

					//printf("CLD sec %d: row %d: %d\n",sec,r,dd->ncontent) ;

					for(u_int i=0;i<dd->ncontent;i++) {
						file_d[r][file_d_cou[r]].pad = dd->cld[i].pad ;
						file_d[r][file_d_cou[r]].tb = dd->cld[i].tb ;
						file_d[r][file_d_cou[r]].charge = dd->cld[i].charge ;
						file_d[r][file_d_cou[r]].flags = dd->cld[i].flags ;
						file_d[r][file_d_cou[r]].match = 0 ;

						file_d[r][file_d_cou[r]].p1 = dd->cld[i].p1 ;
						file_d[r][file_d_cou[r]].p2 = dd->cld[i].p2 ;
						file_d[r][file_d_cou[r]].t1 = dd->cld[i].t1 ;
						file_d[r][file_d_cou[r]].t2 = dd->cld[i].t2 ;
						

						file_d_cou[r]++ ;
					}
				}
			}
			

			if(offline_mode==0) {

				LOG(INFO,"Doing Offline mode") ;
				
				tpc->online = 0 ;		// NOTE setting for Offline mode!

				tpc->sim_evt_start(sec) ;	// NOTE different "evt_start" for offline mode

				// now grab the ADC data from the DAQ file
				daq_dta *dd = rdr->det(det)->get("adc",sec) ;

				int track_id[512] ;
				u_short adc[512];

				memset(track_id,0,sizeof(track_id)) ;	// I'm not using it in this example...

				//int tb_max = 0 ;
				while(dd && dd->iterate()) {
					int r = dd->row ;
					int p = dd->pad ;


					// prepare 1 pad's worth of data
					memset(adc,0,sizeof(adc)) ;
					
					for(u_int i=0;i<dd->ncontent;i++) {
						//printf("ADC: sec %d, row %d, pad %d: tb %d, adc %d\n",sec,r,p,
						//       dd->adc[i].tb,dd->adc[i].adc) ;

						adc[dd->adc[i].tb] = dd->adc[i].adc ;

						//if(dd->adc[i].tb>tb_max) tb_max= dd->adc[i].tb ;
					}

					tpc->do_ch_sim(r,p,adc,track_id) ;	// this is how you "fill" the Offline TPC23
					
				}

				// LOG(TERR,"tb_max %d",tb_max) ;	// for my checks... ignore...
			}
			else {

				LOG(INFO,"Doing Online RAW mode") ;	// this is the way it is done Online!

				tpc->online = 1 ;

				// now grab the _RAW_ bank and re-run simulation
				//tpc->sim_evt_start(sec) ;	// WATCH IT: this is a _sim_ thing!
				tpc->evt_start() ;

				printf("Event %d, local %d\n",event,tpc->evt) ;

				// now grab the _RAW_ bank and re-run simulation

				for(int r=0;r<6;r++) {
			
				if(ddb.rb_mask & (1<<r)) ;
				else continue ;

				if(rdr) {
					
					//LOG(INFO,"requesting RAW: S%02d:%d",sec,real_rdo[r]) ;

					daq_dta *dd = rdr->det(det)->get("raw",sec,real_rdo[r]) ;
					

					if(dd==0) continue ;


					if(dd->iterate()==0) continue ;

					LOG(INFO,"Evt %d: in rdr data: SR%02d:%d: T %d, bytes %d",i,dd->sec,dd->rdo,rdr->token,dd->ncontent) ;


					u_int *d = (u_int *) dd->Void ;
					//for(int i=0;i<32;i++) {
					//	printf("raw in file: %d/%d = 0x%08X\n",i,dd->ncontent/4,d[i]) ;
					//}

					tpc->sector1 = dd->sec ;
					tpc->rdo1 = dd->rdo ;

					if(fmt_version < 23) tpc->from22to23((char *)dd->Void,dd->ncontent/4) ;

					//d = (u_int *) dd->Void ;
					//for(int i=0;i<32;i++) {
					//	printf("23 in file: %d/%d = 0x%08X\n",i,dd->ncontent/4,d[i]) ;
					//}


					tpc->sim_dta[0].rb[r].mem = (char *)dd->Void ;
					tpc->sim_dta[0].rb[r].bytes = dd->ncontent ;

					real_sec[r] = dd->sec ;
					real_rdo[r] = dd->rdo ;

				}

				tpc->set_rdo(real_sec[r],real_rdo[r]) ;

				char *c_addr = tpc->sim_dta[fifo].rb[r].mem ;
				int words = tpc->sim_dta[fifo].rb[r].bytes/4 ;


				if(tpc->log_level>=1) LOG(INFO,"EVT %d, RB %d (SR %d:%d): words %d",i,r,tpc->sector1,tpc->rdo1,words) ;

				// not needed but I want to see the token etc...
				u_int t_tmp = tpc->get_token_s(c_addr,words) ;
				int t_t = t_tmp & 0xFFF ;
				int t_daq = (t_tmp>>12)&0xF ;
				int t_trg = (t_tmp>>16)&0xF ;

				LOG(TERR,"EVT %d: fifo %d: T %d(%d,%d): words %d; T %d",i,fifo,t_t,t_trg,t_daq,words,tpc->token) ;

				tpc->rdo_scan(c_addr,words) ;

				if(rdr) {	// I need to nix it since I manipulated it
					tpc->sim_dta[0].rb[r].mem = 0 ;
				}

				}

			}
			
			// COMMON to both Offline and Online modes!

			

			const int words_per_cluster = 5 ;	// 5 for simulation, 2 normally, but let's exagerate a bit
			tpc->s2_max_words = tpc->sequence_cou*words_per_cluster + 2000 ;	// and a bit more
			tpc->s2_start = (u_int *) malloc(tpc->s2_max_words*4) ;

			LOG(INFO,"Before evt_stop: max_words %d",tpc->s2_max_words) ;

			int ret = tpc->evt_stop() ;

			LOG(INFO,"EVT %d: sequences %d, words %d, s2_words %d",i,tpc->sequence_cou,ret,tpc->s2_words) ;

			printf("sequence %d, words %d, s2_words %d\n",tpc->sequence_cou,ret,tpc->s2_words) ;

			if(tpc->s2_start) {
				itpc_fcf_c *fcf ;

				u_int *p_buff = tpc->s2_start ;
				u_int *end_buff = p_buff + tpc->s2_words ;


				while(p_buff < end_buff) {
					u_int row = *p_buff++ ;
					u_int version = *p_buff++ ;
					u_int int_cou ;
					int ints_per_cluster ;

					if(ddb.rts_id == ITPC_ID) {

						int_cou = *p_buff++ ;

						ints_per_cluster = (row>>16) ;
					}
					else {
						int_cou = version ;

						version = row & 0xFFFF0000 ;
						

						if(tpc->online==0) {
							ints_per_cluster = 5 ;
						}
						else {
							ints_per_cluster = 2 ;
						}
					}


					row &= 0xFFFF ;

					int clusters = int_cou / ints_per_cluster ;

					//LOG(TERR,"row %d, version 0x%X, int_cou %d, ints_per_cluster %d",
					//    row,version,int_cou,ints_per_cluster) ;

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


						
						sim_d[row][sim_d_cou[row]].pad = dc.pad ;
						sim_d[row][sim_d_cou[row]].tb = dc.tb ;
						sim_d[row][sim_d_cou[row]].charge = dc.charge ;
						sim_d[row][sim_d_cou[row]].flags = dc.flags ;
						sim_d[row][sim_d_cou[row]].match = 0 ;

						sim_d[row][sim_d_cou[row]].p1 = dc.p1 ;
						sim_d[row][sim_d_cou[row]].p2 = dc.p2 ;
						sim_d[row][sim_d_cou[row]].t1 = dc.t1 ;
						sim_d[row][sim_d_cou[row]].t2 = dc.t2 ;

						sim_d_cou[row]++ ;

						printf("row %d: %f %d %d %f %d %d %d 0x%02X[%s]\n",row,
						       dc.pad,dc.p1,dc.p2,
						       dc.tb,dc.t1,dc.t2,
						       dc.charge,
						       dc.flags,c_flags) ;

						fflush(stdout) ;

						p_buff += ints_per_cluster ;
					}
				}


				free(tpc->s2_start) ;
				tpc->s2_start = 0 ;
			}

			
			if(tpc->log_level>=1) LOG(INFO,"EVT %d: evt_stop: cluster words %d(%d)",i,ret,tpc->s2_words) ;

			printf("MATCHING sec %d\n",sec) ; fflush(stdout) ;
			int r_start ;
			int r_stop ;

			if(ddb.rts_id==ITPC_ID) {
				r_start = 1 ;
				r_stop = 40 ;
			}
			else {
				r_start = 14 ;
				r_stop = 45 ;
			}

			for(int r=r_start;r<=r_stop;r++) {

				printf("MATCHING: sec %d: r %d: file %d, sim %d\n",sec,r,file_d_cou[r],sim_d_cou[r]) ;
				fflush(stdout) ;

				for(int i=0;i<sim_d_cou[r];i++) {
					for(int j=0;j<file_d_cou[r];j++) {

						int m = 0 ;

						if(sim_d[r][i].tb == file_d[r][j].tb) m++ ;
						if(sim_d[r][i].pad == file_d[r][j].pad) m++ ;
						if(sim_d[r][i].charge == file_d[r][j].charge) m++ ;
						//if(sim_d[r][i].flags == file_d[r][j].flags) m++ ;

						if(m==3) {
							sim_d[r][i].match++ ;
							file_d[r][j].match++ ;
						}
					}
				}

				for(int i=0;i<sim_d_cou[r];i++) {
					//if(sim_d[r][i].match==1) continue ;
					const char *pp = "" ;

					int fla = sim_d[r][i].flags ;
					int match = sim_d[r][i].match ;

					//if(fla==0 || fla==2) ;
					//else continue ;

					//if(match) continue ;

					if(fla==0x10) pp = " WHOA" ;
					if(match==0 && fla==0) pp = " WHOAA" ;
					
					//if((fla&3)==3) pp="" ;
					//else {
					//	pp="WHOAAAAA" ;
					//	printf("...at event %d\n",event) ;
					//}

					if(match==1) continue ;

					printf("SIM mismatch (%d): r %d: %f %d %d %f %d %d %d 0x%X %s\n",sim_d[r][i].match,r,
					       sim_d[r][i].pad,sim_d[r][i].p1,sim_d[r][i].p2,
					       sim_d[r][i].tb,sim_d[r][i].t1,sim_d[r][i].t2,
					       sim_d[r][i].charge,sim_d[r][i].flags,pp) ;

				}

				for(int i=0;i<file_d_cou[r];i++) {
					//if(file_d[r][i].match==1) continue ;
					const char *pp ;

					int fla = file_d[r][i].flags ;
					int match = file_d[r][i].match ;
					//if(fla==0 || fla==2) ;
					//else continue ;

					//if(match) continue ;

					pp = "" ;

					if(match==1) ;
					else if(match==0 && fla==0x10) ;
					else pp=" WHOA" ;
					
					if(match==0 && fla==0) pp= "WHOAB" ;

					if(ddb.rts_id==ITPC_ID) {
						if(match==0 && fla==0x10) continue ;
					}

					if(match==1) continue ;

					printf("FILE mismatch (%d): row %d: %f %d %d %f %d %d %d 0x%X %s\n",file_d[r][i].match,r,
					       file_d[r][i].pad, file_d[r][i].p1,file_d[r][i].p2,
					       file_d[r][i].tb,file_d[r][i].t1,file_d[r][i].t2,
					       file_d[r][i].charge,file_d[r][i].flags,pp) ;

				}

			}
			
		}	// end of sector processing
	}// end of event

	run_done: ;
	tpc->run_stop() ;

	if(ddb.run_type==1) {
		if(ddb.rts_id==ITPC_ID) {

			ped_c->calc() ;
			ped_c->sanity(0) ;

			ped_c->to_cache("itpc_pedestals.txt",-1,-1) ;

	
		}
	}

	delete tpc ;

	return 0 ;
}
