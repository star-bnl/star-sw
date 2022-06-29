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

#include <DAQ_ITPC/daq_itpc.h>
#include <DAQ_ITPC/itpcInterpreter.h>
#include <DAQ_ITPC/itpcFCF.h>

static struct sim_t {
	float pad ;
	float tb ;
	u_short charge ;
	char flags ;
	char match ;
} sim_d[41][1000], file_d[41][1000] ;

static int sim_d_cou[41] ;
static int file_d_cou[41] ;

int main(int argc, char *argv[])
{
	int c ;
	int events = 1 ;		// default
	int sector = 1 ;


	rtsLogOutput(RTS_LOG_STDERR) ;

	itpc_fcf_c *fcf = new itpc_fcf_c ;
	fcf->my_id = 1 ;
	fcf->offline = 0 ;

	

	while((c=getopt(argc,argv,"e:s:"))!=EOF) {
	switch(c) {
	case 'e' :
		events = atoi(optarg) ;
		break ;
	case 's' :
		sector = atoi(optarg) ;
		break ;
	}
	}


	fcf->sector_id = sector ;
	fcf->init(sector,"/RTS/conf/itpc/itpc_gains.txt.11Sep19.1") ;	// load gains for sector 

	


	restart:;

	while(optind<argc) {	// loop over files


	LOG(INFO,"Using file %s [%d/%d] for sector %d",argv[optind],optind,argc,sector) ;


	daqReader *rdr = new daqReader((char *)argv[optind]) ;


	// inject gains
	daq_dta *gain_dta = rdr->det("itpc")->put("gain") ;
	for(int r=1;r<=40;r++) {
		daq_det_gain *gain = (daq_det_gain *) gain_dta->request(121) ;
		for(int p=0;p<=120;p++) {
			fcf->get_gain(sector,r,p) ;

			gain[p].gain = fcf->get_gain(sector,r,p) ;
			gain[p].t0 = fcf->get_t0(sector,r,p) ;
		}
		gain_dta->finalize(121,sector,r) ;
	}


	// loop over requested events in the file...
	for(int i=0;i<events;i++) {	// loop over events in the file

	if(rdr->get(0,EVP_TYPE_ANY)==0) {	// no more events...
		optind++ ;
		delete rdr ;
		goto restart ;			// end-of-file, try next file
	}

	daq_dta *dd = rdr->det("itpc")->get("adc",sector) ;

	daq_dta *sim_dta = rdr->det("itpc")->put("adc_sim") ;


	int got_data = 0 ;
	while(dd && dd->iterate()) {
		got_data = 1 ;
		daq_sim_adc_tb *sim_d = (daq_sim_adc_tb *) sim_dta->request(512) ;

		for(u_int i=0;i<dd->ncontent;i++) {
//			if(dd->adc[i].adc<2) printf("pad %d, tb %d, adc %d\n",dd->pad,dd->adc[i].tb,dd->adc[i].adc) ;

			sim_d[i].adc = dd->adc[i].adc ;
			sim_d[i].tb = dd->adc[i].tb ;
			sim_d[i].track_id = 0xFFFF ;
		}

		sim_dta->finalize(dd->ncontent,sector,dd->row,dd->pad) ;
	}


	if(got_data) {
		daq_dta *cld = rdr->det("itpc")->get("cld_sim",sector) ;

		memset(sim_d_cou,0,sizeof(sim_d_cou)) ;

		while(cld && cld->iterate()) {
			int r = cld->row ;	// shorthand

			daq_sim_cld_x *dc = (daq_sim_cld_x *) cld->Void ;

			for(u_int i=0;i<cld->ncontent;i++) {
#if 0
				printf("RERUN: %d: sec %2d, row %2d: %f %f %d 0x%X - track %d, Q %d, pixels %d, max adc %d\n",sim_d_cou,cld->sec,cld->row,
				       dc->cld.pad,dc->cld.tb,
				       dc->cld.charge,dc->cld.flags,
				       dc->track_id,dc->quality,
				       dc->pixels,dc->max_adc) ;
#endif
				sim_d[r][sim_d_cou[r]].match = 0 ;
				sim_d[r][sim_d_cou[r]].pad = dc->cld.pad ;
				sim_d[r][sim_d_cou[r]].tb = dc->cld.tb ;
				sim_d[r][sim_d_cou[r]].charge = dc->cld.charge ;
				sim_d[r][sim_d_cou[r]].flags = dc->cld.flags ;

				dc++ ;
				sim_d_cou[r]++ ;
			}
		}



		cld = rdr->det("itpc")->get("cld",sector) ;

		memset(file_d_cou,0,sizeof(file_d_cou)) ;

		while(cld && cld->iterate()) {
			int r = cld->row ;	// shorthand

			daq_cld *dc = (daq_cld *)cld->Void ;
			for(u_int i=0;i<cld->ncontent;i++) {
#if 0
				printf("FILE: %d: sec %2d, row %2d: %f %f %d 0x%X\n",file_d_cou,cld->sec,cld->row,
				       dc->pad,dc->tb,
				       dc->charge,dc->flags) ;

#endif
				if(dc->flags & 0x10) {
					dc++ ;
					continue ;
				}

				file_d[r][file_d_cou[r]].match = 0 ;
				file_d[r][file_d_cou[r]].pad = dc->pad ;
				file_d[r][file_d_cou[r]].tb = dc->tb ;
				file_d[r][file_d_cou[r]].charge = dc->charge ;
				file_d[r][file_d_cou[r]].flags = dc->flags ;

				file_d_cou[r]++ ;
				dc++ ;
			}

			

		}

		printf("***** Event %d: sector %d\n",i,sector) ;

		int matches = 0 ;
		for(int r=1;r<=40;r++) {	// row

		for(int i=0;i<sim_d_cou[r];i++) {
		for(int j=0;j<file_d_cou[r];j++) {
			int m = 0 ;

			if(sim_d[r][i].tb==file_d[r][j].tb) m++ ;
			if(sim_d[r][i].pad==file_d[r][j].pad) m++ ;
			if(sim_d[r][i].charge==file_d[r][j].charge) m++ ;
			if(sim_d[r][i].flags==file_d[r][j].flags) m++ ;

			if(m==4) {
				matches++  ;
				//printf("MATCH %d\n",matches) ;
				sim_d[r][i].match++ ;
				file_d[r][j].match++ ;
			}
		}
		}

			
		for(int i=0;i<sim_d_cou[r];i++) {
			if(sim_d[r][i].match==1) continue ;

			printf("SIM mismatch (%d): row %d: %f %f %d %d\n",sim_d[r][i].match,r,
			       sim_d[r][i].pad,
			       sim_d[r][i].tb,sim_d[r][i].charge,sim_d[r][i].flags) ;
		}

		for(int i=0;i<file_d_cou[r];i++) {
			if(file_d[r][i].match==1) continue ;

			printf("FILE mismatch (%d): row %d: %f %f %d %d\n",file_d[r][i].match,r,
			       file_d[r][i].pad,file_d[r][i].tb,file_d[r][i].charge,file_d[r][i].flags) ;
		}
		}	// end of row

	}




	}	// event loop

	delete rdr ;
	optind++ ;

	}	// DAQ file loop


	return 0 ;
}
