#include <stdio.h>
#include <sys/types.h>
#include <string.h>
#include <getopt.h>
#include <stdlib.h>

#include <rtsLog.h>
#include <rtsSystems.h>

#include <daqFormats.h>

#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>

#include <trgConfNum.h>
#include <DAQ_TRG/daq_trg.h>

#include <DAQ_FCS/daq_fcs.h>
#include <DAQ_FCS/fcs_data_c.h>

#include "fcs_trg_base.h"


static int sim_mode = 0 ;	//0 DAQ file rerun, 1 ASCII emulation
static fcs_trg_base *f_t ;


static u_int trg(daqReader *rdr)
{
	u_int fcs2019 = 0 ;
	int ok = 0 ;
	int fcs = 0 ;

	daq_dta *dd = rdr->det("trg")->get("raw") ;

	while(dd && dd->iterate()) {

#if 0
	printf("tinfo: seq = #%d  token = %d detectors = 0x%x triggers = 0x%llx/0x%llx/0x%llx  evpgroups=0x%x flags=0x%x\n",
           rdr->seq,
           rdr->token,
           rdr->detectors,
           rdr->daqbits64_l1,
           rdr->daqbits64_l2,
           rdr->daqbits64,
           rdr->evpgroups,
           rdr->flags);
#endif

	for(int i=0;i<64;i++) {
		if(rdr->daqbits64 & (1ll<<i)) {
			if(i==38) {
				fcs =1  ;
				ok = 1 ;
			}
			if(i==0) ok = 1 ;
			if(i==1) ok = 1 ;
			if(i==2) ok = 1 ;

			printf("Trg offline id %d\n",i) ;
		}
	}

        TriggerDataBlk *trg = (TriggerDataBlk *)dd->Byte;

        L1_DSM_Data *l1Dsm = (L1_DSM_Data *)(((char *)trg) + swap32(trg->L1_DSM_ofl.offset));

	u_int lastdsm[8] ;

        for(int i=0;i<8;i++) {
              lastdsm[i] = swap16(l1Dsm->lastDSM[i]) ;
              //printf(".... %d: 0x%04X\n",i,lastdsm[i]) ;
	}

        fcs2019 = (lastdsm[4] >> 10) & 1 ;




	}

	fcs2019 |= (fcs<<2) | (ok<<1) ;

//	printf("fcs2019 %d\n",fcs2019) ;

	return fcs2019;
}


int from_ascii_evt()
{

	printf("==> Running simulation now\n") ;
	f_t->end_event() ;

	return 0 ;
}

int from_ascii_open(const char *fn)
{
	int evts = 0 ;
	u_short dta[8] ;

	LOG(INFO,"Opening %s",fn) ;

	FILE *f = fopen(fn,"r") ;

	if(f==0) {
		perror(fn) ;
		return -1 ;
	} ;

	memset(dta,0,sizeof(dta)) ;


	for(;;) {
		char buff[128] ;
		if(fgets(buff,sizeof(buff),f)==0) break ;

		int det,ns,dep,ch,adc ;
		int ret = sscanf(buff,"%d %d %d %d %d",&det,&ns,&dep,&ch,&adc) ;
		if(ret != 5) continue ;

//		printf("... %d %d %d %d %d\n",det,ns,dep,ch,adc) ;

		if(det < 0) {	// end of ASCII event data...
			evts++ ;
			LOG(TERR,"Running evt %d",evts) ;
			from_ascii_evt() ;

			f_t->start_event() ;	// clears misc structures
		}
		else {
			if(adc > 4095) adc = 4095 ;
			dta[3] = adc ;

			if(det<0 || det>=3) printf("*** ERROR: DET %d\n",det) ;
			if(ns<0 || ns>=2) printf("*** ERROR: NS %d\n",ns) ; 
			if(dep<0 || dep>=24) printf("*** ERROR: DEP %d\n",dep) ;

			f_t->fill_event(det,ns,dep,ch,dta,8) ;
		}

	}


	return evts ;
}


int main(int argc, char *argv[])
{
	extern char *optarg ;
	extern int optind ;
	int c ;

	rtsLogOutput(RTS_LOG_STDERR) ;	// must come first!

	f_t = new fcs_trg_base ;


	f_t->log_level = 110 ;	// comes first

	while((c=getopt(argc,argv,"d:al:")) != EOF) {
		switch(c) {
		case 'd' :
			rtsLogLevel(optarg) ;
			break ;
		case 'l' :
			f_t->log_level = atoi(optarg) ;
			break ;
		case 'a' :
			sim_mode = 1 ;
			break ;
		default :
			break ;
		}
	}

	if(sim_mode) f_t->sim_mode = 1 ;
	f_t->init() ;


	// overrides...
	f_t->ht_threshold[2] = 90 ;
	f_t->marker.last_xing = 8 ;	// 14 was for 20*8, 9 for 12*8?

	int evt = 0 ;
	int fcs_evt = 0 ;


	if(sim_mode) {
		f_t->run_start(0) ;

		from_ascii_open(argv[optind]) ;

		f_t->run_stop() ;

		return 0 ;
	}


	daqReader *rdr = new daqReader(argv[1]) ;

	f_t->run_start(rdr->run) ;

	for(;;) {
		daq_dta *dd ;

		rdr->get(0, EVP_TYPE_ANY) ;

		if(rdr->status) break ;


		dd = rdr->det("fcs")->get("adc") ;


		evt++ ;

		int got_one = 0 ;



//		f_t->start_event() ;



		while(dd && dd->iterate()) {
			if(got_one == 0) {
				fcs_evt++ ;
				printf("=== fcs_evt %d\n",fcs_evt) ;
				u_int fcs_trg = trg(rdr) ;
				printf("--- fcs_trg %d\n",fcs_trg) ;
				f_t->start_event() ;
				got_one = 1 ;
			}

			int det = (dd->sec >> 6) & 0x3 ;
			int ns = (dd->sec >> 5) & 1 ;
			int dep = dd->row ;
			int c = dd->pad ;

			LOG(NOTE,"evt %d: %d %d %d %d : %d",evt,det,ns,dep,c,dd->ncontent) ;

			f_t->fill_event(det,ns,dep,c,(u_short *)dd->Void,dd->ncontent) ;

		}

		if(got_one) {

			f_t->end_event() ;
		}

		printf("***** Event %d/%d\n",fcs_evt,evt) ;

		//if(fcs_evt==100) break ;
	}

	f_t->run_stop() ;

	LOG(INFO,"Done: %d/%d evts",fcs_evt,evt) ;

	return 0 ;
}

