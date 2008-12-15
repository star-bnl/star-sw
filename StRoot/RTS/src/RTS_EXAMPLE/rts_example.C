#include <stdio.h>
#include <getopt.h>
#include <sys/types.h>
#include <stdlib.h>

#include <rtsLog.h>	// for my LOG() call


// this needs to be always included
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>

// only the detectors we will use need to be included
// for their structure definitions...
#include <DAQ_BSMD/daq_bsmd.h>
#include <DAQ_BTOW/daq_btow.h>
#include <DAQ_EMC/daq_emc.h>
#include <DAQ_ESMD/daq_esmd.h>
#include <DAQ_ETOW/daq_etow.h>
#include <DAQ_FPD/daq_fpd.h>
#include <DAQ_FTP/daq_ftp.h>
#include <DAQ_L3/daq_l3.h>
#include <DAQ_PMD/daq_pmd.h>
#include <DAQ_PP2PP/daq_pp2pp.h>
#include <DAQ_RIC/daq_ric.h>
#include <DAQ_SC/daq_sc.h>
#include <DAQ_SSD/daq_ssd.h>
#include <DAQ_SVT/daq_svt.h>
#include <DAQ_TOF/daq_tof.h>
#include <DAQ_TPC/daq_tpc.h>
#include <DAQ_TPX/daq_tpx.h>
#include <DAQ_TRG/daq_trg.h>

// I wrapped more complicated detectors inside their own functions
// for this example
static int bsmd_doer(daqReader *rdr, char  *do_print) ;
static int esmd_doer(daqReader *rdr, char  *do_print) ;
static int btow_doer(daqReader *rdr, char  *do_print) ;
static int etow_doer(daqReader *rdr, char  *do_print) ;
static int tpc_doer(daqReader *rdr, char *do_print) ;
static int tpx_doer(daqReader *rdr, char *do_print) ;
static int trg_doer(daqReader *rdr, char *do_print) ;

int main(int argc, char *argv[])
{
	extern char *optarg ;
	extern int optind ;
	int c ;
	char *print_det = "" ;

	rtsLogOutput(RTS_LOG_STDERR) ;
	rtsLogLevel(WARN) ;


	while((c = getopt(argc, argv, "D:d:h")) != EOF) {
		switch(c) {
		case 'd' :
			rtsLogLevel(optarg) ;
			break ;
		case 'D' :
			print_det = optarg ;
			break ;
		default :
			break ;
		}
	}

	class daqReader *evp ;			// tha main guy
	evp = new daqReader(argv[optind]) ;	// create it with the filename argument..


	while(evp->get(0,0)) {	// keep getting new events
		daq_dta *dd ;	// generic data pointer; reused all the time


		LOG(INFO,"File name \"%s\": sequence %d",evp->file_name, evp->seq) ;

		/***************** let's do simple detectors; the ones which only have legacy *****/

		dd = evp->det("sc")->get() ;
		if(dd) LOG(INFO,"SC found") ;

		dd = evp->det("fpd")->get("legacy") ;
		if(dd) LOG(INFO,"FPD found") ;

		dd = evp->det("ftp")->get("legacy") ;
		if(dd) LOG(INFO,"FTP found") ;

		dd = evp->det("l3")->get("legacy") ;
		if(dd) LOG(INFO,"L3 found") ;

		dd = evp->det("rich")->get("legacy") ;
		if(dd) LOG(INFO,"RIC found") ;

		dd = evp->det("ssd")->get("legacy") ;
		if(dd) LOG(INFO,"SSD found") ;

		dd = evp->det("svt")->get("legacy") ;
		if(dd) LOG(INFO,"SVT found") ;

		dd = evp->det("pp2pp")->get("legacy") ;
		if(dd) LOG(INFO,"PP2PP found") ;

		dd = evp->det("pmd")->get("legacy") ;
		if(dd) LOG(INFO,"PMD found") ;

		dd = evp->det("tof")->get("legacy") ;
		if(dd) {
			LOG(INFO,"TOF found") ;
			if(strcasecmp(print_det,"tof")==0) {
				while(dd->iterate()) {
					tof_t *tof = (tof_t *)dd->Void ;
					for(int r=0;r<4;r++) {
						printf("RDO %d: words %d:\n",r+1,tof->ddl_words[r]) ;
						for(u_int i=0;i<tof->ddl_words[r];i++) {
							printf("\t%d: 0x%08X [%u dec]\n",i,tof->ddl[r][i],tof->ddl[r][i]) ;
						}
					}
				}
			}
		}

		if(trg_doer(evp, print_det)) LOG(INFO,"TRG found") ;
		
		/***************** EMCs ************************/

		if(btow_doer(evp, print_det)) LOG(INFO,"BTOW found") ;

		if(bsmd_doer(evp,print_det)) LOG(INFO,"BSMD found (any bank)") ;

		if(etow_doer(evp, print_det)) LOG(INFO,"ETOW found") ;

		if(esmd_doer(evp, print_det)) LOG(INFO,"ESMD found") ;

		/******************** TPC & TPX ***********************/

		if(tpc_doer(evp,print_det)) LOG(INFO,"TPC found (legacy)") ;

		if(tpx_doer(evp,print_det)) LOG(INFO,"TPX found (any bank)") ;



		/************  PSEUDO: SHOULD ONLY BE USED FOR BACKWARD COMPATIBILITY! ************/
		dd = evp->det("emc_pseudo")->get("legacy") ;
		if(dd) LOG(INFO,"EMC_PSEUDO LEGACY found") ;

		

	}

	return 0 ;
}


static int trg_doer(daqReader *rdr, char  *do_print)
{
	int found = 0 ;
	daq_dta *dd ;

	if(strcasestr(do_print,"trg")) ;
	else do_print = 0 ;

	// If i want decoded data ala old evpReader I call "legacy" ;

	dd = rdr->det("trg")->get("legacy") ;
	if(dd) {
		if(dd->iterate()) {
			trg_t *trg_p = (trg_t *) dd->Void ;

			if(do_print) {	// print something...
				printf("Trigger: daqbits 0x%08X, trg_word 0x%04X\n",trg_p->daqbits,trg_p->trg_word) ;
			}

		}
	}


	// if you need the void black box of untouched trigger data:
	dd = rdr->det("trg")->get("raw") ;
	if(dd) {
		if(dd->iterate()) {
			found = 1 ;


			u_char *trg_raw = dd->Byte;
			
			if(do_print) {	// I have no clue but let me print first few words...


				// simple start of trig desc; the way it should be...
				struct simple_desc {
					short len ;
					char evt_desc ;
					char ver ;
				} *desc ;

				desc = (simple_desc *) trg_raw ;


				printf("Trigger: raw bank has %d bytes: ver 0x%02X, desc %d, len %d\n",dd->ncontent,desc->ver, desc->evt_desc, desc->len) ;

				// dump first 10 ints...
				u_int *i32 = (u_int *) trg_raw ;
				for(int i=0;i<10;i++) {
					printf("Trigger: word %d: 0x%08X\n",i,i32[i]) ;
				}
			}
		}
	}

	return found ;
}


static int tpx_doer(daqReader *rdr, char  *do_print)
{
	int found = 0 ;
	daq_dta *dd ;

	if(strcasestr(do_print,"tpx")) ;	// leave as is...
	else do_print = 0 ;

	// TPX
	// it is better, more memory efficient, to call stuff sector by sector
	for(int s=1;s<=24;s++) {
		/// TPX legacy not done yet!
		//dd = rdr->det("tpx")->get("legacy",s) ;	// uses tpc_t! but not done YET!
		//if(dd) found = 1 ;

		int pixel_count[46] ;	// as an example we'll count pixels per row
		memset(pixel_count,0,sizeof(pixel_count)) ;
		int sec_found = 0 ;

		dd = rdr->det("tpx")->get("adc",s) ;
		if(dd) 	{

			while(dd->iterate()) {
				found = 1 ;
				sec_found = 1 ;
				if(do_print) {
					printf("TPX: sec %02d, row %2d, pad %3d: %3d pixels\n",dd->sec,dd->row,dd->pad,dd->ncontent) ;
				}

				pixel_count[dd->row] += dd->ncontent ;

				for(u_int i=0;i<dd->ncontent;i++) {
					if(do_print) {
						printf("\ttb %3d = %4d ADC\n",dd->adc[i].tb, dd->adc[i].adc) ;
					}
				}
			}
		

			if(sec_found && do_print) {
				for(int row=0;row<=45;row++) {
					printf("+sector %2d, row %2d: pixels %d\n",s,row,pixel_count[row]) ;
				}
			}
		}

		found = sec_found ;

		dd = rdr->det("tpx")->get("cld",s) ;
		if(dd) 	found = 1 ;
	
		// will only exist in token 0 of a pedestal run!
		dd = rdr->det("tpx")->get("pedrms",s) ;
		if(dd) found = 1 ;

	}

	return found ;

}

static int tpc_doer(daqReader *rdr, char  *do_print)
{
	int found = 0 ;
	daq_dta *dd ;

	if(strcasestr(do_print,"tpc")) ;	// leave as is...
	else do_print = 0 ;

	// although it is possible to have all sectors of the TPC
	// present in memory, it is better to do this sector-by-sector
	// ala the old evpReader, due to the memory footprint
	for(int s=1;s<=24;s++) {
		dd = rdr->det("tpc")->get("legacy",s) ;
		while(dd && dd->iterate()) {	
			found++ ;	// mark as found..
			tpc_t *tpc = (tpc_t *) dd->Void ;

			if(do_print) {
				printf("TPC sector %d: pixels %d\n",dd->sec,tpc->channels_sector) ;
			}
			
			// one can rerun the afterburner as well with:

			//daq_tpc *tpc_class = (daq_tpc *)rdr->det("tpc") ;
			//int cl_found = tpc_class->fcfReader(dd->sec,0,0,tpc) ;
			//LOG(NOTE,"TPC: rerun cluster finder: sector %d: found %d clusters",dd->sec,cl_found) ;
		}
	}

	return found ;

	
}
	
static int bsmd_doer(daqReader *rdr, char *do_print)
{
	int found = 0 ;
	daq_dta *dd ;

	if(strcasestr(do_print,"bsmd")) ;	// leave as is...
	else do_print = 0 ;


	// do I see the non-zero-suppressed bank? let's do this by fiber...
	for(int f=1;f<=12;f++) {
		dd = rdr->det("bsmd")->get("adc_non_zs",0,f) ;	// sector is ignored (=0)
		if(dd) {
			while(dd->iterate()) {
				found = 1 ;

				bsmd_t *d = (bsmd_t *) dd->Void ;

				if(do_print) printf("BSMD non-ZS: fiber %2d, capacitor %d:\n",dd->rdo,d->cap) ;


				for(int i=0;i<BSMD_DATSIZE;i++) {
					if(do_print) printf("   %4d = %4d\n",i,d->adc[i]) ;
				}
			}
		}
	}

	// do I see the zero suppressed bank?
	for(int f=1;f<=12;f++) {
		dd = rdr->det("bsmd")->get("adc",0,f) ;
		if(dd) {
			while(dd->iterate()) {
				found = 1 ;

				bsmd_t *d = (bsmd_t *) dd->Void ;

				if(do_print) printf("BSMD ZS: fiber %2d, capacitor %d:\n",dd->rdo,d->cap) ;

				for(int i=0;i<BSMD_DATSIZE;i++) {
					// since this is zero-suppressed, I'll skip zeros...
					if(do_print) if(d->adc[i]) printf("   %4d = %4d\n",i,d->adc[i]) ;
				}
			}
		}
	}


	return found ;
}

static int esmd_doer(daqReader *rdr, char *do_print)
{
	int found = 0 ;
	daq_dta *dd ;

	if(strcasestr(do_print,"esmd")) ;	// leave as is...
	else do_print = 0 ;


	dd = rdr->det("esmd")->get("adc") ;
	if(dd) {
		while(dd->iterate()) {
			found = 1 ;

			esmd_t *d = (esmd_t *) dd->Void ;

			for(int i=0;i<ESMD_MAXFEE;i++) {
				for(int j=0;j<ESMD_PRESIZE;j++) {
					if(do_print) printf("ESMD: fee %2d: preamble %d: 0x%04X [%d dec]\n",i,j,d->preamble[i][j], d->preamble[i][j]) ;
				}
				for(int j=0;j<ESMD_DATSIZE;j++) {
					if(do_print) printf("ESMD: fee %2d: data %d: 0x%04X [%d dec]\n",i,j,d->adc[i][j], d->adc[i][j]) ;
				}

			}
		}
	}

	return found ;
}

static int etow_doer(daqReader *rdr, char *do_print)
{
	int found = 0 ;
	daq_dta *dd ;

	if(strcasestr(do_print,"etow")) ;	// leave as is...
	else do_print = 0 ;


	dd = rdr->det("etow")->get("adc") ;
	if(dd) {
		while(dd->iterate()) {
			found = 1 ;

			etow_t *d = (etow_t *) dd->Void ;

			for(int i=0;i<ETOW_MAXFEE;i++) {
				for(int j=0;j<ETOW_PRESIZE;j++) {
					if(do_print) printf("ETOW: fee %2d: preamble %d: 0x%04X [%d dec]\n",i,j,d->preamble[i][j], d->preamble[i][j]) ;
				}
				for(int j=0;j<ETOW_DATSIZE;j++) {
					if(do_print) printf("ETOW: fee %2d: data %d: 0x%04X [%d dec]\n",i,j,d->adc[i][j], d->adc[i][j]) ;
				}

			}
		}
	}

	return found ;
}

static int btow_doer(daqReader *rdr, char *do_print)
{
	int found = 0 ;
	daq_dta *dd ;

	if(strcasestr(do_print,"btow")) ;	// leave as is...
	else do_print = 0 ;


	dd = rdr->det("btow")->get("adc") ;
	if(dd) {
		while(dd->iterate()) {
			found = 1 ;

			btow_t *d = (btow_t *) dd->Void ;

			for(int i=0;i<BTOW_MAXFEE;i++) {
				for(int j=0;j<BTOW_PRESIZE;j++) {
					if(do_print) printf("BTOW: fee %2d: preamble %d: 0x%04X [%d dec]\n",i,j,d->preamble[i][j], d->preamble[i][j]) ;
				}
				for(int j=0;j<BTOW_DATSIZE;j++) {
					if(do_print) printf("BTOW: fee %2d: data %d: 0x%04X [%d dec]\n",i,j,d->adc[i][j], d->adc[i][j]) ;
				}

			}
		}
	}

	return found ;
}

