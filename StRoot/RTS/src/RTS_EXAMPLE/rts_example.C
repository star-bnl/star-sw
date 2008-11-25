#include <stdio.h>
#include <getopt.h>
#include <sys/types.h>

#include <rtsLog.h>

// this needs to be always included
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>

// only the detectors we will use need to be included
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
#include <DAQ_TOF_t/daq_tof.h>
#include <DAQ_TPC/daq_tpc.h>
#include <DAQ_TPX_t/daq_tpx.h>
#include <DAQ_TRG/daq_trg.h>

// more complicated detectors I wrapped inside their own functions...
static int bsmd_doer(daqReader *rdr, int do_print) ;
static int tpc_doer(daqReader *rdr, int do_print) ;
static int tpx_doer(daqReader *rdr, int do_print) ;

int main(int argc, char *argv[])
{
	extern char *optarg ;
	extern int optind ;
	int c ;

	rtsLogOutput(RTS_LOG_STDERR) ;
	rtsLogLevel(WARN) ;


	while((c = getopt(argc, argv, "d:h")) != EOF) {
		switch(c) {
		case 'd' :
			rtsLogLevel(optarg) ;
			break ;
		default :
			break ;
		}
	}

	class daqReader *evp ;			// tha main guy
	evp = new daqReader(argv[optind]) ;	// create it with the filename argument..

#if 0
	// create all the detectors we need 
	// and assign them to our "driver" class daqReader
	new daq_bsmd(evp) ;
	new daq_btow(evp) ;
	new daq_emc(evp) ;
	new daq_esmd(evp) ;
	new daq_etow(evp) ;
	new daq_fpd(evp) ;
	new daq_ftp(evp) ;
	new daq_l3(evp) ;
	new daq_pmd(evp) ;
	new daq_pp2pp(evp) ;
	new daq_ric(evp) ;
	//new daq_sc(evp) ;
	new daq_ssd(evp) ;
	new daq_svt(evp) ;
	new daq_tof(evp) ;
	//new daq_tpc(evp) ;
	new daq_tpx(evp) ;
	new daq_trg(evp) ;
#endif

	while(evp->get(0,0)) {	// keep getting new events
		daq_dta *dd ;	// generic data pointer; reused all the time


		LOG(INFO,"File name \"%s\": sequence %d",evp->file_name, evp->seq) ;

		/***************** let's do simple detectors; the ones which only have legacy *****/
		dd = evp->det("trg")->get("legacy") ;
		if(dd) LOG(INFO,"TRG found") ;

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

		/************* now come dets which have legacy and non-legacy banks... */

		// TOF has 
		dd = evp->det("tof")->get("legacy") ;
		if(dd) LOG(INFO,"TOF LEGACY found") ;

		dd = evp->det("tof")->get("raw") ;
		if(dd) LOG(INFO,"TOF RAW(sfs) found") ;

		
		/***************** EMCs ************************/
		// PSEUDO: SHOULD ONLY BE USED FOR BACKWARD COMPATIBILITY!
		dd = evp->det("emc_pseudo")->get("legacy") ;
		if(dd) LOG(INFO,"EMC_PSEUDO LEGACY found") ;

		// simple...
		dd = evp->det("btow")->get("adc") ;
		if(dd) LOG(INFO,"BTOW found") ;

		dd = evp->det("etow")->get("adc") ;
		if(dd) LOG(INFO,"ETOW found") ;

		dd = evp->det("esmd")->get("adc") ;
		if(dd) LOG(INFO,"ESMD found") ;

		// BSMD
		if(bsmd_doer(evp,0)) LOG(INFO,"BSMD found (any bank)") ;

		// TPC
		if(tpc_doer(evp,0)) LOG(INFO,"TPC found (legacy)") ;

		// TPX
		if(tpx_doer(evp,0)) LOG(INFO,"TPX found (any bank)") ;

		

	}

	return 0 ;
}

static int tpx_doer(daqReader *rdr, int do_print)
{
	int found = 0 ;
	daq_dta *dd ;

	// TPX
	// it is better, more memory efficient, to call stuff sector by sector
	for(int s=1;s<=24;s++) {
		dd = rdr->det("tpx")->get("legacy",s) ;	// uses tpc_t!
		if(dd) found = 1 ;

		dd = rdr->det("tpx")->get("adc",s) ;
		if(dd) 	found = 1 ;
	
		dd = rdr->det("tpx")->get("cld",s) ;
		if(dd) 	found = 1 ;
	
		dd = rdr->det("tpx")->get("pedrms",s) ;
		if(dd) found = 1 ;

	}

	return found ;

}

static int tpc_doer(daqReader *rdr, int do_print)
{
	int found = 0 ;
	daq_dta *dd ;

	// although it is possible to have all sectors of the TPC
	// present in memory, it is better to do this sector-by-sector
	// ala the old evpReader, due to the memory footprint
	for(int s=1;s<=24;s++) {
		dd = rdr->det("tpc")->get("legacy",s) ;
		if(dd) {
			
			found++ ;	// mark as found...
			tpc_t *tpc = (tpc_t *) dd->Void ;
			
			// one can rerun the afterburner as well with:
			//daq_tpc *tpc_class = (daq_tpc *)rdr->det("tpc") ;


			int cl_found = 0 ;
			//cl_found = tpc_class->fcfReader(s,0,0,tpc) ;


			LOG(NOTE,"TPC: rerun cluster finder: sector %d: found %d clusters",s,cl_found) ;
		}
	}

	return found ;

	
}
	
static int bsmd_doer(daqReader *rdr, int do_print)
{
	int found = 0 ;
	daq_dta *dd ;

	// do I see the non-zero-suppressed bank? let's do this by fiber...
	for(int f=1;f<=12;f++) {
		dd = rdr->det("bsmd")->get("adc_non_zs",0,f) ;
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
