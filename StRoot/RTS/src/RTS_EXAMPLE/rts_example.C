#include <stdio.h>
#include <unistd.h>
#include <getopt.h>
#include <sys/types.h>
#include <stdlib.h>
#include <time.h>

#include <rtsLog.h>	// for my LOG() call
#include <rtsSystems.h>

// this needs to be always included
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>

#include <trgDataDefs.h>

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
#include <DAQ_HLT/daq_hlt.h>
#include <DAQ_L4/daq_l4.h>
#include <DAQ_FGT/daq_fgt.h>	//includes GMT & IST
#include <DAQ_MTD/daq_mtd.h>
#include <DAQ_PXL/daq_pxl.h>
#include <DAQ_SST/daq_sst.h>
#include <DAQ_FPS/daq_fps.h>
#include <DAQ_RHICF/daq_rhicf.h>

#include <TPC/rowlen.h>

/* various test routines... typically used by Tonko only */
#include <DAQ_FGT/fgtPed.h>
static int fgt_test(daqReader *rdr, const char *do_print, int which) ;
#include <DAQ_SST/sstPed.h>
static int sst_test(daqReader *rdr, int mode) ;

// I wrapped more complicated detectors inside their own functions
// for this example
#ifdef INSIST_ON_EMC_PSEUDO
static int emc_pseudo_doer(daqReader *rdr, const char *do_print) ;
#endif

static int bsmd_doer(daqReader *rdr, const char  *do_print) ;
static int esmd_doer(daqReader *rdr, const char  *do_print) ;
static int btow_doer(daqReader *rdr, const char  *do_print) ;
static int etow_doer(daqReader *rdr, const char  *do_print) ;
static int tpc_doer(daqReader *rdr, const char *do_print) ;
static int tpx_doer(daqReader *rdr, const char *do_print) ;
static int trg_doer(daqReader *rdr, const char *do_print) ;
static int ftp_doer(daqReader *rdr, const char *do_print) ;
static int pmd_doer(daqReader *rdr, const char *do_print) ;
static int hlt_doer(daqReader *rdr, const char *do_print) ;
static int l4_doer(daqReader *rdr, const char *do_print) ;

static int pp2pp_doer(daqReader *rdr, const char *do_print) ;
static int l3_doer(daqReader *rdr, const char *do_print) ;
static int fgt_doer(daqReader *rdr, const char *do_print, int which) ;
static int mtd_doer(daqReader *rdr, const char *do_print) ;
static int tinfo_doer(daqReader *rdr, const char *do_print);
static int pxl_doer(daqReader *rdr, const char *do_print) ;
static int sst_doer(daqReader *rdr, const char *do_print) ;
static int fps_doer(daqReader *rdr, const char *do_print) ;
static int rhicf_doer(daqReader *rdr, const char *do_print) ;

static int good ;
static int bad ;
static int nfs_loops ;

static int run_number ;

int main(int argc, char *argv[])
{
	extern char *optarg ;
	extern int optind ;
	int c ;
	const char *print_det = "" ;
	char _mountpoint[256];
	const char *mountpoint = "/net/evp";	// sensible default from daqman...

	rtsLogOutput(RTS_LOG_STDERR) ;
	rtsLogLevel((char *)WARN) ;


	run_number = -1 ;

	while((c = getopt(argc, argv, "D:d:m:h")) != EOF) {
		switch(c) {
		case 'd' :
			rtsLogLevel(optarg) ;
			break ;
		case 'D' :
			print_det = optarg ;
			break ;
		case 'm' :
			mountpoint = _mountpoint;
			strncpy(_mountpoint, (char *)optarg,sizeof(_mountpoint)-1);
			break;
		  
		default :
			break ;
		}
	}

	class daqReader *evp = 0 ;			// tha main guy

	if(optind >= argc) {	// no arguments -- NFS
		LOG(INFO,"Using direct access via NFS directory %s",mountpoint) ;

		evp = new daqReader(argv[optind]) ;	// create it with the filename argument..

		evp->setEvpDisk((char *)mountpoint);

		optind = argc - 1 ;	//hack up 1 loop

	}


	while(optind < argc) {

	if(evp==0) {
		LOG(INFO,"Opening file %s",argv[optind]) ;
		evp = new daqReader(argv[optind]) ;	// create it with the filename argument..
	}


	optind++ ;

	for(;;) {
	        char *ret = evp->get(0,EVP_TYPE_ANY);
       
		if(ret) {
		  if(evp->status) {
			bad++ ;
			LOG(ERR,"evp status is non-null [0x08X, %d dec]",evp->status,evp->status) ;
			continue ;
		  }
		  good++;
		}
		else {    // something other than an event, check what.
		  switch(evp->status) {
		  case EVP_STAT_OK:   // just a burp!
		    continue;
		  case EVP_STAT_EOR:
		    LOG(DBG, "End of Run/File");
		    if(evp->IsEvp()) {   // but event pool, keep trying...
		      LOG(DBG, "Wait a second...");
		      sleep(1);
		      nfs_loops++ ;
		      if((nfs_loops%5)==0) {
				LOG(WARN,"Still waiting for Event Pool...") ;
		      }
		      continue;
		    }
		    break;        // file, we're done...
		  case EVP_STAT_EVT:
		    bad++;
		    LOG(WARN, "Problem getting event - skipping [good %d, bad %d]",good,bad);
		    sleep(1);
		    continue;
		  case EVP_STAT_CRIT:
		    bad++ ;
		    LOG(CRIT,"evp->status CRITICAL (?)") ;
		    break ;
		  }
		}

		if(run_number < 0) {
			
			run_number = evp->run ;		
			LOG(INFO,"Opened run number %08u",evp->run) ;
		}

		if(evp->status == EVP_STAT_EOR) {
			LOG(INFO,"End of File reached...") ;
			break ;	// of the for() loop...
		}

		daq_dta *dd ;	// generic data pointer; reused all the time

		time_t e_time = (time_t) evp->evt_time ;
		char *date = ctime(&e_time) ;
		date[strlen(date)-1] = 0 ;	//get rid of NL

		LOG(INFO,"evt %d: sequence %d: token %4d, trgcmd %d, daqcmd %d, time \"%s\", detectors 0x%08X (status 0x%X), evpgroups 0x%X",good,evp->seq, evp->token, evp->trgcmd, evp->daqcmd,
		    date, evp->detectors, evp->status,evp->evpgroups) ;


		/***************** let's do simple detectors; the ones which only have legacy *****/

		if(print_det[0]) {
		  if(strcmp(print_det, "tinfo") == 0) {		    
		    printf("trginfo: seq = #%d  token = %d detectors = 0x%x triggers = 0x%llx/0x%llx/0x%llx  evpgroups=0x%x flags=0x%x\n",
			   evp->seq,
			   evp->token,
			   evp->detectors,
			   evp->daqbits64_l1,
			   evp->daqbits64_l2,
			   evp->daqbits64,
			   evp->evpgroups,
			   evp->flags);

		    tinfo_doer(evp, "tinfo");
		  }
		}

		if(print_det[0]) {
		  if(strcmp(print_det, "readahead") == 0) {		    
		    SummaryInfo nsummary;
		    int ret = evp->readNextFutureSummaryInfo(&nsummary);
		   
		   
		    if(ret <= 0) {
		      printf("Event #%d, token %d triggers 0x%llx  ---->   No Next Event...",
			     evp->seq,evp->token,evp->daqbits64);
		    }
		    else {
		      printf("Event #%d, token %d triggers 0x%llx   ---->   Next Event:  #%d, token %d triggers 0x%llx\n",
			     evp->seq,evp->token,evp->daqbits64,
			     nsummary.seq, nsummary.token, 
			     make64(nsummary.L3summary[0],nsummary.L3summary[1]));
		    }
		  }
		}

		dd = evp->det("sc")->get() ;
		if(dd && dd->iterate()) {
			LOG(INFO,"SC found") ;
			if(strcasecmp(print_det,"sc")==0) {
				sc_t *sc_p = (sc_t *) dd->Void ;
				
				// oh well, one needs to to these calculations here
				// since the SFS daq_sc.cxx doesn't know the absolute time
				int alag ;

				sc_p->timelag = evp->evt_time - sc_p->time ;
				if(sc_p->timelag > 0) alag = sc_p->timelag ;
				else alag = -sc_p->timelag ;

				if(alag > 5) sc_p->valid = 0 ;
				else sc_p->valid = 1 ;

				printf("SC: valid %d, time %u, timelag %d, B field %.3f\n",sc_p->valid,sc_p->time,sc_p->timelag,sc_p->mag_field) ;
				for(int i=0;i<16;i++) {
					printf("\tRICH scaler %2d: %u\n",i,sc_p->rich_scalers[i]) ;
				}
			}
		}

		dd = evp->det("fpd")->get("legacy") ;
		if(dd) LOG(INFO,"FPD found") ;

		if(ftp_doer(evp,print_det)) LOG(INFO,"FTP found") ;
	
				
		dd = evp->det("rich")->get("legacy") ;
		if(dd) LOG(INFO,"RIC found") ;

		dd = evp->det("ssd")->get("legacy") ;
		if(dd) LOG(INFO,"SSD found") ;

		dd = evp->det("svt")->get("legacy") ;
		if(dd) LOG(INFO,"SVT found") ;

		
		if(pmd_doer(evp, print_det)) LOG(INFO,"PMD found") ;

		dd = evp->det("tof")->get("legacy") ;
		if(dd) {
			LOG(INFO,"TOF found") ;
			if(strcasecmp(print_det,"tof")==0) {
				while(dd->iterate()) {
					tof_t *tof = (tof_t *)dd->Void ;
					for(int r=0;r<4;r++) {
						printf("TOF RDO %d: words %d:\n",r+1,tof->ddl_words[r]) ;
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

		// logging done in the doer...
		bsmd_doer(evp,print_det) ;

		if(etow_doer(evp, print_det)) LOG(INFO,"ETOW found") ;

		if(esmd_doer(evp, print_det)) LOG(INFO,"ESMD found") ;

		/******************** TPC (old electronics) ***********************/

		if(tpc_doer(evp,print_det)) LOG(INFO,"TPC found (legacy)") ;

		/********************** TPX ***************************/
		// logging is done in the tpx_doer...
		tpx_doer(evp,print_det) ;



		/*************************** PP2PP **********************/
		pp2pp_doer(evp,print_det) ;


		/*************************** L3/HLT09 **************************/
		if(l3_doer(evp,print_det)) LOG(INFO,"L3/HLT_FY09 found") ;

		/*************************** HLT10 **************************/
		if(hlt_doer(evp,print_det)) LOG(INFO,"HLT_FY10 found") ;

		/*************************** L4 (HLT 12) *******************/
		if(l4_doer(evp,print_det)) {
		    LOG(INFO, "HLT FY12 found");
		}

		/*************************** FGT **************************/
		fgt_doer(evp,print_det,FGT_ID) ;

		/*************************** MTD **************************/
		if(mtd_doer(evp,print_det)) LOG(INFO,"MTD found") ;


		/*************************** GMT **************************/
		fgt_doer(evp,print_det,GMT_ID) ;

		/*************************** IST **************************/
		fgt_doer(evp,print_det,IST_ID) ;
		fgt_test(evp,print_det,IST_ID) ;	//used by Tonko; ignore

		/*************************** PXL **************************/
		if(pxl_doer(evp,print_det)) LOG(INFO,"PXL found") ;
		
		/*************************** SST **************************/
		sst_doer(evp,print_det) ;

		/*************************** FPS **************************/
		fps_doer(evp,print_det) ;

		/*************************** RHICF **************************/
		rhicf_doer(evp,print_det) ;
		



		/************  PSEUDO: SHOULD ONLY BE USED FOR BACKWARD COMPATIBILITY! ************/
#ifdef INSIST_ON_EMC_PSEUDO
		if(emc_pseudo_doer(evp,print_det)) LOG(INFO,"EMC found (any detector)") ;
#endif

	}

	delete evp ;	// cleanup i.e. if running through a set of files.
	evp = 0 ;
	run_number = -1 ;

	}

	LOG(INFO,"Found %d good, %d bad events",good,bad) ;

//	sst_test(0,2) ;
	return 0 ;
}

static int trg_doer(daqReader *rdr, const char  *do_print)
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
				printf("Trigger: tcubits 0x%08X, trg_word 0x%04X\n",trg_p->tcubits,trg_p->trg_word) ;
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


static int hlt_doer(daqReader *rdr, const char  *do_print)
{
	int found = 0 ;
	daq_dta *dd ;

	if(strcasestr(do_print,"hlt")) ;	// leave as is...
	else do_print = 0 ;

	for(int s=1;s<=24;s++) {
		dd = rdr->det("hlt")->get("tpx",s) ;
		while(dd && dd->iterate()) {
			found = 1 ;
			if(do_print) {
				printf("HLT TPX sec %02d: bytes %d\n",dd->sec,dd->ncontent) ;
			}
		}
	}

	dd = rdr->det("hlt")->get("trg") ;
	while(dd && dd->iterate()) {
		found = 1 ;
		if(do_print) {
			printf("HLT TRG sec %02d: bytes %d\n",dd->sec,dd->ncontent) ;
		}
	}

	dd = rdr->det("hlt")->get("tof") ;
	while(dd && dd->iterate()) {
		found = 1 ;
		if(do_print) {
			printf("HLT TOF sec %02d: bytes %d\n",dd->sec,dd->ncontent) ;
		}
	}

	dd = rdr->det("hlt")->get("gl3") ;
	while(dd && dd->iterate()) {
		found = 1 ;
		if(do_print) {
			hlt_gl3_t *h = (hlt_gl3_t *) dd->Void ;
			printf("HLT GL3 sec %02d: bytes %d: %d %s\n",dd->sec,dd->ncontent,h->bytes,h->name) ;
		}
	}



	return found ;
}

static int l4_doer(daqReader *rdr, const char  *do_print)
{
	int found = 0 ;
	daq_dta *dd ;

	if(strcasestr(do_print,"l4")) ;	// leave as is...
	else do_print = 0 ;

	for(int s=1;s<=24;s++) {
		dd = rdr->det("l4")->get("tpx",s) ;
		while(dd && dd->iterate()) {
			found = 1 ;
			if(do_print) {
				printf("L4 TPX sec %02d: bytes %d\n",dd->sec,dd->ncontent) ;
			}
		}
	}

	dd = rdr->det("l4")->get("trg") ;
	while(dd && dd->iterate()) {
		found = 1 ;
		if(do_print) {
			printf("l4 TRG sec %02d: bytes %d\n",dd->sec,dd->ncontent) ;
		}
	}

	dd = rdr->det("l4")->get("tof") ;
	while(dd && dd->iterate()) {
		found = 1 ;
		if(do_print) {
			printf("l4 TOF sec %02d: bytes %d\n",dd->sec,dd->ncontent) ;
		}
	}

	dd = rdr->det("l4")->get("gl3") ;
	while(dd && dd->iterate()) {
		found = 1 ;
		if(do_print) {
			l4_gl3_t *h = (l4_gl3_t *) dd->Void ;
			printf("L4 GL3 sec %02d: bytes %d: %d %s\n",dd->sec,dd->ncontent,h->bytes,h->name) ;
		}
	}



	return found ;
}

static int tpx_doer(daqReader *rdr, const char  *do_print)
{
	int found = 0 ;
	int adc_found = 0 ;
	int cld_found = 0 ;
	int ped_found = 0 ;
	char s_mask[24] ;

	daq_dta *dd ;

	if(strcasestr(do_print,"tpx")) ;	// leave as is...
	else do_print = 0 ;

	// TPX
	// it is better, more memory efficient, to call stuff sector by sector (if possible)

	memset(s_mask,0,sizeof(s_mask)) ;

	for(int s=1;s<=24;s++) {

/* stop using legacy, even in the example...
		dd = rdr->det("tpx")->get("legacy",s) ;	// uses tpc_t
		while(dd && dd->iterate()) {
			found = 1 ;
				
			struct tpc_t *tpc = (tpc_t *) dd->Void ;
			if(do_print) {
				int cls = 0 ;
				for(int r=0;r<45;r++) {
					cls += tpc->cl_counts[r] ;
				}

				printf("TPX: sec %02d (as legacy): %d pixels, %d clusters\n",dd->sec,tpc->channels_sector,cls) ;
			}

		}
*/

		int pixel_count[46] ;	// as an example we'll count pixels per row
		memset(pixel_count,0,sizeof(pixel_count)) ;
		int sec_found = 0 ;

		dd = rdr->det("tpx")->get("adc",s) ;
		if(dd) 	{
			while(dd->iterate()) {
				found = 1 ;	// any sector...
				sec_found = 1 ;
				adc_found = 1 ;	// any sector...

				//if(dd->row > 8) continue ;
				
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
		

			if(sec_found) {

				s_mask[dd->sec-1]=1 ;

				if(do_print) {

					for(int row=0;row<=45;row++) {
						int max_cou = tpc_rowlen[row] * 400 ;
						if(max_cou==0) max_cou = 1 ;
						double occ = 100.0*(double)pixel_count[row]/(double)max_cou ;
						printf("+sector %2d, row %2d: pixels %d, occupancy %.1f%%\n",s,row,pixel_count[row],occ) ;
					}
				}
			}
		}

		LOG(DBG,"Doing TPX get CLD, s %d",s) ;

		dd = rdr->det("tpx")->get("cld",s) ;
		while(dd && dd->iterate()) {

			found = 1 ;
			cld_found = 1 ;

			s_mask[dd->sec-1]=1 ;


			if(do_print) {
				printf("TPX: sec %02d, row %2d: %3d clusters (evt %d)\n",dd->sec,dd->row,dd->ncontent,good) ;
			}
			
			for(u_int i=0;i<dd->ncontent;i++) {
				if(do_print) {
					int p1,p2,t1,t2 ;
					int bad = 0 ;
					p1 = dd->cld[i].p1 ;
					p2 = dd->cld[i].p2 ;
					t1 = dd->cld[i].t1 ;
					t2 = dd->cld[i].t2 ;

					if(p1 > 200) bad = 1 ;
					if(p2 > 200) bad = 1 ;
					if(p2<p1) bad = 1 ;
					if((p2-p1)>14) bad = 1 ;

					if(t1 > 1200) bad = 1 ;
					if(t2 > 1200) bad = 1 ;
					if(t2<t1) bad = 1 ;
					if((t2-t1)>30) bad = 1 ;

					if(bad) printf("BAD: ") ;
					printf("\tpad %7.3f[%d,%d], time %7.3f[%d,%d], charge %5d, flags 0x%02X\n",
					       dd->cld[i].pad,dd->cld[i].p1,dd->cld[i].p2,
					       dd->cld[i].tb,dd->cld[i].t1,dd->cld[i].t2,
					       dd->cld[i].charge,dd->cld[i].flags) ;
					
				}
			}
		}

	
		// will only exist in token 0 of a pedestal run!
		dd = rdr->det("tpx")->get("pedrms",s) ;
		while(dd && dd->iterate()) {
			found = 1 ;
			ped_found = 1 ;

			s_mask[dd->sec-1]=1 ;

			if(do_print) {
				printf("TPX: sec %02d, row %2d, pad %3d (%d pix)\n",dd->sec,dd->row,dd->pad,dd->ncontent) ;
				daq_det_pedrms *ped = (daq_det_pedrms *)dd->Void ;
				for(u_int tb=0;tb<dd->ncontent;tb++) {
					printf("  tb %3d: ped %3d, rms %.2f\n",tb,ped[tb].ped,ped[tb].rms) ;
				}
			}
		}

		//new ALTRO bank, for a test
		for(int r=1;r<=6;r++) {
			dd = rdr->det("tpx")->get("altro",s,r) ;
			while(dd && dd->iterate()) {
				if(do_print) {
					printf("TPX ALTRO: sec %02d, RDO %d: ALTRO %3d, ch %2d: %3d pixels\n",dd->sec,r,dd->row,dd->pad,dd->ncontent) ;
				}
			}
		}

				

	}

	char fstr[128] ;
	fstr[0] = 0 ;	// EOS marker...

	if(cld_found) {
		strcat(fstr,"CLD ") ;
	}
	if(adc_found) {
		strcat(fstr,"ADC " ) ;
	}
	if(ped_found) {
		strcat(fstr,"PEDRMS ") ;
	}


	int s_cou = 0 ;
	for(int s=0;s<24;s++) {
		if(s_mask[s]) {
			char stmp[8] ;
			sprintf(stmp,"%d ",s+1) ;
			strcat(fstr,stmp) ;
			s_cou++ ;
		}
	}

	if(found) {
		LOG(INFO,"TPX found [%s;%d]",fstr,s_cou) ;
	}

	return found ;

}

static int ftp_doer(daqReader *rdr, const char *do_print)
{
  int found = 0;
  daq_dta *dd;
  
  if(strcasestr(do_print,"ftp")) ;	// leave as is...
  else do_print = 0 ;

  dd = rdr->det("ftp")->get("legacy") ;

  while(dd && dd->iterate()) {	
    found++ ;	// mark as found..
    ftp_t *ftp = (ftp_t *) dd->Void ;

    if(do_print) {
      printf("FTP sector %d: pixels %d\n",dd->sec,ftp->channels) ;
	   
		
      for(int ss=0;ss<2;ss++) {
	for(int rb=0;rb<10;rb++) {
	  for(int pad=0;pad<960;pad++) {
	    for(int tbi=0;tbi<ftp->counts[ss][rb][pad];tbi++) {
	      printf("%d %d %d %d %d\n",
		     ss,rb,pad,
		     ftp->timebin[ss][rb][pad][tbi],
		     ftp->adc[ss][rb][pad][tbi]);
	    }
	  }
	}
      }
    }
  }
  

  return found ;
}


static int tpc_doer(daqReader *rdr, const char  *do_print)
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
				for(int r=0;r<45;r++) {
					for(int c=0;c<tpc->cl_counts[r];c++) {
						printf("row %2d: pad %1.f, tb %.1f, charge %d\n",r+1,
						       tpc->cl[r][c].p,tpc->cl[r][c].t,tpc->cl[r][c].charge) ;
					}
				}
			}
			
			// one can rerun the afterburner as well with:

			//daq_tpc *tpc_class = (daq_tpc *)rdr->det("tpc") ;
			//int cl_found = tpc_class->fcfReader(dd->sec,0,0,tpc) ;
			//LOG(NOTE,"TPC: rerun cluster finder: sector %d: found %d clusters",dd->sec,cl_found) ;
		}
	}

	return found ;

	
}


static int pmd_doer(daqReader *rdr, const char  *do_print)
{
	int found = 0 ;
	daq_dta *dd ;

	if(strcasestr(do_print,"pmd")) ;	// leave as is...
	else do_print = 0 ;

	dd = rdr->det("pmd")->get("legacy") ;

	if(dd && dd->iterate()) {	
		struct pmd_t *pmd_p = (pmd_t *) dd->Void ;

		if(do_print) {
			for(int crate=0;crate<2;crate++) {
				printf("Crate %s: status %d, mode %d\n",crate==0?"Up":"Dn",pmd_p->status[crate],pmd_p->mode) ;

				for(int c=0;c<PMD_CRAMS_MAX;c++) {
				for(int s=0;s<2;s++) {
				for(int ch=0;ch<PMD_CRAMS_CH_MAX;ch++) {
					printf("  CRAM %2d: side %d: ch %4d: adc %4d [ped %4.1f, rms %4.2f, thr %4.1f]\n",
					       c,s,ch,
					       pmd_p->adc[crate][c][s][ch],
					       (double)pmd_p->ped[crate][c][s][ch]/16.0,
					       (double)pmd_p->rms[crate][c][s][ch]/16.0,
					       (double)pmd_p->thr[crate][c][s][ch]/16.0) ;

				}}}
			}

		}

		found = 1  ;	// mark as found..
	}

	if(found) LOG(NOTE,"PMD legacy found") ;

	return found ;

	
}
	
static int bsmd_doer(daqReader *rdr, const char *do_print)
{
	int found = 0 ;
	daq_dta *dd ;

	int raw_found = 0 ;
	int adc_non_zs_found = 0 ;
	int adc_found = 0 ;
	int ped_found = 0 ;

	if(strcasestr(do_print,"bsmd")) ;	// leave as is...
	else do_print = 0 ;


	// do I see the non-zero-suppressed bank? let's do this by fiber...
	for(int f=1;f<=12;f++) {
		dd = rdr->det("bsmd")->get("raw",0,f) ;	// sector is ignored (=0)
		if(dd) {
			while(dd->iterate()) {
				found = 1 ;
				raw_found = 1 ;

				if(do_print) printf("BSMD RAW: fiber %2d [==%d], sector %d:\n",dd->rdo,f,dd->sec) ;

				// just the header
				for(int i=0;i<10;i++) {
					if(do_print) printf("   Head %2d = %08X\n",i,dd->Int32[i]) ;
				}
			}
		}
	}


	// do I see the non-zero-suppressed bank? let's do this by fiber...
	for(int f=1;f<=12;f++) {
		dd = rdr->det("bsmd")->get("adc_non_zs",0,f) ;	// sector is ignored (=0)
		if(dd) {
			while(dd->iterate()) {
				found = 1 ;
				adc_non_zs_found = 1 ;

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
				adc_found = 1 ;

				bsmd_t *d = (bsmd_t *) dd->Void ;

				if(do_print) printf("BSMD ZS: fiber %2d, capacitor %d:\n",dd->rdo,d->cap) ;

				for(int i=0;i<BSMD_DATSIZE;i++) {
					// since this is zero-suppressed, I'll skip zeros...
					if(do_print) if(d->adc[i]) printf("   %4d = %4d\n",i,d->adc[i]) ;
				}
			}
		}
	}

	// do I see the pedestal bank?
	for(int f=1;f<=12;f++) {
		dd = rdr->det("bsmd")->get("ped",0,f) ;
		if(dd) {
			while(dd->iterate()) {
				found = 1 ;
				ped_found = 1 ;

				bsmd_t *d = (bsmd_t *) dd->Void ;

				if(do_print) printf("BSMD PED: fiber %2d, capacitor %d:\n",dd->rdo,d->cap) ;

				for(int i=0;i<BSMD_DATSIZE;i++) {
					if(do_print) printf("   %4d = %4d\n",i,d->adc[i]) ;
				}
			}
		}
	}

	// do I see the rms bank?
	for(int f=1;f<=12;f++) {
		dd = rdr->det("bsmd")->get("rms",0,f) ;
		if(dd) {
			while(dd->iterate()) {
				found = 1 ;
				ped_found = 1 ;

				bsmd_t *d = (bsmd_t *) dd->Void ;

				if(do_print) printf("BSMD RMS: fiber %2d, capacitor %d:\n",dd->rdo,d->cap) ;

				for(int i=0;i<BSMD_DATSIZE;i++) {
					if(do_print) printf("   %4d = %.2f\n",i,(double)d->adc[i]/8.0) ;
				}
			}
		}
	}


	char fstr[64] ;
	fstr[0] = 0 ;	// EOS marker...

	if(raw_found) {
		strcat(fstr,"RAW ") ;
	}

	if(adc_found) {
		strcat(fstr,"ADC-ZS ") ;
	}
	if(adc_non_zs_found) {
		strcat(fstr,"ADC-non-ZS " ) ;
	}
	if(ped_found) {
		strcat(fstr,"PEDRMS ") ;
	}

	if(found) {
		LOG(INFO,"BSMD found [%s]",fstr) ;
	}


	return found ;
}

static int esmd_doer(daqReader *rdr, const char *do_print)
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

static int etow_doer(daqReader *rdr, const char *do_print)
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

static int btow_doer(daqReader *rdr, const char *do_print)
{
	int found = 0 ;
	daq_dta *dd ;

	if(strcasestr(do_print,"btow")) ;	// leave as is...
	else do_print = 0 ;

/* generally commented out... */
#if 0
	dd = rdr->det("btow")->get("raw") ;
	if(dd) {
		while(dd->iterate()) {
			u_short *s16 = (u_short *) dd->Void ;

			if(do_print) {
				printf("BTOW: bytes %d\n",dd->ncontent) ;
			}

			for(u_int i=0;i<dd->ncontent/2;i++) {
				if(do_print) {
					printf("%d: 0x%04X [%d dec]\n",i,s16[i],s16[i]) ;	
				}
			}
		}
	}
#endif

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

#ifdef INSIST_ON_EMC_PSEUDO
static int emc_pseudo_doer(daqReader *rdr, const char *do_print)
{
	int found = 0 ;
	daq_dta *dd ;

	if(strcasestr(do_print,"emc_pseudo")) ;	// leave as is...
	else do_print = 0 ;


	dd = rdr->det("emc_pseudo")->get("legacy") ;
	if(dd) {
		while(dd->iterate()) {
			found = 1 ;

			emc_t *d = (emc_t *) dd->Void ;

			if(do_print) printf("BTOW found %d (%d ch), BSMD found %d (%d ch), ETOW found %d (%d ch), ESMD found %d (%d ch)\n",
					    d->btow_in, d->btow_ch, d->bsmd_in, d->bsmd_ch, d->etow_in, d->etow_ch, d->esmd_in, d->esmd_ch) ;
		}
	}

	return found ;

}
#endif


static int pp2pp_doer(daqReader *rdr, const char *do_print)
{
	int raw_found = 0 ;
	int adc_found = 0 ;
	int pedrms_found = 0 ;
	int adc_ped_sub_found = 0 ;

	daq_dta *dd ;

	if(strcasestr(do_print,"pp2pp")) ;	// leave as is...
	else do_print = 0 ;

	dd = rdr->det("pp2pp")->get("raw") ;
	if(dd) {
		while(dd->iterate()) {
			raw_found++ ;

			if(do_print) {
				printf("PP2PP: RAW: sector %d, RDO %d, bytes %d\n",dd->sec,dd->rdo,dd->ncontent) ;

				u_int *d32 = (u_int *)dd->Void ;
				for(int i=0;i<16;i++) {
					printf("   %2d: 0x%08X\n",i,d32[i]) ;

				}
			}

		}
	}


	dd = rdr->det("pp2pp")->get("adc") ;
	if(dd) {
		while(dd->iterate()) {
			adc_found++ ;

			pp2pp_t *d = (pp2pp_t *) dd->Void ;

			if(do_print) {
				int cou = 0 ;
				for(int c=0;c<PP2PP_SVX_CH;c++) {
					if(d->trace[c]) cou++ ;
				}
				
				printf("PP2PP: sector %d, sequencer %d, chain %c, SVX %d: %d channels:\n",dd->sec,d->seq_id,'A'+d->chain_id,d->svx_id,cou) ;
				for(int c=0;c<PP2PP_SVX_CH;c++) {
					// print only found channels via the "trace" array
					if(d->trace[c]) printf("   ch %3d: ADC %3d [0x%02X], trace %d\n",c,d->adc[c],d->adc[c],d->trace[c]) ;
					//printf("   ch %3d: ADC %3d [0x%02X], trace %d\n",c,d->adc[c],d->adc[c],d->trace[c]) ;
				}
			}

		}
	}

	dd = rdr->det("pp2pp")->get("adc_ped_sub") ;
	if(dd) {
		while(dd->iterate()) {
			adc_ped_sub_found++ ;

			pp2pp_t *d = (pp2pp_t *) dd->Void ;

			if(do_print) {
				int cou = 0 ;
				for(int c=0;c<PP2PP_SVX_CH;c++) {
					if(d->trace[c]) cou++ ;
				}
				
				printf("PP2PP PED_SUB: sector %d, sequencer %d, chain %c, SVX %d: %d channels:\n",dd->sec,d->seq_id,'A'+d->chain_id,d->svx_id,cou) ;

				for(int c=0;c<PP2PP_SVX_CH;c++) {
					// print only found channels via the "trace" array
					if(d->trace[c]) printf("   ch %3d: ADC %3d [0x%02X], trace %d\n",c,d->adc[c],d->adc[c],d->trace[c]) ;
					//printf("   ch %3d: ADC %3d [0x%02X], trace %d\n",c,d->adc[c],d->adc[c],d->trace[c]) ;
				}
			}

		}
	}

	dd = rdr->det("pp2pp")->get("pedrms") ;
	if(dd) {
		while(dd->iterate()) {
			pedrms_found++ ;

			pp2pp_pedrms_t *d = (pp2pp_pedrms_t *) dd->Void ;

			if(do_print) {
				printf("PP2PP PEDRMS: sector %d, sequencer %d, chain %c, SVX %d: SVX pedestal %f +- %f\n",dd->sec,d->seq_id,'A'+d->chain_id,d->svx_id,
				       d->svx_ped,d->svx_rms) ;

				for(int c=0;c<PP2PP_SVX_CH;c++) {
					printf("   ch %3d: ped %f, rms %f\n",c,d->ped[c],d->rms[c]) ;
				}
			}

		}
	}

	char fstr[256] ;
	fstr[0] = 0 ;

	if(raw_found) strcat(fstr,"RAW ") ;
	if(adc_found) strcat(fstr,"ADC ") ;
	if(adc_ped_sub_found) strcat(fstr,"ADC-PED-SUB ") ;
	if(pedrms_found) strcat(fstr,"PEDRMS ") ;

	int found = raw_found || adc_found || pedrms_found ;

	if(found) {
		LOG(INFO,"PP2PP found [%s]",fstr) ;
	}

	return found ;
}

static int l3_doer(daqReader *rdr, const char *do_print)
{
	int found = 0 ;
	daq_dta *dd ;

	if(strcasestr(do_print,"l3")) ;	// leave as is...
	else do_print = 0 ;


	dd = rdr->det("l3")->get("legacy") ;
	if(dd) {
		while(dd->iterate()) {
			found = 1 ;

			l3_t *l3_p = (l3_t *) dd->Void ;

			if(do_print) {
				printf("L3/HLT: sequence %u, decision 0x%X: tracks %d, clusters %d, vertex %f:%f:%f\n",
				       l3_p->channels, l3_p->mode, // note comment in daq_l3.h!
				       l3_p->tracks_num, l3_p->cluster_num,
				       l3_p->xVertex, l3_p->yVertex, l3_p->xVertex) ;


				for(u_int i=0;i<l3_p->tracks_num;i++) {
					// just an example of what one would print out...
					printf("  track %d: Pt %f, charge %d, nHits %d\n",i+1, 
					       l3_p->track[i].pt, l3_p->track[i].q, l3_p->track[i].nHits) ;

				}
			}

		}
	}

	return found ;
}

static int fgt_doer(daqReader *rdr, const char *do_print, int which)
{
	int found = 0 ;
	char s_found[128] ;
	daq_dta *dd ;

	const char *d_name = 0 ;


	switch(which) {
	case GMT_ID :
		d_name = "GMT" ;
		break ;
	case IST_ID :
		d_name = "IST" ;
		break ;
	case FGT_ID :
	default :
		d_name = "FGT" ;
	}


	if(strcasestr(do_print,d_name)) ;	// leave as is...
	else do_print = 0 ;

	s_found[0] = 0 ;

	dd = rdr->det(d_name)->get("raw") ;
	if(dd) {
		while(dd->iterate()) {
			found |= 1 ;

			// point to the start of the DDL raw data
			u_int *d = (u_int *) dd->Void ;	


			if(do_print) {
				printf("%s RAW: RDO %d: %d bytes, %d words\n",d_name,dd->rdo,dd->ncontent,dd->ncontent/4) ;
				// dump a few ints
				for(int i=0;i<10;i++) {
					printf(" %3d: 0x%08X\n",i,d[i]) ;
				}
			}

		}
	}


	// one can get the data in the electronics/logical layout
	dd = rdr->det(d_name)->get("adc") ;

	// let's dump the meta-data first
	if(dd && dd->meta && do_print) {
		apv_meta_t *meta = (apv_meta_t *)dd->meta ;
				
		printf("%s meta data:\n",d_name) ;
		for(int r=1;r<=FGT_RDO_COU;r++) {
			if(meta->arc[r].present == 0) continue ;
				
			printf("  ARC %d: error %c; format %d, ARM mask 0x%X\n",r,meta->arc[r].error?'Y':'N',
			       meta->arc[r].format_code,
			       meta->arc[r].arm_mask) ;

			for(int arm=0;arm<FGT_ARM_COU;arm++) {
				if(meta->arc[r].arm[arm].present == 0) continue ;

				printf("    ARM %d: error %c\n",arm,meta->arc[r].arm[arm].error?'Y':'N') ;
				printf("         : arm_id %d, arm_seq %d, arm_err %d, apv_mask 0x%X\n",
				       meta->arc[r].arm[arm].arm_id,
				       meta->arc[r].arm[arm].arm_seq,
				       meta->arc[r].arm[arm].arm_err,
				       meta->arc[r].arm[arm].apv_mask) ;

				for(int apv=0;apv<FGT_APV_COU;apv++) {
					if(meta->arc[r].arm[arm].apv[apv].present == 0) continue ;
							
					printf("      APV %2d: error %c\n",apv,meta->arc[r].arm[arm].apv[apv].error?'Y':'N') ;
					printf("            : apv_id %d, fmt %d, length %d, seq %d, capid %d, nhits %d, is_error %d, refadc %d, ntim %d\n",
					       meta->arc[r].arm[arm].apv[apv].apv_id,
					       meta->arc[r].arm[arm].apv[apv].fmt,
					       meta->arc[r].arm[arm].apv[apv].length,
					       meta->arc[r].arm[arm].apv[apv].seq,
					       meta->arc[r].arm[arm].apv[apv].capid,
					       meta->arc[r].arm[arm].apv[apv].nhits,
					       meta->arc[r].arm[arm].apv[apv].is_error,
					       meta->arc[r].arm[arm].apv[apv].refadc,
					       meta->arc[r].arm[arm].apv[apv].ntim) ;
				}
			}
		}

	}

	
	while(dd && dd->iterate()) {
		found |= 2 ;

		fgt_adc_t *f = (fgt_adc_t *) dd->Void ;


		if(do_print) {
			printf("%s ADC: RDO %d, ARM %d, APV %d: %d values\n",d_name,dd->rdo,dd->sec,dd->pad,dd->ncontent) ;

			for(u_int i=0;i<dd->ncontent;i++) {
				printf(" %5d: ch %3d, tb %d = %3d\n",i,f[i].ch,f[i].tb,f[i].adc) ;
			}
		}
	}
				





	// one can get the data in the electronics/logical layout
	dd = rdr->det(d_name)->get("zs") ;
	if(dd) found |= 4 ;

	// let's dump the meta-data first
	if(dd && dd->meta && do_print) {
		apv_meta_t *meta = (apv_meta_t *)dd->meta ;
				
		printf("%s ZS meta data:\n",d_name) ;
		for(int r=1;r<=FGT_RDO_COU;r++) {
			if(meta->arc[r].present == 0) continue ;
				
			printf("  ARC %d: error %c; format %d, ARM mask 0x%X\n",r,meta->arc[r].error?'Y':'N',
			       meta->arc[r].format_code,
			       meta->arc[r].arm_mask) ;

			for(int arm=0;arm<FGT_ARM_COU;arm++) {
				if(meta->arc[r].arm[arm].present == 0) continue ;

				printf("    ARM %d: error %c\n",arm,meta->arc[r].arm[arm].error?'Y':'N') ;
				printf("         : arm_id %d, arm_seq %d, arm_err %d, apv_mask 0x%X\n",
				       meta->arc[r].arm[arm].arm_id,
				       meta->arc[r].arm[arm].arm_seq,
				       meta->arc[r].arm[arm].arm_err,
				       meta->arc[r].arm[arm].apv_mask) ;

				for(int apv=0;apv<FGT_APV_COU;apv++) {
					if(meta->arc[r].arm[arm].apv[apv].present == 0) continue ;
							
					printf("      APV %2d: error %c\n",apv,meta->arc[r].arm[arm].apv[apv].error?'Y':'N') ;
					printf("            : apv_id %d, fmt %d, length %d, seq %d, capid %d, nhits %d, is_error %d, refadc %d, ntim %d\n",
					       meta->arc[r].arm[arm].apv[apv].apv_id,
					       meta->arc[r].arm[arm].apv[apv].fmt,
					       meta->arc[r].arm[arm].apv[apv].length,
					       meta->arc[r].arm[arm].apv[apv].seq,
					       meta->arc[r].arm[arm].apv[apv].capid,
					       meta->arc[r].arm[arm].apv[apv].nhits,
					       meta->arc[r].arm[arm].apv[apv].is_error,
					       meta->arc[r].arm[arm].apv[apv].refadc,
					       meta->arc[r].arm[arm].apv[apv].ntim) ;
				}
			}
		}

	}

	int charge_sum = 0 ;

	while(dd && dd->iterate()) {
		found |= 4 ;

		fgt_adc_t *f = (fgt_adc_t *) dd->Void ;

		for(u_int i=0;i<dd->ncontent;i++) {
			if(f[i].adc > 0) {
				charge_sum += f[i].adc ;
			}
		}


		if(do_print) {
			printf("%s ZS: RDO %d, ARM %d, APV %d: %d values\n",d_name,dd->rdo,dd->sec,dd->pad,dd->ncontent) ;

			for(u_int i=0;i<dd->ncontent;i++) {
				printf(" %5d: ch %3d, tb %d = %3d\n",i,f[i].ch,f[i].tb,f[i].adc) ;
			}
		}
	}
	
	if((found & 4) && do_print) {
		printf("+++ Charge sum %d\n",charge_sum) ;
	}

	dd = rdr->det(d_name)->get("pedrms") ;
	while(dd && dd->iterate()) {
		found |= 8 ;

		fgt_pedrms_t *f = (fgt_pedrms_t *) dd->Void ;


		if(do_print) {
			int arc = dd->rdo ;
			int arm = dd->sec ;
			int apv = dd->pad ;

			printf("%s PEDRMS: RDO %d, ARM %d, APV %d: %d values\n",d_name,dd->rdo,dd->sec,dd->pad,dd->ncontent) ;

			for(u_int i=0;i<dd->ncontent;i++) {
				printf("%d %d %2d %3d %2d: %.3f +- %.3f\n",arc,arm,apv,f[i].ch,f[i].tb,f[i].ped,f[i].rms) ;
			}
		}

		

	}

	if(found & 1) {
		strcat(s_found,"RAW ") ;
	}
	if(found & 2) {
		strcat(s_found,"ADC ") ;
	}
	if(found & 4) {
		strcat(s_found,"ZS ") ;
	}
	if(found & 8) {
		strcat(s_found,"PEDRMS ") ;
	}

	if(found) LOG(INFO,"%s found: %s",d_name,s_found) ;

	return found ;
}

static int mtd_doer(daqReader *rdr, const char *do_print)
{
	int found = 0 ;
	daq_dta *dd ;

	if(strcasestr(do_print,"mtd")) ;	// leave as is...
	else do_print = 0 ;


	// right now only the "raw" pointer is available/known
	dd = rdr->det("mtd")->get("raw") ;
	if(dd) {
		while(dd->iterate()) {
			found = 1 ;

			// point to the start of the DDL raw data
			u_int *d = (u_int *) dd->Void ;	


			if(do_print) {
				printf("MTD: RDO %d: %d bytes\n",dd->rdo,dd->ncontent) ;
				
				for(u_int i=0;i<dd->ncontent/4;i++) {
					printf(" %2d: 0x%08X\n",i,d[i]) ;
				}
			}

		}
	}

	return found ;
}




// This is called by tinfo flag:

static int tinfo_doer(daqReader *rdr, const char *do_print)
{
  int found = 0;

  daq_dta *dd = rdr->det("trg")->get("raw") ;
  if(dd) {
    if(dd->iterate()) {
      found = 1;

//      int sz = dd->get_size_t();
      TriggerDataBlk *trg = (TriggerDataBlk *)dd->Byte;

//      EvtDescData *evtDesc = (EvtDescData *)(((char *)trg) + swap32(trg->EventDesc_ofl.offset));
      TrgSumData *trgSum = (TrgSumData *)(((char *)trg) + swap32(trg->Summary_ofl.offset));
      L1_DSM_Data *l1Dsm = (L1_DSM_Data *)(((char *)trg) + swap32(trg->L1_DSM_ofl.offset));

      printf("L1 trg = 0x%x-%x\n",swap32(trgSum->L1Sum[1]),swap32(trgSum->L1Sum[0]));
      printf("L2 trg = 0x%x-%x\n",swap32(trgSum->L2Sum[1]),swap32(trgSum->L2Sum[0]));
      for(int i=0;i<64;i++) {
	printf("L2Result[%d]=0x%x\n",i,swap32(trgSum->L2Result[i]));
      }
      for(int i=0;i<8;i++) {
	printf("lastDsm[%d] = 0x%x\n",i,swap16(l1Dsm->lastDSM[i]));
      }
      printf("ids: ");
      for(int i=0;i<64;i++) {
	  if(rdr->daqbits64 & (1ll << i)) {
	      printf("{%d}",rdr->getOfflineId(i));
	  }
      }
      printf("\n");
    }
  }
  
  return found;
}



static int pxl_doer(daqReader *rdr, const char *do_print)
{
	int found = 0 ;
	daq_dta *dd ;

	if(strcasestr(do_print,"pxl")) ;	// leave as is...
	else do_print = 0 ;


	// right now only the "raw" pointer is available/known
	dd = rdr->det("pxl")->get("raw") ;
	if(dd) {
		while(dd->iterate()) {
			found = 1 ;

			// point to the start of the DDL raw data
			u_int *d = (u_int *) dd->Void ;	


			if(do_print) {
				printf("PXL RAW: Sector %d, RDO %d: %d bytes (%d words)\n",dd->sec,dd->rdo,dd->ncontent,dd->ncontent/4) ;
				// dump a few
				int cou = dd->ncontent/4 ;

				if(cou > 30) cou = 30 ;

				for(int i=0;i<cou;i++) {
					printf(" %2d: 0x%08X\n",i,d[i]) ;
				}
			}

		}
	}



	return found ;
}

static int sst_doer(daqReader *rdr, const char *do_print)
{
	int found = 0 ;
	daq_dta *dd ;

	if(strcasestr(do_print,"sst")) ;	// leave as is...
	else do_print = 0 ;



	dd = rdr->det("sst")->get("raw") ;
	if(dd) {
		while(dd->iterate()) {
			found |= 1 ;

			// point to the start of the DDL raw data
			u_int *d = (u_int *) dd->Void ;	


			if(do_print) {
				printf("SST RAW: Sector %d, RDO %d: %d bytes (%d words)\n",dd->sec,dd->rdo,dd->ncontent,dd->ncontent/4) ;
				// dump a few
				int cou = dd->ncontent/4 ;

				if(cou > 30) cou = 30 ;

				for(int i=0;i<cou;i++) {
					printf(" %2d: 0x%08X\n",i,d[i]) ;
				}
			}

		}
	}


	dd = rdr->det("sst")->get("adc") ;
	if(dd) {
		while(dd->iterate()) {
			found |= 2 ;
			
			daq_sst_data_t *sst = (daq_sst_data_t *)dd->Void ;

			
			if(do_print) {

				printf("SST ADC: Sector %d, RDO %d, fiber %d: %d ADCs\n",dd->sec,dd->rdo,dd->pad,dd->ncontent) ;
			
				for(u_int i=0;i<dd->ncontent;i++) {
					printf("   Strip %3d, hybrid %2d: %4d\n",sst[i].strip,sst[i].hybrid,sst[i].adc) ;
				}
			}

		}
	}

	dd = rdr->det("sst")->get("pedrms") ;
	if(dd) {
		while(dd->iterate()) {
			found |= 4 ;
			
			daq_sst_pedrms_t *sst = (daq_sst_pedrms_t *)dd->Void ;

			
			if(do_print) {

				printf("SST PEDRMS: Sector %d, RDO %d, fiber %d: %d vals\n",dd->sec,dd->rdo,dd->pad,dd->ncontent) ;
			
				for(u_int i=0;i<dd->ncontent;i++) {
					for(int h=0;h<SST_HYBRID_COU;h++) {
					for(int s=0;s<SST_STRIP_COU;s++) {
						printf("     ped %d, rms %.3f\n",sst->ped[h][s],(float)sst->rms[h][s]/16.0) ;
					}
					}
				}
			}

		}
	}



	char s_found[128] ;
	s_found[0] = 0 ;
	
	if(found & 1) {
		strcat(s_found,"RAW ") ;
	}
	if(found & 2) {
		strcat(s_found,"ADC ") ;
	}
	if(found & 4) {
		strcat(s_found,"PEDRMS ") ;
	}

	if(found) LOG(INFO,"SST: found %s",s_found) ;

	sst_test(rdr,1) ;

	return found ;
}



/* various test routines... typically used by Tonko only */
static int fgt_test(daqReader *rdr, const char *do_print, int which)
{
//#define DO_FGT_TEST
#ifdef DO_FGT_TEST
	fgtPed *ped = new fgtPed() ;

	ped->tb_cou_xpect = 9 ;
	ped->init(0x3F, which) ;
	ped->from_cache("/net/ist01/RTScache/pedestals.txt") ;
	ped->bad_from_cache() ;
	double perc_bad = ped->do_thresh(5.0,2) ;

	LOG(INFO,"IST: bad channels %.1f%%",perc_bad) ;
#endif
	return 0 ;
}

static int sst_test(daqReader *rdr, int mode)
{
//#define DO_SST_TEST
#ifdef DO_SST_TEST
	static sstPed *ped[2] ;

	LOG(TERR,"sst_test: mode %d",mode) ;

	if(ped[0]==0) {
		ped[0] = new sstPed() ;
		ped[0]->init(0x7) ;
		ped[0]->sector = 1 ;

		ped[1] = new sstPed() ;
		ped[1]->init(0x3) ;
		ped[1]->sector = 2 ;

	}

	if(mode==1) {
		daq_dta *dd = rdr->det("sst")->get("raw") ;

		LOG(NOTE,"Here") ;

		while(dd && dd->iterate()) {
			// this part works in non-ped runs
			((daq_sst *)(rdr->det("sst")))->raw_to_adc_utility(dd->sec,dd->rdo,(char *)dd->Void,dd->ncontent/4,0,0) ;


			ped[dd->sec-1]->accum((char *)dd->Void, dd->ncontent, dd->rdo) ;

			LOG(NOTE,"Accum done") ;

		}
	}
	else {
		ped[0]->calc() ;
		ped[1]->calc() ;

		ped[0]->to_cache("/log",run_number) ;
		ped[1]->to_cache("/log",run_number) ;
	}

#endif
	return 0 ;
}



static int fps_doer(daqReader *rdr, const char *do_print)
{
	int adc_found = 0 ;
	int pedrms_found = 0 ;
//	const double cpu_clock = 2166872000.0 ;
	const double cpu_clock = 2166855000.0 ;
	daq_dta *dd ;

	if(strcasestr(do_print,"fps")) ;	// leave as is...
	else do_print = 0 ;


	dd = rdr->det("fps")->get("adc") ;
	if(dd) {
		while(dd->iterate()) {	//per xing and per RDO
			adc_found |= 1 << (dd->sec - 1) ;	//sector mask

			u_int ev_ch = 0 ;	//count of channels

			
			fps_evt_hdr_t *hdr = (fps_evt_hdr_t *)dd->meta ;



			fps_adc_t *a = (fps_adc_t *)dd->Void ;

			if(do_print) {
				double occ = 100.0 * (double)dd->ncontent / (double)32 ;

				int sum = 0 ;

				for(u_int i=0;i<dd->ncontent;i++) {
					sum += a[i].adc ;
				}
				
				printf("FPS sector %d: xing %2d, QT %d, chs %d (occupancy %.2f %%, charge %d)\n",dd->sec,(char)dd->row,dd->rdo,dd->ncontent,occ,sum) ;
		
				int xing = (char)dd->row ;

				//int ix = xing + 10 ;

				//if(ix>=0 && ix<21) tot_charge[ix] += sum ;

				if(xing==0) ev_ch += dd->ncontent;;

			}



			for(u_int i=0;i<dd->ncontent;i++) {
				if(do_print) {
					printf("    ch %2d: ADC %4d, TDC %2d\n",a[i].ch, a[i].adc, a[i].tdc) ;
					
				}
			}
		

			// moved it to the end, when I know the channel count
			if(do_print) {
				//time of arrival of the STP command
				double stp = ((double) hdr->tick) * 1024.0 ;	//in clocks

				stp /= cpu_clock ;
				stp *= 1000000.0 ;

				//time at the end of the full event readout
				double readout = ((double) (u_int) hdr->delta) * 1024.0 ;

				readout /= cpu_clock ;
				readout *= 1000000.0 ;
			
				double just_readout = ((double)(u_int) hdr->reserved[0]) * 1024.0 ;
				just_readout /= cpu_clock ;
				just_readout *= 1000000.0 ;

				printf("FPS Sector %d META: time of STP-arrival %.1f us, time of End-of-Readout %.1f us (delta %.1f us, just readout %.1f us), ch count  %u\n",
				       dd->sec,stp, readout, readout-stp, readout-just_readout, ev_ch) ;
			}


		}

	}


	dd = rdr->det("fps")->get("pedrms") ;
	if(dd) {
		while(dd->iterate()) {
			pedrms_found = 1 ;

			fps_pedrms_t *ped = (fps_pedrms_t *) dd->Void ;

			if(do_print) {
				printf("FPS PEDRMS: Sector %d: QT %d\n",dd->sec,dd->rdo) ;

				for(int i=0;i<ped->ch_cou;i++) {
					printf("    ch %2d: %f +- %f (bad 0x%X)\n",i,
					       ped->ped[i].ped,ped->ped[i].rms,ped->ped[i].flag) ;
				}
			}
			
		}
	}


	char fstr[256] ;
	fstr[0] = 0 ;

	if(adc_found | 1) strcat(fstr,"FPS-ADC ") ;
	if(adc_found | 2) strcat(fstr,"FPOST-ADC ") ;

	if(pedrms_found) strcat(fstr,"PEDRMS ") ;

	int found = 0 ;
	if(adc_found || pedrms_found) found = 1 ;


	if(found) {
		LOG(INFO,"FPS found: [%s]",fstr) ;
	}

	return found ;

}

static int rhicf_doer(daqReader *rdr, const char *do_print)
{
	int found = 0 ;
	daq_dta *dd ;

	if(strcasestr(do_print,"rhicf")) ;	// leave as is...
	else do_print = 0 ;

	dd = rdr->det("rhicf")->get("raw") ;
	

	if(dd) {
		while(dd->iterate()) {	//per xing and per RDO
			found = 1 ;

			if(do_print) {
				printf("RHICF: %d bytes\n",dd->ncontent) ;

				for(int i=0;i<10;i++) {
					printf("    %d: 0x%08X\n",i,dd->Int32[i]) ;
				}
			}

		}

	}


	if(found) {
		LOG(INFO,"RHICF found") ;
	}

	return found ;

}
