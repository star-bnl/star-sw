#include <stdio.h>
#include <unistd.h>
#include <getopt.h>
#include <sys/types.h>
#include <stdlib.h>

#include <rtsLog.h>	// for my LOG() call
#include <rtsSystems.h>

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
static int ftp_doer(daqReader *rdr, char *do_print) ;
static int pmd_doer(daqReader *rdr, char *do_print) ;

static int emc_pseudo_doer(daqReader *rdr, char *do_print) ;
static int pp2pp_doer(daqReader *rdr, char *do_print) ;
static int l3_doer(daqReader *rdr, char *do_print) ;

int main(int argc, char *argv[])
{
	extern char *optarg ;
	extern int optind ;
	int c ;
	char *print_det = "" ;
	char _mountpoint[256];
	char *mountpoint = NULL;

	rtsLogOutput(RTS_LOG_STDERR) ;
	rtsLogLevel(WARN) ;


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
		  strcpy(mountpoint, optarg);
		  break;
		  
		default :
			break ;
		}
	}

	class daqReader *evp ;			// tha main guy
	evp = new daqReader(argv[optind]) ;	// create it with the filename argument..
	if(mountpoint) {
	  evp->setEvpDisk(mountpoint);
	}

	int good=0;
	int bad=0;
	
	for(;;) {
	        char *ret = evp->get(0,EVP_TYPE_ANY);
       
		if(ret) {
		  if(evp->status) {
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
		      continue;
		    }
		    break;        // file, we're done...
		  case EVP_STAT_EVT:
		    bad++;
		    LOG(WARN, "Problem getting event - skipping [good %d, bad %d]",good,bad);
		    sleep(1);
		    continue;
		  case EVP_STAT_CRIT:
		    LOG(CRIT,"evp->status CRITICAL (?)") ;
		    return -1;
		  }
		}

		
		if(evp->status == EVP_STAT_EOR) {
			LOG(INFO,"End of File reached...") ;
			break ;	// of the for() loop...
		}

		daq_dta *dd ;	// generic data pointer; reused all the time


		LOG(INFO,"sequence %d: token %4d, trgcmd %2d, daqcmd %2d, time %u, detectors 0x%08X (status 0x%X)",evp->seq, evp->token, evp->trgcmd, evp->daqcmd,
		    evp->evt_time, evp->detectors, evp->status) ;


		if(print_det[0]) printf("***** Seq #%d, token %d\n",evp->seq,evp->token) ;
		/***************** let's do simple detectors; the ones which only have legacy *****/

		if(print_det[0]) {
		  if(strcmp(print_det, "tinfo") == 0) {		    
		    printf("trginfo: seq = #%d  token = %d detectors = 0x%x triggers = 0x%x\n",
			   evp->seq,
			   evp->token,
			   evp->detectors,
			   evp->daqbits);
		  }
		}

		dd = evp->det("sc")->get() ;
		if(dd && dd->iterate()) {
			LOG(INFO,"SC found") ;
			if(strcasecmp(print_det,"sc")==0) {
				sc_t *sc_p = (sc_t *) dd->Void ;

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

		/*************************** PP2PP **********************/
		if(pp2pp_doer(evp,print_det)) LOG(INFO,"PP2PP found") ;


		/*************************** L3/HLT **************************/
		if(l3_doer(evp,print_det)) LOG(INFO,"L3/HLT found") ;

		/************  PSEUDO: SHOULD ONLY BE USED FOR BACKWARD COMPATIBILITY! ************/
#ifdef INSIST_ON_EMC_PSEUDO
		if(emc_pseudo_doer(evp,print_det)) LOG(INFO,"EMC found (any detector)") ;
#endif

	}

	delete evp ;	// cleanup i.e. if running through a set of files.

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


static int tpx_doer(daqReader *rdr, char  *do_print)
{
	int found = 0 ;
	daq_dta *dd ;

	if(strcasestr(do_print,"tpx")) ;	// leave as is...
	else do_print = 0 ;

	// TPX
	// it is better, more memory efficient, to call stuff sector by sector (if possible)

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


		dd = rdr->det("tpx")->get("cld",s) ;
		while(dd && dd->iterate()) {

			found = 1 ;


			if(do_print) {
				printf("TPX: sec %02d, row %2d: %3d clusters\n",dd->sec,dd->row,dd->ncontent) ;
			}
			
			for(u_int i=0;i<dd->ncontent;i++) {
				if(do_print) {
					printf("\tpad %7.3f, time %7.3f, charge %5d, flags 0x%02X\n",dd->cld[i].pad,dd->cld[i].tb,dd->cld[i].charge,dd->cld[i].flags) ;
				}
			}
		}

	
		// will only exist in token 0 of a pedestal run!
		dd = rdr->det("tpx")->get("pedrms",s) ;
		while(dd && dd->iterate()) {
			found = 1 ;
			if(do_print) {
				printf("TPX: sec %02d, row %2d, pad %3d (%d pix)\n",dd->sec,dd->row,dd->pad,dd->ncontent) ;
				daq_det_pedrms *ped = (daq_det_pedrms *)dd->Void ;
				for(u_int tb=0;tb<dd->ncontent;tb++) {
					printf("  tb %3d: ped %3d, rms %.2f\n",tb,ped[tb].ped,ped[tb].rms) ;
				}
			}
		}

	}

	return found ;

}

static int ftp_doer(daqReader *rdr, char *do_print)
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


static int pmd_doer(daqReader *rdr, char  *do_print)
{
	int found = 0 ;
	daq_dta *dd ;

	if(strcasestr(do_print,"pmd")) ;	// leave as is...
	else do_print = 0 ;

	dd = rdr->det("pmd")->get("legacy") ;

	if(dd && dd->iterate()) {	
		struct pmd_t *pmd_p = (pmd_t *) dd->Void ;

		if(do_print) {
			printf("PMD statuses %d %d: channels %d\n",pmd_p->status[0],pmd_p->status[1],pmd_p->channels) ;
		}

		found = 1  ;	// mark as found..
	}

	if(found) LOG(NOTE,"PMD legacy found") ;

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
		dd = rdr->det("bsmd")->get("raw",0,f) ;	// sector is ignored (=0)
		if(dd) {
			while(dd->iterate()) {
				found = 1 ;


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

	// do I see the pedestal bank?
	for(int f=1;f<=12;f++) {
		dd = rdr->det("bsmd")->get("ped",0,f) ;
		if(dd) {
			while(dd->iterate()) {
				found = 1 ;

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

				bsmd_t *d = (bsmd_t *) dd->Void ;

				if(do_print) printf("BSMD RMS: fiber %2d, capacitor %d:\n",dd->rdo,d->cap) ;

				for(int i=0;i<BSMD_DATSIZE;i++) {
					if(do_print) printf("   %4d = %.2f\n",i,(double)d->adc[i]/8.0) ;
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

static int emc_pseudo_doer(daqReader *rdr, char *do_print)
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


static int pp2pp_doer(daqReader *rdr, char *do_print)
{
	int found = 0 ;
	daq_dta *dd ;

	if(strcasestr(do_print,"pp2pp")) ;	// leave as is...
	else do_print = 0 ;


	dd = rdr->det("pp2pp")->get("adc") ;
	if(dd) {
		while(dd->iterate()) {
			found = 1 ;

			pp2pp_t *d = (pp2pp_t *) dd->Void ;

			if(do_print) {
				printf("PP2PP: sector %d, seq %d, chain %d, SVX %d:\n",dd->sec,d->seq_id,d->chain_id,d->svx_id) ;
				for(int c=0;c<PP2PP_SVX_CH;c++) {
					// print only found channels via the "trace" array
					if(d->trace[c]) printf("   %3d: %3d [0x%02X], trace %d\n",c,d->adc[c],d->adc[c],d->trace[c]) ;
				}
			}

		}
	}

	return found ;
}

static int l3_doer(daqReader *rdr, char *do_print)
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
