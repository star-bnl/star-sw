//******************************************************
//*** Chops events out of .daq file 
//*** 
//*** usage:   daqFileChopper fn.daq arglist
//***
//***          arglist is passed to the function
//***          FilterEvent(), which returns true
//***          if the event is to be saved
//***     
//***          output goes to standard out...
//*****************************************************

#include <stdio.h>
#include <unistd.h>
#include <getopt.h>
#include <sys/types.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <rtsLog.h>	// for my LOG() call
#include <rtsSystems.h>

#include <daqFormats.h>
#include <SFS/sfs_index.h>

// this needs to be always included
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_det.h>
#include <DAQ_READER/daq_dta.h>
#include <DAQ_BTOW/daq_btow.h>




struct JmlSz {
    int cld_pix;
    int cld_cl;
    UINT64 cld_adc;
    int adc_pix;
    UINT64 adc_adc;

    int invalid_count;
};




void initHack() {
}

void doHack(daqReader *rdr) {
    struct JmlSz tpxSz;
    struct JmlSz itpcSz;
    int sz = 0;
    UINT64 bx64 = 0;
    int bx7 = 0;
    
    
    
    memset(&tpxSz, 0, sizeof(tpxSz));
    memset(&itpcSz, 0, sizeof(itpcSz));
    sz = rdr->event_size;
    
    // Only minbias events!
    if((rdr->daqbits64 & (1ll << 1)) == 0) return;

    // get bx64
    daq_dta *dd = rdr->det("trg")->get("raw");
    if(dd) {
	if(dd->iterate()) {
	    TriggerDataBlk *trg = (TriggerDataBlk *)dd->Byte;

	    EvtDescData *evtDesc = (EvtDescData *)(((char *)trg) + swap32(trg->EventDesc_ofl.offset));
	    UINT32 bx_high = swap32(evtDesc->bunchXing_hi);
	    UINT32 bx_low = swap32(evtDesc->bunchXing_lo);
	    bx64 = bx_high;
	    bx64 = (bx64 << 32) + bx_low;
	    bx7 = bx64 % 120;
	    //printf("%llu %d\n", bx64, bx7);
	}
    }

   
    for(int s=1;s<=24;s++) {

	// Get itpc ADC data
	dd = rdr->det("itpc")->get("adc", s);
	if(dd) {
	    while(dd->iterate()) {
		for(int i=0;i<dd->ncontent;i++) {
		    itpcSz.adc_pix++;
		    itpcSz.adc_adc += dd->adc[i].adc;
		}
	    }
	}
    
	// Get itpc CLD data
	dd = rdr->det("itpc")->get("cld", s);
	if(dd) {
	    while(dd->iterate()) {
		for(int i=0;i<dd->ncontent;i++) {
		    itpcSz.cld_cl++;
		    itpcSz.cld_adc += dd->cld[i].charge;
	
		    if(dd->cld[i].t2 <= dd->cld[i].t1) {
			itpcSz.invalid_count++;
			
			// printf("jjj itpc: sec=%d row=%d pad=%lf tb=%lf charge=%d  (%d = %d)\n", 
			//        s, 
			//        dd->row, 
			//        dd->cld[i].pad, 
			//        dd->cld[i].tb, 
			//        dd->cld[i].charge, 
			//        dd->cld[i].t1, 
			//        dd->cld[i].t2);
		    }
		    else {
			itpcSz.cld_pix += dd->cld[i].t2 - dd->cld[i].t1 + 1;
		    }
		}
	    }
	}

	
    	// Get tpx ADC data
	dd = rdr->det("tpx")->get("adc", s);
	if(dd) {
	    while(dd->iterate()) {
		for(int i=0;i<dd->ncontent;i++) {
		    tpxSz.adc_pix++;
		    tpxSz.adc_adc += dd->adc[i].adc;
		}
	    }
	}
    
	// Get tpx CLD data
	dd = rdr->det("tpx")->get("cld", s);
	if(dd) {
	    while(dd->iterate()) {
		for(int i=0;i<dd->ncontent;i++) {
		    tpxSz.cld_cl++;
		    tpxSz.cld_adc += dd->cld[i].charge;
		    if(dd->cld[i].t2 <= dd->cld[i].t1) {
			tpxSz.invalid_count++;
			//printf("jjj tpx: sec=%d row=%d pad=%lf tb=%lf charge=%d  (%d = %d)\n", s, dd->row, dd->cld[i].pad, dd->cld[i].tb, dd->cld[i].charge, dd->cld[i].t1, dd->cld[i].t2);
		    }
		    else {
			tpxSz.cld_pix += dd->cld[i].t2 - dd->cld[i].t1 + 1;
		    }
		}
	    }
	}
    }

    printf("%d %llu %d %d %llu %d %d %llu %d %d %llu %d %d %llu %d\n",
	   rdr->seq,          //1
	   bx64,              //2
	   bx7,               //3
	   itpcSz.adc_pix,    //4
	   itpcSz.adc_adc,    //5
	   itpcSz.cld_cl,     //6
	   itpcSz.cld_pix,    //7
	   itpcSz.cld_adc,    //8
	   itpcSz.invalid_count, // 9
	   tpxSz.adc_pix,     //10
	   tpxSz.adc_adc,     //11
	   tpxSz.cld_cl,      //12
	   tpxSz.cld_pix,     //13
	   tpxSz.cld_adc,    //14
	   tpxSz.invalid_count); // 15
         
}

void finishHack() {
}



void displayHelp()
{
    LOG(ERR,"Usage:  daqFileHacker filename"); 
}

int main(int argc, char *argv[])
{
    rtsLogOutput(RTS_LOG_STDERR) ;
    rtsLogLevel(WARN) ;

    if(argc < 2) {
	displayHelp();
	exit(0);
    }

    initHack();

    for(int file=1;file<argc;file++) {
	daqReader *evp;
	evp = new daqReader(argv[file]) ;	// create it with the filename argument..

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
		    LOG(OPER, "Done after scanning %d events (%d bad)",good,bad);
		    break;        // file, we're done...
		case EVP_STAT_EVT:
		    bad++;
		    LOG(WARN, "Problem getting event - skipping [good %d, bad %d]",good,bad);
		    continue;
		case EVP_STAT_CRIT:
		    LOG(CRIT,"evp->status CRITICAL (?)") ;
		    return -1;
		}
	    }
	    
	    if(evp->status == EVP_STAT_EOR) {
		LOG(INFO,"Done after scanning %d events (%d bad)",good,bad) ;
		break; 
	    }
	    
	    doHack(evp);
	}
	
	delete evp;
    }

    finishHack();
    return 0 ;
}
