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
#define BTOW_CALC
#ifdef BTOW_CALC

#define BTOW_ADC_MAX 4096
#define BTOW_CHAN_MAX 9600

UINT64 bht3 = 0x800;
UINT64 bht1vpd = 0x8000;

int btow_histo_bht3[BTOW_ADC_MAX];
int btow_histo_bht1vpd[BTOW_ADC_MAX];
int cou_bht3 = 0;
int cou_bht1 = 0;
int max_adc = 0;
int bht3_towers = 0;
int bht1_towers = 0;

void initHack() {
    memset(btow_histo_bht1vpd, 0, sizeof(btow_histo_bht1vpd));
    memset(btow_histo_bht3, 0, sizeof(btow_histo_bht3));
}

void doHack(daqReader *rdr) {
    int isbht3 = 0;
    int isbht1 = 0;
    
    if(bht3 & rdr->daqbits64) {
	isbht3 = 1;
	cou_bht3++;
    }
    if(bht1vpd & rdr->daqbits64) {
	isbht1 = 1;
	cou_bht1++;
    }

    daq_dta *dd = rdr->det("btow")->get("adc") ;
    if(dd) {
	while(dd->iterate()) {
	    btow_t *d = (btow_t *)dd->Void;

	    for(int i=0;i<BTOW_MAXFEE;i++) {
		for(int j=0;j<BTOW_DATSIZE;j++) {
		    int adc = d->adc[i][j];
		    if(adc > max_adc) {
			max_adc = adc;
		    }
		    
		    assert(adc < BTOW_ADC_MAX);

		    if(isbht1) {
          		btow_histo_bht1vpd[adc]++;
			bht1_towers++;
		    }
		    if(isbht3) {
			btow_histo_bht3[adc]++;
			bht3_towers++;
		    }
		}
	    }
	}
    }
}

void finishHack() {
    printf("# bht3=%d evts,  bht1=%d evts,   maxadc=%d\n", cou_bht3, cou_bht1, max_adc);
    printf("# bht3=%d towers.  bht1=%d towers.\n", bht3_towers, bht1_towers);

    for(int i=0;i<BTOW_ADC_MAX;i++) {
	printf("%d %d %d\n", i, btow_histo_bht3[i], btow_histo_bht1vpd[i]);
    }	
}

#endif

//#define EXTRACT_L4
#ifdef EXTRACT_L4

void initHack() {
}

void doHack(daqReader *rdr) {
    char *buff = rdr->memmap->mem;
    int sz = rdr->event_size;

    daq_dta *dd = rdr->det("l4")->get("gl3") ;
    if(dd) {
	write(STDOUT_FILENO, buff, sz);
    }
}

void finishHack() {
}

#endif

//#define COUNT_TRIGGERS
#ifdef COUNT_TRIGGERS

int evts[64];
int totalEvts;

void initHack() {
    memset(evts, 0, sizeof(evts));
    totalEvts = 0;
}

void doHack(daqReader *rdr) {
    if((totalEvts % 100) == 0) {
	printf(".");
	fflush(stdout);
    }

    if((totalEvts % 1000) == 0) {
	printf("\n");
    }

    for(int i=0;i<64;i++) {
	if(rdr->daqbits64 & (1ll << i)) {
	    evts[i]++;
	}
    }
    totalEvts++;
}

void finishHack() {
    printf("\n\nTotal events: %d\n", totalEvts);
    for(int i=0;i<64;i++) {
	if(evts[i] > 0) {
	    printf("trigger[%d] : %d events\n",i,evts[i]);
	}
    }
}

#endif

//#define FIX_L4_EVP_DATA
#ifdef FIX_L4_EVP_DATA
void doHack(daqReader *evp) {
    long long int pos = 0;
    SFS_File *sfs = (SFS_File *)evp->memmap->mem;
    LOGREC *log = (LOGREC *)evp->memmap->mem;
    char *buff = evp->memmap->mem;

    int inserted=0;
    while(pos < evp->event_size) {
       	
	int sz = 0;
	if(memcmp(log->lh.bank_type, "LRHD", 4) == 0) {
	    sz = sizeof(LOGREC);
	    //log->length += 512;

	    LOGREC newrec;
	    memcpy(&newrec, buff, sizeof(LOGREC));
	    //newrec.length = swap32(newrec.length);
	    newrec.length += 512/4;
	    //newrec.length = swap32(newrec.length);
	    write(STDOUT_FILENO, &newrec, sz);
		
	    buff += sz;
	    pos += sz;
	    log = (LOGREC *)buff;
	    sfs = (SFS_File *)buff;
	    continue;
	}
	else if(memcmp(sfs->type, "FILE", 4) == 0) {
	    sz = seeksize(sfs->sz) + sfs->head_sz;

		
	    if(!inserted) {
		if(memcmp(sfs->name, "gl3", 3) == 0) {
		    char l4dirbuff[512];
		    SFS_File *l4f = (SFS_File *)l4dirbuff;
		    memcpy(l4f->type, "FILE", 4);
		    l4f->byte_order = 0x04030201;		
		    sprintf(l4f->name, "l4/");
		    l4f->attr = SFS_ATTR_CD;
		    l4f->head_sz = get_sfsFileSize("l4/");
		    l4f->sz = 512 - l4f->head_sz;

		    write(STDOUT_FILENO, l4dirbuff, 512);
		    inserted = 1;
		}
	    }
	}

	write(STDOUT_FILENO, buff, sz);
	//printf("[%c%c%c%c] pos =%lld sz=%d\n",buff[0],buff[1],buff[2],buff[3],pos,sz);

	buff += sz;
	pos += sz;
	log = (LOGREC *)buff;
	sfs = (SFS_File *)buff;

	  
    }
}
#endif

void displayHelp()
{
    LOG(ERR,"Usage:  daqFileHacker filename"); 
}

int main(int argc, char *argv[])
{
    rtsLogOutput(RTS_LOG_STDERR) ;
    rtsLogLevel(WARN) ;

    if(argc != 2) {
	displayHelp();

	exit(0);
    }

    daqReader *evp;
    evp = new daqReader(argv[1]) ;	// create it with the filename argument..

    int good=0;
    int bad=0;

    initHack();

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
  

    finishHack();

    delete evp ; 
    return 0 ;
}
