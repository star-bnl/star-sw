#include <stdio.h>
#include <unistd.h>
#include <getopt.h>
#include <sys/types.h>
#include <stdlib.h>
#include <string.h>

#include <rtsLog.h>   // for my LOG() call
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
#include <DAQ_TRG/daq_trg.h>

static int bsmd_doer(daqReader *rdr, const char  *do_print) ;
static int esmd_doer(daqReader *rdr, const char  *do_print) ;
static int btow_doer(daqReader *rdr, const char  *do_print) ;
static int etow_doer(daqReader *rdr, const char  *do_print) ;
static int trg_doer(daqReader *rdr, const char *do_print) ;

#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TFile.h"

void histinit();
void histwrite();
void histdelete();

using namespace std;

TH2F* etow[ETOW_MAXFEE];
TH2F* btow[BTOW_MAXFEE];
TH2F* bsmd[BSMD_FIBERS];
TH2F* bsmd_zs[BSMD_FIBERS];
TH2F* esmd[ESMD_MAXFEE];

TFile* outfile;

static int good ;

int main(int argc, char *argv[])
{
        outfile=0;
	char dir[FILENAME_MAX];
	getcwd(dir,FILENAME_MAX);
	printf("%s\n",dir);

        extern char *optarg ;
        extern int optind ;
        int c ;
        const char *print_det = "" ;
        char _mountpoint[256];
        char *mountpoint = NULL;

        rtsLogOutput(RTS_LOG_STDERR) ;
        rtsLogLevel((char* )WARN) ;

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

        class daqReader *evp ;                  // tha main guy
        evp = new daqReader(argv[optind]) ;     // create it with the filename argument..
        if(mountpoint) {
          evp->setEvpDisk(mountpoint);
        }

        good=0;
        int bad=0;
        int lastrun = -1;
	int byteTot = 0;

	// "DAQ trigger ID" for triggers of interest (usually MB)
        int trigID[2]={2048,128}; //BBCMB=2048 and JP0=128 //200 GeV Run 12 emc-check
        int eventTrig[2]={0,0}; //counter for events satisfying triggerxs

        for(;;) {
                char *ret = evp->get(0,EVP_TYPE_ANY);

                if(ret) {
                  if(evp->status) {
                        LOG(ERR,"evp status is non-null [0x08X, %d dec]",evp->status,evp->status) ;
                        continue ;
                  }
                  good++;
                  if(good%1000 == 0) LOG(INFO,"%d good events processed",good);
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
		  LOG(INFO,"End of File reached...%i good events processed",good) ;
		  LOG(INFO,"bit %i = %i events processed",trigID[0],eventTrig[0]) ;
		  LOG(INFO,"bit %i = %i events processed",trigID[1],eventTrig[1]) ;
		  break ; // of the for() loop...
                }
		
		if(evp->run == 0)continue;
		if(lastrun != evp->run){
		  printf("new run started\n");
		  lastrun = evp->run;
		  if(outfile){
		    histwrite();
		    outfile->Close();
		    delete outfile;
		    outfile = 0;
		    printf("old file closed, deleted\n");
		    histdelete();
		    printf("hists cleared deleted\n");
		  }
		  if(!outfile){
		    printf("need to create new file\n");
		    string infile(argv[optind]);
		    char name[100];
		    string daq(".daq");
		    if(infile.find(daq)!=string::npos){
                      string phys("st_physics");
                      string w("st_W");
                      size_t found = infile.find(phys);
                      if(found==string::npos)found=infile.find(w);
                      string base = infile.substr(found);
                      size_t fd = base.find(daq);
                      base.replace(fd,4,"");
                      //sprintf(name,"%s/%i.ushist.root",dir,evp->run);
                      sprintf(name,"%s/%s.ushist.root",dir,base.c_str());
		    }else{
		      sprintf(name,"%s/%i.ushist.root",dir,evp->run);
		    }
		    printf("%s\n",name);
		    outfile = new TFile(name,"RECREATE");
		    histinit();
		    printf("new hists and file created\n");
		  }

		}

                daq_dta *dd ;   // generic data pointer; reused all the time
             
		bool goodTrig[2]={0,0};

		//try trigger filter (from daqFileChopper)
		unsigned int bits = evp->daqbits;
		//try daq trigger bits
		//cout<<"event ="<<good<<", bits ="<<bits;
		if(bits & trigID[0]) goodTrig[0]=1; 
		if(bits & trigID[1]) goodTrig[1]=1;

		if(goodTrig[1]) eventTrig[1]++;
		if(!goodTrig[0]) continue;
		eventTrig[0]++;

		/***************** EMCs ************************/

                if(btow_doer(evp, print_det)) ;//LOG(INFO,"BTOW found") ;

                //if(bsmd_doer(evp,print_det)) ;//LOG(INFO,"BSMD found (any bank)") ;

                if(etow_doer(evp, print_det)) ;//LOG(INFO,"ETOW found") ;

                if(esmd_doer(evp, print_det)) ;//LOG(INFO,"ESMD found") ;

                	
        }
	if(outfile){
	  histwrite();
	  outfile->Close();
	  delete outfile;
	  outfile = 0;
	  printf("old file closed, deleted\n");
	  histdelete();
	  printf("hists cleared deleted\n");
	}
	return 0 ;
}

void histinit(){

  for(int i = 0; i < ETOW_MAXFEE; i++){
    char name[100];
    char title[100];
    sprintf(name,"ETOW_%i",i+1);
    sprintf(title,"ETOW FEE %i",i+1);
    etow[i] = new TH2F(name,title,ETOW_DATSIZE,-0.5,ETOW_DATSIZE-0.5,4096,-0.5,4095.5);
  }
  for(int i = 0; i < BTOW_MAXFEE; i++){
    char name[100];
    char title[100];
    sprintf(name,"BTOW_%i",i+1);
    sprintf(title,"BTOW FEE %i",i+1);
    btow[i] = new TH2F(name,title,BTOW_DATSIZE,-0.5,BTOW_DATSIZE-0.5,4096,-0.5,4095.5);
  }	
  for(int i = 0; i < ESMD_MAXFEE; i++){
    char name[100];
    char title[100];
    sprintf(name,"ESMD_%i",i+1);
    sprintf(title,"ESMD FEE %i",i+1);
    esmd[i] = new TH2F(name,title,ESMD_DATSIZE,-0.5,ESMD_DATSIZE-0.5,4096,-0.5,4095.5);
  }

#if 0 //not using BSMD at the moment
  for(int i = 0; i < BSMD_FIBERS; i++){
    char name[100];
    char title[100];
    sprintf(name,"BSMD_%i",i+1);
    sprintf(title,"BSMD FIBER %i",i+1);
    bsmd[i] = new TH2F(name,title,BSMD_DATSIZE,-0.5,BSMD_DATSIZE-0.5,1024,-0.5,1023.5);
  }
  for(int i = 0; i < BSMD_FIBERS; i++){
    char name[100];
    char title[100];
    sprintf(name,"BSMD_zs_%i",i+1);
    sprintf(title,"BSMD FIBER %i",i+1);
    bsmd_zs[i] = new TH2F(name,title,BSMD_DATSIZE,-0.5,BSMD_DATSIZE-0.5,1024,-0.5,1023.5);
  }
#endif

}

void histwrite()
{
  outfile->Write();
}
void histdelete()
{
  for(int i = 0; i < ETOW_MAXFEE; i++){
    etow[i] = 0;
  }
  for(int i = 0; i < BTOW_MAXFEE; i++){
    btow[i] = 0;
  }
  for(int i = 0; i < ESMD_MAXFEE; i++){
    esmd[i] = 0;
  }
#if 0 //not using BSMD at the moment
  for(int i = 0; i < BSMD_FIBERS; i++){
    bsmd[i] = 0;
  }
#endif

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

                        if(do_print) {  // print something...
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
                        
                        if(do_print) {  // I have no clue but let me print first few words...


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

#if 0 //not using BSMD at the moment
static int bsmd_doer(daqReader *rdr, const char *do_print)
{
        int found = 0 ;
        daq_dta *dd ;

        if(strcasestr(do_print,"bsmd")) ;       // leave as is...
        else do_print = 0 ;


        // do I see the non-zero-suppressed bank? let's do this by fiber...
        for(int f=1;f<=12;f++) {
                dd = rdr->det("bsmd")->get("adc_non_zs",0,f) ;  // sector is ignored (=0)
                if(dd) {
                        while(dd->iterate()) {
                                found = 1 ;

                                bsmd_t *d = (bsmd_t *) dd->Void ;

                                if(do_print) printf("BSMD non-ZS: fiber %2d, capacitor %d:\n",dd->rdo,d->cap) ;


                                for(int i=0;i<BSMD_DATSIZE;i++) {
                                  short cap = (short)d->cap;
                                  if(cap!=124 && cap!=125)bsmd[f-1]->Fill(i,d->adc[i]);
                                        if(do_print) printf("   %4d = %4d\n",i,d->adc[i]) ;
                                }
                        }
                }
        }

        // do I see the zero suppressed bank?
        for(int f=1;f<=12;f++) {
                dd = rdr->det("bsmd")->get("adc",0,f) ;
		int ngood = 0;
                if(dd) {
                        while(dd->iterate()) {
                                found = 1 ;

                                bsmd_t *d = (bsmd_t *) dd->Void ;

                                if(do_print) printf("BSMD ZS: fiber %2d, capacitor %d:\n",dd->rdo,d->cap) ;

                                for(int i=0;i<BSMD_DATSIZE;i++) {
                                  short cap = (short)d->cap;
                                  if(cap!=124 && cap!=125 && d->adc[i]!=0)bsmd_zs[f-1]->Fill(i,d->adc[i]);
				  if(d->adc[i]!= 0)ngood++;
                                        // since this is zero-suppressed, I'll skip zeros...
                                        if(do_print) if(d->adc[i]) printf("   %4d = %4d\n",i,d->adc[i]) ;
                                }
                        }
                }
		if(ngood == 0){
		  printf("bsmd rdo %i had no hits in %i event %i\n",f,rdr->run,rdr->event_number);
		}
        }


        return found ;
}
#endif

static int esmd_doer(daqReader *rdr, const char *do_print)
{
        int found = 0 ;
        daq_dta *dd ;

        if(strcasestr(do_print,"esmd")) ;       // leave as is...
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
                                  esmd[i]->Fill(j,d->adc[i][j]);
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

        if(strcasestr(do_print,"etow")) ;       // leave as is...
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
                                  etow[i]->Fill(j,d->adc[i][j]);
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

        if(strcasestr(do_print,"btow")) ;       // leave as is...
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
                                  btow[i]->Fill(j,d->adc[i][j]);
                                        if(do_print) printf("BTOW: fee %2d: data %d: 0x%04X [%d dec]\n",i,j,d->adc[i][j], d->adc[i][j]) ;
                                }

                        }
                }
        }

        return found ;
}

