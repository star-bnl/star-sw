#ifndef TRG_VERSION
#define TRG_VERSION 0x32
#endif

#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <string.h>
#include <daqFormats.h>
#include <rts.h>
#include "rtsLog.h"
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_det.h>
#include <DAQ_READER/daq_dta.h>
#include <DAQ_L3/daq_l3.h>
#include <DAQ_TRG/daq_trg.h>
#include "eventTrackerLib.hh"


// control....
char g_fn[255];    // filename
uint g_seq = 0;        // only write this event if != 0
float g_bfield = 1000;
uint g_ptracks = 0;
uint g_nftracks = 0;
uint g_nctracks = 0;
uint g_pause = 0;
uint g_vertex = 0;

void printL3Info(l3_t& l3)
{
  printf("%d tracks, %d clusters: Vertex = (%f, %f %f)\n",l3.tracks_num, l3.cluster_num, l3.xVertex,l3.yVertex,l3.zVertex);

  if(g_ptracks) {
    for(u_int i=0;i<l3.tracks_num;i++) {
      global_track *track = &l3.track[i];

      printf("%5d: pt=%5.3f z0=%7.2f q=%2d nHits=%2d ndedx=%2d ",
	     i, track->pt, track->z0, track->q, 
	     track->nHits, track->ndedx); 
      
      printf("flag=0x%04x iRow=%2d oRow=%2d\n",
	     track->flag, track->innerMostRow, 
	     track->outerMostRow); 
    }
  }
}


void parseArgs(int argc, char *argv[])
{
  g_fn[0] = 0;

  for(int i=1;i<argc;i++) {
    if(argv[i][0] != '-') {   // should be filename...
      if(g_fn[0] != 0) goto badargs;
      strcpy(g_fn, argv[i]);
    }
    else if(strcmp(argv[i], "-event") == 0) {
      i++;
      g_seq = atoi(argv[i]);
      //printf("g_seq = %d\n",g_seq);
    }
    else if(strcmp(argv[i], "-B") == 0) {
      i++;
      g_bfield = atof(argv[i]);
      printf("g_bfield = %f\n",g_bfield);
    }
    else if (strcmp(argv[i], "-p") == 0) {
      g_ptracks = 1;
    }
    else if (strcmp(argv[i], "-nft") == 0) {
      g_nftracks = 1;
    }
    else if (strcmp(argv[i], "-nct") == 0) {
      g_nctracks = 1;
    }
    else if (strcmp(argv[i], "-pause") == 0) {
      g_pause = 1;
    }
    else if (strcmp(argv[i], "-vtx") == 0) {
      rtsLogLevel(CRIT);
      g_vertex = 1;
    }
    else {
      goto badargs;
    }
  }

  if(g_fn[0] == 0) goto badargs;

  return;

 badargs:
  printf("eventTracker <-event #> <-B> <-p> <-nft> <-nct> filename\n");
  printf("\t-event    -> event number\n");
  printf("\t-B        -> mag field\n");
  printf("\t-p        -> print track params\n");
  printf("\t-nft      -> don't read tracks from file\n");
  printf("\t-nct      -> don't calculate tracks\n");
  printf("\t-pause    -> pause before and after\n");
  printf("\t-vtx      -> produce output for vtx scan\n");

  exit(0);
}

// Dumps tracks....
int main(int argc, char *argv[])
{

  daq_dta *dd;

  int ret = 0;
  
  l3_t l3_legacy;

  rtsLogOutput(RTS_LOG_STDERR);

  parseArgs(argc, argv);

  rtsLogLevel(WARN);

  // printf("sizeof tpc %d\n",sizeof(tpc));
 
  daqReader *rdr = new daqReader(g_fn);
  if(!rdr) {
    printf("Error getting daqReader\n");
    return 0;
  }

  // Buffer for event storage...
  L3_P *l3p = (L3_P *)malloc(szL3_max);

  EventTracker *evtTracker;

  if(g_bfield == 1000)
    evtTracker = new EventTracker();
  else
    evtTracker = new EventTracker(g_bfield);

  char tmp[200];
  
  if(g_pause) {
    printf("Enter something: ");
    scanf("%s", tmp);
  }

  for(;;) {


    char *mem = rdr->get(0,EVP_TYPE_PHYS);

 
    if(!mem) {

      if(rdr->status == EVP_STAT_EOR) {
	if(!g_vertex) {
	  printf("End of run...\n");
	}
	return 0;
      }
      else {
	printf("Error reading an event\n");
	return 0;
      }
    }


    // We have an event of some kind...
    //
    // do filtering...
    //
    if(g_seq != 0) {                // event number

      if(rdr->seq != g_seq) continue;

    }


    if(rdr->token == 0) continue;


    if(!g_vertex) { 
      printf("**** Event %d (seq = %d): %d bytes, token %d, triggers = 0x%x\n",

	     rdr->event_number, rdr->seq, rdr->bytes, rdr->token, rdr->daqbits);
    }
    
    DATAP *datap = (DATAP *)mem;
    if(!datap) {
      printf("Error reading datap:\n");
      return 0;
    }

    // First Use old L3 reader to read L3 from datafile if its there...

    if(!g_nftracks) {

      dd = rdr->det("l3")->get("legacy");
      //LOG("JEFF", "blih");
      if(!dd) {
 	//printf("No L3 banks in data file %d\n",ret);
      }
      else {

	dd->iterate();
	l3_t *pL3 = (l3_t *)dd->Void;
 	printf("This comes from the datafile L3 banks...------ len=%d\n",ret);
 	printL3Info(*pL3);
 	printf("End Datafile L3 banks-------------------------\n");
      }
    }
      

    //printf("*********** %s  0x%x **********\n",datap->bh.bank_type, (uint) datap);

    // Now, track the event into a new buffer l3p (this was allocated above)
    //

    ret = evtTracker->trackEvent(rdr, mem, l3p, szL3_max);

    if(ret < 0) {

      printf("Error tracking event %d\n",rdr->seq);

      continue;
    }

    //    continue;

    if(l3p->tracks.off == 0) {
      LOG(NOTE, "No tracks produced for event %d\n",rdr->seq);
      continue;
    }
   
    // And now that we have "retracked" events, get a l3Reader as before
    //
    // ***Note*** You must use "l3p" here which is of Type L3_P
    // this is actually a different function than the one above!
    // although it will write the data into the SAME global 
    // structure "l3"
    //
    
    double zdc_vertex=0;
    double bbc_vertex=0;
    double zdc8=0;
    double zdc9=0;
    int zdc0 = 0;
    int zdc4 = 0;
    if(g_vertex) {
      dd = rdr->det("trg")->get("legacy");
      if(!dd) {
	printf("Invalid trigger reader\n");
      }
      else {
	dd->iterate();
	trg_t *pTRG = (trg_t *)dd->Void;
	zdc8 = (double)pTRG->ZDC[8];
	zdc9 = (double)pTRG->ZDC[9];
	zdc_vertex = (zdc9 - zdc8)*0.16*30.0/2.0;
	zdc0 = pTRG->ZDC[0];
	zdc4 = pTRG->ZDC[4];
	
	TrgSumData *sum =(TrgSumData *)pTRG->trg_sum;
	bbc_vertex = sum->DSMdata.VTX[3]%512;
      }
    }

    if(!g_nctracks) {
      ret = evtTracker->copyl3_t(l3_legacy, l3p);
      if(ret <= 0) {
	// printf("No retracked L3 banks valid %d\n",ret);
      }
      else {
	if(g_vertex) {
	  printf("%d %d %lf %lf %lf %lf %lf %lf %lf %d %d\n", rdr->daqbits, l3_legacy.tracks_num, zdc8, zdc9, zdc_vertex, l3_legacy.xVertex, l3_legacy.yVertex,l3_legacy.zVertex, bbc_vertex, zdc0, zdc4);
	}
	else {
	  printf("This comes from retracking---------------- len=%d\n",ret);
	  printL3Info(l3_legacy);
	  printf("End of retracking-------------------------\n");
	}
      }
    }



    // This is a dump of all the tracks... its probably better to use
    // the standard l3Reader method however...
    //    L3_GTD *gtd = (L3_GTD *)((uint)l3p + l3p->tracks.off*4);
    //    LOG(DBG, "GTD size = %d\n",gtd->bh.length * 4);
    //    if(g_ptracks) evtTracker->dumpGTD(gtd); 
  }

  if(g_pause) {
    printf("Enter something: ");
    scanf("%s", tmp);
  }
  
  free(l3p);
}



