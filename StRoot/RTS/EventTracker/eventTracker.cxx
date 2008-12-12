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


inline float fswap(float swapped)
{
        unsigned int* uintptr = (unsigned int*) &swapped;
        unsigned int uintvar = l2h32(*uintptr);
        float* floatvar = (float*)&uintvar;
        return *floatvar;
}


#ifdef UNIX_LITTLE_ENDIAN
#define l2hfloat(x) (x)
#define b2hfloat(x) (fswap(x))
#else
#define l2hfloat(x) (fswap(x))
#define b2hfloat(x) (x)
#endif


// control....
char g_fn[255];    // filename
uint g_seq = 0;        // only write this event if != 0
float g_bfield = 1000;
uint g_ptracks = 0;
uint g_nftracks = 0;
uint g_nctracks = 0;
uint g_pause = 0;
uint g_vertex = 0;

int copyl3_t(l3_t &l3, L3_P *l3p);

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
  rtsLogLevel(ERR);

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
  
  l3_t *l3_datafile;
  l3_t l3_legacy;

  rtsLogOutput(RTS_LOG_STDERR);
 

  parseArgs(argc, argv);
  
  rtsLogLevel(DBG);
 
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
      //LOG("JEFF", "blah");
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
      ret = copyl3_t(l3_legacy, l3p);
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



// Copy from type L3_P --> l3_t
//
int copyl3_t(l3_t &l3, L3_P *l3p)
{
  int len = l3p->bh.length;

  l3.max_channels = 1280000 ;
  l3.channels = 0 ;
  l3.mode = 1 ;

  // Tonko, zero this out and make non-sensical in case of further
  // problems down the road
  l3.tracks_num = 0 ;
  l3.cluster_num = 0 ;
  l3.xVertex = -1000.0 ;
  l3.yVertex = -1000.0 ;
  l3.zVertex = -1000.0 ;

  LOG(DBG,"L3_P bytes %d",len,0,0,0) ;

  if(checkBank(l3p->bh.bank_type,"L3_P") != 0) {
    return -1 ;
  }


  if(l3p->tracks.len && l3p->tracks.off){
    struct L3_GTD* l3gtd = 
      (struct L3_GTD*)((char*)l3p + l2h32(l3p->tracks.off)*4) ;

    // Tonko, sanity check
    if(checkBank(l3gtd->bh.bank_type,"L3_GTD") < 0) {
      return -1 ;
    }

    l3.tracks_num = l2h32(l3gtd->nTracks);
    l3.cluster_num = l2h32(l3gtd->nHits);
    l3.xVertex = l2hfloat(l3gtd->xVert);
    l3.yVertex = l2hfloat(l3gtd->yVert);
    l3.zVertex = l2hfloat(l3gtd->zVert);

    // Tonko, sanity check
    if(l3.tracks_num >= L3_MAX_NR_TRACKS) {
      LOG(ERR,"L3 track number %d > %d!",l3.tracks_num,L3_MAX_NR_TRACKS ,0,0,0) ;
      return -1 ;
    }


    for (unsigned int i=0; i<l3.tracks_num; i++) {
      global_track *tr = &(l3gtd->track[i]);

      l3.track[i].id = l2h32(tr->id);

#ifndef UNIX_LITTLE_ENIDAN
      l3.track[i].flag         = tr->flag;
      l3.track[i].innerMostRow = tr->innerMostRow;
      l3.track[i].outerMostRow = tr->outerMostRow;

      l3.track[i].nHits = tr->nHits;
      l3.track[i].ndedx = tr->ndedx;
      l3.track[i].q     = tr->q;
#else
      l3.track[i].flag = ( ((unsigned short)tr->innerMostRow) |
			   ((unsigned short)tr->outerMostRow)<<8);
      l3.track[i].innerMostRow = (char)( tr->flag & 0x00ff );
      l3.track[i].outerMostRow = (char)((tr->flag & 0xff00)>>8);

      l3.track[i].nHits    = (unsigned char)tr->q;
      l3.track[i].reserved = (char)tr->ndedx;
      l3.track[i].ndedx    = (unsigned char)tr->reserved;
      l3.track[i].q        = (char)tr->nHits;
#endif
      l3.track[i].chi2[0] = l2hfloat(tr->chi2[0]);
      l3.track[i].chi2[1] = l2hfloat(tr->chi2[1]);
      l3.track[i].dedx    = l2hfloat(tr->dedx);
      l3.track[i].pt      = l2hfloat(tr->pt);
      l3.track[i].phi0    = l2hfloat(tr->phi0);
      l3.track[i].psi     = l2hfloat(tr->psi);
      l3.track[i].r0      = l2hfloat(tr->r0);
      l3.track[i].tanl    = l2hfloat(tr->tanl);
      l3.track[i].z0      = l2hfloat(tr->z0);
      l3.track[i].length  = l2hfloat(tr->length);
      l3.track[i].dpt     = l2hfloat(tr->dpt);
      l3.track[i].dpsi    = l2hfloat(tr->dpsi);
      l3.track[i].dz0     = l2hfloat(tr->dz0);
      l3.track[i].dtanl   = l2hfloat(tr->dtanl);

    }
	      

  }


#ifdef SHOW_DEBUG_INFO

#ifdef UNIX_LITTLE_ENDIAN
  printf("Running on LITTLE endian machine\n");
#else
  printf("Running on BIG endian machine\n");
#endif

  printf("\nVertex: (%6.2f/%6.2f/%6.2f)\n",
	 l3.xVertex, l3.yVertex, l3.zVertex);

  printf("Tracks: %5d   Clusters %7d\n",
	 l3.tracks_num, l3.cluster_num);
	
  for (unsigned int i=0; i<l3.tracks_num; i++) {
    printf("%5d: pt=%5.3f z0=%7.2f q=%2d nHits=%2d ndedx=%2d ",
	   i, l3.track[i].pt, l3.track[i].z0, l3.track[i].q, 
	   l3.track[i].nHits, l3.track[i].ndedx); 

    printf("flag=0x%04x iRow=%2d oRow=%2d\n",
	   l3.track[i].flag, l3.track[i].innerMostRow, 
	   l3.track[i].outerMostRow); 
  }

#endif

  return len ;

}
