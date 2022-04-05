#include <string.h>
#include <stdio.h>

#include <daqFormats.h>
#include <rtsSystems.h>
#include <rtsLog.h>

#include <fcfClass.hh>

//#include <evpSupport.h>
//#include <tpcReader.h>
#include <TPC/padfinder.h>
#include <TPC/rowlen.h>
#include <adcLogTable.h>

#include "daq_tpc.h"

/************************************************/
// This is not a READER!
// 
// It calculates clusters from the filled tpc 
// structure...
//
/************************************************/

#define FCF_MAX_CLUSTERS 6000

// Assume that tpc raw structure is alread filled
//
//  t0corr   --> int   t0[46][242], for this sector
//  gainCorr --> u_int gain[46][242], for this sector
// 
int daq_tpc::fcfReader(int sector, int *t0c, u_int *gainc, tpc_t *tpc)          // from zero
{
  fcfClass *fcf = NULL;
  fcfAfterburner *fcf_after = NULL;

  int row, p, t;
  u_short tb;

  u_int adcOff[183] ;
  u_short cppOff[183] ;
  u_short startFlags[183];
  u_int output[2*FCF_MAX_CLUSTERS];
  u_int hits = 0;
  u_int *fcf_ptrs[3];
  int t0[242];
  u_int gain[242];
  
  memset(tpc->cl_counts, 0, sizeof(tpc->cl_counts));
  memset(tpc->cl, 0, sizeof(tpc->cl));
  tpc->has_clusters = 1;

  if(tpc->mode != 0) return -1;   // don't do for pedestals...

  fcf = new fcfClass(TPC_ID, NULL);
  fcf_after = new fcfAfterburner();
  
  for(int r=0;r<45;r++) {
    int have_data = 0;
    row = r+1;

    // Padrow adc data stored in "adc"
    u_char adc[182][512];

    memset(adc, 0, sizeof(adc));
    int adcs=0;
    for(p=0;p<182;p++) {
      for(t=0;t<tpc->counts[r][p];t++) {
	u_char val;
	tb = tpc->timebin[r][p][t];
	val = tpc->adc[r][p][t];

	adc[p][tb] = val;	

// 	if((sector == 1)) {
// 	  printf("cl_raw  %d %d %d %d %d\n",sector,r+1,p+1,tb,val) ;
// 	}
	
	if(val) {
	  have_data = 1;
	  adcs++;
	}
      }
    }

    // Padrow cpp stored in "cpp"
    u_short cpp[182][32*2];

    int seqs = 0;

    memset(cpp, 0xff, sizeof(cpp));
    for(p=0;p<182;p++) {
      int started, cou, max_seq;

      started = cou = max_seq = 0;

      for(tb=0;tb<512;tb++) {
	
	if(adc[p][tb] > 0) {
	  if(started) continue;

	  // start a new cluster pointer
	  started = 1;

	  if(cou <= 60)          // more than 31 clusters?
	    cpp[p][cou++] = tb;
	  else 
	    LOG(NOTE, "Cou is begger %d pad %d tb %d",cou,p,tb);
	}
	
	if(started && (adc[p][tb] == 0)) {  // finish cluster
	  
	  seqs++;

	  if(cou <= 61) 
	    cpp[p][cou++] = tb-1;
	  else 
	    LOG(NOTE, "Cou is bigger %d pad %d tb %d",cou,p,tb);


	  //test
// 	  if(r==0) {
// 	    LOG(NOTE, "r=%d p=%d t1=%d t2=%d:",r,p,cpp[p][cou-2],cpp[p][cou-1]);
// 	    for(int ii=cpp[p][cou-2]; ii <= cpp[p][cou-1];ii++) {
// 	      LOG(NOTE, "        adc[%d][%d]=%d",p,ii,adc[p][ii]);
// 	    }
// 	  }
	  //

	  started = 0;
	}
      }

      // done last tb, stop if neccessary
      if(started) {
	seqs++;
	cpp[p][cou++] = tb-1;
      }

      //      LOG(NOTE, "r=%d p=%d seq=%d",r,p,cou);
      started = 0;
    }

    

    //    LOG(NOTE, "adcs=%d seqs=%d",adcs, seqs);

    // Point to correct locations...
    for(int i=0;i<182;i++) {
      adcOff[i+1] = (char *)(&adc[i][0]) - (char *)(&adc[0][0]);
      cppOff[i+1] = (char *)(&cpp[i][0]) - (char *)(&cpp[0][0]) ;
    }

//     for(int i=0;i<182;i++) {
//       u_short *pp = (u_short *)(((u_char *)cpp) + cppOff[i+1]);
//       if((sector == 1)) {
// 	while(*pp != 0xffff) {
// 	  printf("cpp  %d %d %d %d %d\n",sector,r+1,i+1,*pp,1) ;
// 	  pp++;
// 	  printf("cpp  %d %d %d %d %d\n",sector,r+1,i+1,*pp,1) ;
// 	  pp++;
// 	}
//       }
//     }

    
    /*    
    if(r == 0) {
      for(p=0;p<182;p++) {
	u_char *x = ((u_char *)cpp) + cppOff[p+1];
	
	for(tb=0;tb<512;tb++) {
	  
	  LOG(NOTE, "cpp[%d][%d] = %d = cppOff = %d",
	      p,tb,cpp[p][tb], *(x+tb));

	  if(cpp[p][tb] == 0xff) break;
	}  
      }
    }
    */


    fcf->adcOff = adcOff;
    fcf->cppOff = cppOff;
    
    fcf->timebinLo = 0;
    fcf->timebinHi = 400;
    fcf->chargeMin = 40;
    fcf->set8to10(log8to10_table);
    

    if(t0c) {
      for(int i=0;i<182;i++) t0[i+1] = t0c[46*r + i];
    }
    else {
      for(int i=0;i<182;i++) t0[i+1] = 0;
    }

    if(gainc) {
      for(int i=0;i<182;i++) gain[i+1] = gainc[46*r + i];
    }
    else {
      for(int i=0;i<182;i++) gain[i+1] = 64;
    }

    fcf->t0Corr = t0;
    fcf->gainCorr = gain;

    fcf->maxClusters = FCF_MAX_CLUSTERS;

    fcf->simIn = NULL;
    fcf->simOut = NULL;

    fcf_after->setVerbose(false);

    fcf->startFlags = startFlags;
    
    memset(startFlags, 0, sizeof(startFlags));

    memset(output, 0, sizeof(output));
    u_int *out = output;
 
    memset(fcf_ptrs, 0, sizeof(fcf_ptrs));

    fcf->row = r+1;

    for(int mz=0;mz<3;mz++) {
      
      if(padfinder[r][mz].rdo == 0) continue;

      fcf->padStart = padfinder[r][mz].minpad;
      fcf->padStop = padfinder[r][mz].maxpad;

     
      if(fcf->padStart == 1) startFlags[fcf->padStart] |= FCF_ROW_EDGE;
      else(startFlags[fcf->padStart]) |= FCF_BROKEN_EDGE;

      if(tpc_rowlen[r+1] == fcf->padStop)
	startFlags[fcf->padStop] |= FCF_ROW_EDGE;
      else
	startFlags[fcf->padStop] |= FCF_BROKEN_EDGE;

   
      
      u_int words = fcf->finder((u_char *)adc, 
				(u_short *)cpp,
				(u_int *)out);

      //      LOG(NOTE, "c(%d)%d-%d padrow %d words=%d", mz, fcf->padStart, fcf->padStop, r,words);
 
      
      fcf_ptrs[mz] = out; 

      if(words != (*(out+1))*2 + 2) {
	LOG(ERR, "Words should match %d vs %d (%d)",
	    words, (*(out+1))*2 + 2, *(out+1));
      }      

      out += words;

    } // mz loop

    // Can burn this row...
    
//     for(int ii=0;ii<3;ii++) {
//       uint *x = fcf_ptrs[ii];
//       if(!x) continue;
//       int row = *x;
//       x++;
//       int ncl = *x;
//       x++;

 
//       while(ncl) {
// 	mzCentroid *cent=(mzCentroid *)x;
// 	if(sector == 1)
// 	  {
// 	    printf("nrcl: %d %d %d %d %d %d %d %d\n",
// 		   sector,r+1,row,ii,cent->x,cent->t,cent->flags,cent->charge);
// 	  }
// 	ncl--;
// 	x++;
// 	x++;
//       }
//     }
    
    fcf_after->burn(fcf_ptrs);

    int ccou=0;
    fcfHit h;
    tpc_cl *cld;
    
    while(fcf_after->next(&h)) {
      cld = &tpc->cl[r][ccou];
      ccou++;
      
      
      cld->p = (double)h.pad/64.0 + 0.5;
      cld->t = (double)h.tm/64.0 + 0.5;

      //      LOG(WARN,"  pad=%lf %lf",cld->p, cld->t);
      cld->charge = h.c;
      cld->flags = h.f;
      cld->t1 = h.t1;
      cld->t2 = h.t2;
      cld->p1 = h.p1;
      cld->p2 = h.p2;

    }

    hits += ccou;
    tpc->cl_counts[r] = ccou;
  }    // row loop

  delete fcf_after;
  delete fcf;

  return hits;
}
