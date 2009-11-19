#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

/***********************************************************
 * $Id: L2wEemc2009.cxx,v 1.2 2009/11/19 15:48:49 balewski Exp $
 * \author Jan Balewski, MIT, 2008 
 ***********************************************************
 * Descripion: see .h
 **********************************************************
 */

#include "../L2algoUtil/L2Histo.h"
#include "../L2algoUtil/L2EmcDb.h"

#include "L2wEemc2009.h"

//=================================================
//=================================================
L2wEemc2009::L2wEemc2009(const char* name, L2EmcDb* db, L2EmcGeom *geoX, char* outDir, int resOff)  :  L2VirtualAlgo2009( name,  db, outDir, false,true, resOff ) { 
  /* called one per days
     all memory allocation must be done here
  */
  
  // geoX is not used, ignore
  
  setMaxHist(2); // set upper range, I uses only 2^N -it is easier to remember
  createHisto(); 
  
}

/* ========================================
   ======================================== */
int 
L2wEemc2009::initRunUser( int runNo, int *rc_ints, float *rc_floats) {
  
  // unpack params from run control GUI
  par_dbg        =  rc_ints[0];
  par_RndAcceptPrescale   =  rc_ints[1];
  par_EtThresh    =  rc_floats[0];
  
  // verify consistency of input params
  int kBad=0;
  kBad+=2*(par_EtThresh<0);
  
  //FIX log output
  if (mLogFile) { 
    fprintf(mLogFile,"L2%s algorithm initRun(R=%d), compiled: %s , %s\n",getName(),mRunNumber,__DATE__,__TIME__);
    fprintf(mLogFile," - use  ET Thresh =%f, RndAcceptPrescale=%d debug=%d\n", par_EtThresh , par_RndAcceptPrescale, par_dbg);
    fprintf(mLogFile,"initRun() params checked for consistency, Error flag=0x%04x\n",kBad);
  }
  if(kBad) return -kBad;    
  
  // clear content of all histograms & token-dependent memory
  int i;
  for (i=0; i<mxHA;i++) if(hA[i])hA[i]->reset();
  for (i=0;i<L2eventStream2009::mxToken;i++) highestEt[i]=0;

  // update titles of histos, add values of params
  char txt[1000];

  sprintf(txt,"highest endcap tower ET; x: tower ET; y: counts");
  hA[0]->setTitle(txt);

  sprintf(txt,"accepted: highest endcap tower ET; x: tower ET; y: counts");
  hA[1]->setTitle(txt);

  return 0; //OK  
}               

  

/* ========================================
  ======================================== */
void 
L2wEemc2009::computeUser(int token){
  // token range is guaranteed by virtual09-class
  // -----------  SCAN FOR HIGHEST TOWER ----
  
  int i;
  // get pointer to input list towers from calib-algo
  const HitTower1 *hit=0;
  int   hitSize=0;

  hit    =mEveStream_etow[token].get_hits();
  hitSize=mEveStream_etow[token].get_hitSize();

  highestEt[token]=0;
  for(i=0;i< hitSize;i++,hit++)
    if (hit->et>highestEt[token]) highestEt[token]=hit->et;
} 


/* ========================================
  ======================================== */
bool 
L2wEemc2009::decisionUser(int token, int *myL2Result){
  // INPUT: token + comput() results stored internally
  // OUTPUT: YES if the highestEt>par_EtThresh.  Plot highestEt.
  float h=highestEt[token];
  highestEt[token]=0; //clear it, just in case of stale data.
  hA[0]->fill((int)h);

  if (h>par_EtThresh) 
    {
      hA[1]->fill((int)h);
      return true;
    }
  return false;
} 


/* ========================================
  ======================================== */
void
L2wEemc2009::finishRunUser() {  /* called once at the end of the run */
  // do whatever you want, log-file & histo-file are still open
  
  //  if (mLogFile){ 
  //    fprintf(mLogFile,"finishRunUser-%s bhla bhla\n",getName());
  //  }
  
}


//=======================================
//=======================================
void 
L2wEemc2009::createHisto() {
  memset(hA,0,sizeof(hA));

  hA[0]=new L2Histo(0,(char*)"L0triggered: highest endcap tower ET; x: tower ET; y: counts", 100); // title set in initRun
  hA[1]=new L2Histo(1,(char*)"accepted: highest endcap tower ET; x: tower ET; y: counts", 100); // title set in initRun

  // printf("L2-%s::createHisto() done\n",getName());
}

/**********************************************************************
  $Log: L2wEemc2009.cxx,v $
  Revision 1.2  2009/11/19 15:48:49  balewski
  add (char*) to many strings to make SL5 happ, few other adjustments

  Revision 1.1  2009/03/28 19:43:53  balewski
  2009 code

  Revision 1.0  2009/03/20 05:00:00 rcorliss
  adapted from L2hienAlgo.

*/


