#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

/***********************************************************
 * $Id: L2wEemc2012.cxx,v 1.4 2012/03/23 20:12:09 balewski Exp $
 * \author Jan Balewski, MIT, 2008 
 ***********************************************************
 * Descripion: see .h
 **********************************************************
 */

#include "../L2algoUtil/L2Histo.h"
#include "../L2algoUtil/L2EmcDb2012.h"

#include "L2wEemc2012.h"

//=================================================
//=================================================
L2wEemc2012::L2wEemc2012(const char* name, const char *uid, L2EmcDb2012* db, L2EmcGeom2012 *geoX, char* outDir, int resOff)  :  L2VirtualAlgo2012( name, uid,  db, outDir, false,true, resOff ) { 
  /* called one per days
     all memory allocation must be done here
  */
  
  // geoX is not used, ignore
  
  setMaxHist(4); // set upper range, I uses only 2^N -it is easier to remember
  createHisto(); 
  
}

/* ========================================
   ======================================== */
int 
L2wEemc2012::initRunUser( int runNo, int *rc_ints, float *rc_floats) {
  
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
  
  // update titles of histos, add values of params
  char txt[1000];

  sprintf(txt,"highest endcap tower ET; x: tower ET; y: counts");
  hA[1]->setTitle(txt);

  return 0; //OK  
}               

  

/* ========================================
  ======================================== */
void 
L2wEemc2012::computeUser(int token){
  // token range is guaranteed by virtual12-class
  // -----------  SCAN FOR HIGHEST TOWER ----
  
  int i;
  // get pointer to input list towers from calib-algo
  const HitTower1 *hit=0;
  int   hitSize=0;

  hit    =mEveStream_etow[token].get_hits();
  hitSize=mEveStream_etow[token].get_hitSize();

  float highestEt=0.;
  unsigned short highestRDO=8888;
  for(i=0;i< hitSize;i++,hit++) {
    if (hit->et>highestEt) {
      highestEt=hit->et;
      highestRDO=hit->rdo;
    }
  }
   
  resultBlob[token].trigger=0;
  resultBlob[token].highestEt=(unsigned char)(highestEt*256.0/60.0);
  resultBlob[token].highestRDO=highestRDO;
  hA[1]->fill((int)highestEt);
  if (highestEt>par_EtThresh) {
    hA[2]->fill((int)highestRDO);
    resultBlob[token].trigger+=2;
  }
  
} 


/* ========================================
  ======================================== */
bool 
L2wEemc2012::decisionUser(int token, int *myL2Result){
  // INPUT: token + comput() results stored internally
  // OUTPUT: YES if the highestEt>par_EtThresh.  Plot highestEt.
  if (mRandomAccept) resultBlob[token].trigger+=1;

  memcpy(myL2Result,&(resultBlob[token]),sizeof( L2weResult2012));
  return resultBlob[token].trigger&2;
} 


/* ========================================
  ======================================== */
void
L2wEemc2012::finishRunUser() {  /* called once at the end of the run */
  // do whatever you want, log-file & histo-file are still open
  
  //  if (mLogFile){ 
  //    fprintf(mLogFile,"finishRunUser-%s bhla bhla\n",getName());
  //  }
  
}


//=======================================
//=======================================
void 
L2wEemc2012::createHisto() {
  //memset(hA,0,sizeof(hA));

  hA[1]=new L2Histo(1,(char*)"Highest endcap tower ET; x: tower ET; y: counts", 100); // title set in initRun
  hA[2]=new L2Histo(2,(char*)"ETOW: Seed Tower RDO", 720); // title in initRun
  //hA[1]=new L2Histo(1,(char*)"accepted: highest endcap tower ET; x: tower ET; y: counts", 100); // title set in initRun
  

  // printf("L2-%s::createHisto() done\n",getName());
}

/**********************************************************************
  $Log: L2wEemc2012.cxx,v $
  Revision 1.4  2012/03/23 20:12:09  balewski
  changes are to write a L2weResult to the L2Result array so we can differentiate random and real accepts, and update the plots so we can have some monitoring,no intended changes to the actual algo

  Revision 1.3  2011/10/19 16:12:12  jml
  more 2012 stuff

  Revision 1.2  2011/10/19 15:39:45  jml
  2012

  Revision 1.1  2011/10/18 15:11:45  jml
  adding 2012 algorithms

  Revision 1.2  2009/11/19 15:48:49  balewski
  add (char*) to many strings to make SL5 happ, few other adjustments

  Revision 1.1  2009/03/28 19:43:53  balewski
  2009 code

  Revision 1.0  2009/03/20 05:00:00 rcorliss
  adapted from L2hienAlgo.

*/


