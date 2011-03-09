#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

/*********************************************************************
 * $Id: L2bemcGamma2009.cxx,v 1.1 2011/03/09 16:29:07 pibero Exp $
 * \author Jan Balewski,MIT , 2008 
 *********************************************************************
 * Descripion: see .h
  *********************************************************************
 */


#ifdef  IS_REAL_L2  //in l2-ana  environment
  #include "../L2algoUtil/L2EmcDb.h"
  #include "../L2algoUtil/L2Histo.h"
#else    //full path needed for cvs'd code
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2EmcDb.h"
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2Histo.h"
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2EmcGeom.h"
#endif

#include "L2bemcGamma2009.h"


//=================================================
//=================================================
L2bemcGamma2009::L2bemcGamma2009(const char* name, L2EmcDb* db, L2EmcGeom *geoX, char* outDir, int resOff)  :  L2VirtualAlgo2009( name,  db, outDir, true, false, resOff) { 
  /* called one per days
     all memory allocation must be done here
  */

  mGeom=geoX; 
  if (!mGeom)
    criticalError("L2bemcGamma is broken -- can't find geom.");

  setMaxHist(16); // set upper range, I uses only 2^N -it is easier to remember
  createHisto();

  //------- self-consistency checks, should never fail
  // printf("ZZ %d %d\n", sizeof(L2gammaResult2008), L2gammaResult2008::mySizeChar);
  if (sizeof(L2gammaResult2009)!= L2gammaResult2009::mySizeChar) 
    criticalError("L2bemcGamma has failed consistency check. sizeof(L2gammaResult2009)!= L2gammaResult2009::mySizeChar");
  
}

/* ========================================
  ======================================== */
int 
L2bemcGamma2009::initRunUser( int runNo, int *rc_ints, float *rc_floats) {

  // unpack params from run control GUI
  par_dbg           =  rc_ints[0];
  par_RndAcceptPrescale      =  rc_ints[1];
  par_seedEtThres   =  rc_floats[0];
  par_clusterEtThres  =  rc_floats[1];

  // verify consistency of input params
  int kBad=0;
  kBad+=0x00001 * (par_seedEtThres<1.0);
  kBad+=0x00002 * (par_clusterEtThres<par_seedEtThres);

  // put notes about configuration into the log file
  if (mLogFile) { 
    fprintf(mLogFile,"L2%s algorithm initRun(R=%d), compiled: %s , %s\n params:\n",
	    getName(),mRunNumber,__DATE__,__TIME__);
    fprintf(mLogFile," - use  seedThres=%.2f (GeV),  debug=%d, prescale=%d (0=off,1=100proc, 2=50proc, etc)\n",
	    par_seedEtThres,par_dbg,par_RndAcceptPrescale);
    fprintf(mLogFile," - accept event cluster Thres=%.2f (GeV)\n",
	    par_clusterEtThres);
    fprintf(mLogFile,"initRun() params checked for consistency, Error flag=0x%04x\n",
	    kBad);
  }

  // clear content of all histograms & token-dependent memory
  int i;
  for (i=0; i<mxHA;i++) if(hA[i])hA[i]->reset();
  memset(mBtow,0,sizeof(mBtow));

  if(kBad>0) return -1*kBad;    
  else if(kBad<0) return kBad;

 
  // update titles of histos
  char txt[1000];

  sprintf(txt,"BTOW-decision: acc seed Tw ET>%.2f GeV; x:tower internal ID; y: counts",par_clusterEtThres);
  hA[7]->setTitle(txt);
  sprintf(txt,"#BTOW decision acc seed Tw, Et>%.2f GeV; eta bin [-1,+1]; y: phi bin ~ TCP sector",par_clusterEtThres);
  hA[14]->setTitle(txt);
  

  for ( int index=0; index<EmcDbIndexMax; index++ )
     {
       const L2EmcDb::EmcCDbItem *x = mDb->getByIndex(index);
       if ( x==0 ) continue;
       if ( !mDb->isBTOW(x) ) continue; 
       int sec = x->sec - 1;
       int sub = 8192; 
       sub = x->sub - 'a';
       int eta = x->eta - 1;
       int phi = BtowGeom::mxSubs *sec + sub;
       int tow = BtowGeom::mxEtaBin *phi + eta; // phi- changes faster
       int rdo = x->rdo;
       if (tow<0 || tow>mxBtow || rdo<0 || rdo>mxBtow) return -101;
       
       mTower2rdo[ tow ] = rdo;    // returns rdo channel for given tower
       mRdo2tower[ rdo ] = tow;    // returns tower for given rdo channel
    }
  return 0; //OK

}               

  

/* ========================================
  ======================================== */
float
L2bemcGamma2009::sumET(int phi, int eta) {
  int tow = BtowGeom::mxEtaBin *((phi+BtowGeom::mxPhiBin)%BtowGeom::mxPhiBin) + ((eta+BtowGeom::mxEtaBin)%BtowGeom::mxEtaBin); // phi- changes faster

  const int maxTowers = BtowGeom::mxEtaBin * BtowGeom::mxPhiBin;
  int towPlusOne;
  float sum;
  sum=0;
  sum=wrkBtow_et[tow];
  towPlusOne = tow+1;
  towPlusOne%= maxTowers;
  sum+=wrkBtow_et[towPlusOne];
  
  //if(tow==0 || towPlusOne==0) {
    //printf("tow : %d, %d --> %f %f \n",tow, towPlusOne,wrkBtow_et[tow],wrkBtow_et[towPlusOne]);
  //}
  tow+=BtowGeom::mxEtaBin;
  tow%=maxTowers;
  
  sum+=wrkBtow_et[tow];
  
  towPlusOne = tow+1;
  towPlusOne%= maxTowers;
  sum+=wrkBtow_et[towPlusOne];
  
  //if(tow==0 || towPlusOne==0) {
    //printf("tow : %d, %d --> %f %f \n",tow, towPlusOne,wrkBtow_et[tow],wrkBtow_et[towPlusOne]);
  //}
  // printf("B sumET=%f\n",sum);
  return sum;
}
  

/* ========================================
  ======================================== */
void 
L2bemcGamma2009::computeUser(int token){
  // token range is guaranteed by virtual08-class

  /* 2x2 cluster finder:
     - can find clusters through the tower border in Phi
     - sorts hit towers by Et and selects Highest Seed Towers first
  */
  clearEvent(token);

  // ------ PROJECT INPUT LIST TO 2D ARRAY AND SCAN FOR SEED TOWERS ----
  int i;
  //  printf("L2-%s-compute: ---BTOW ADC list--- size=%d\n",getName(),*globEve_btow_hitSize);

  L2bemcGammaEvent2009 *btowEve=mBtow+token;
  const HitTower1 *hit=mEveStream_btow[token].get_hits();
  const int hitSize=mEveStream_btow[token].get_hitSize();
  for(i=0;i< hitSize;i++,hit++) {
    int tower=mRdo2tower[hit->rdo];
    wrkBtow_et[tower]=hit->et;
    if(hit->et<par_seedEtThres)continue;
    wrkBtow_tower_seed[wrkBtow_tower_seed_size++]=tower;
    //printf("A  seed TWID=%d \n",tower);
  }
  hA[2]->fill(hitSize);
  int seedTow=-1,seedEta=-1,seedPhi=-1;
  float clustET=0;
  btowEve->isFresh=L2bemcGammaEvent2009::kDataFresh;

  // printf("B nseed=%d\n",wrkBtow_tower_seed_size);
  // ----------- FIND 2x2 CLUSTER AROUND EVERY SEED -----
  for(i=0; i<wrkBtow_tower_seed_size;i++) {
    seedTow=wrkBtow_tower_seed[i];
    seedEta=seedTow%BtowGeom::mxEtaBin;
    seedPhi=seedTow/BtowGeom::mxEtaBin;
    
    //.... find first 2x2 above cluster thresh
    if (seedEta < BtowGeom::mxEtaBin) {
      clustET = sumET(seedPhi,seedEta);
      if(clustET>par_clusterEtThres) goto ACCEPT;
      clustET = sumET(seedPhi-1,seedEta);
      if(clustET>par_clusterEtThres) goto ACCEPT;
    }
    if (seedEta > 0 ) {
      clustET = sumET(seedPhi-1,seedEta-1);
      if(clustET>par_clusterEtThres) goto ACCEPT;
      clustET = sumET(seedPhi,seedEta-1);
      if(clustET>par_clusterEtThres) goto ACCEPT;
    }
  }
  //.... ABORT
  btowEve->resultBlob.clusterEt=0;
  btowEve->resultBlob.meanEtaBin=0;
  btowEve->resultBlob.meanPhiBin=0;
  btowEve->resultBlob.trigger=0;
  btowEve->seedTwID=-1;
  btowEve->clusterET=0;
  btowEve->seedET=0;
  return;
  
 ACCEPT:
  // printf("B acc clustET=%f\n",clustET);
  btowEve->clusterET=clustET;
  btowEve->seedET=wrkBtow_et[seedTow];
  btowEve->resultBlob.clusterEt=(unsigned char)(clustET*256.0/60.0);
  btowEve->resultBlob.meanEtaBin=seedEta;
  btowEve->resultBlob.meanPhiBin=seedPhi;
  btowEve->seedTwID=seedTow;
  btowEve->resultBlob.trigger=2;
  return;
}




/* ========================================
  ======================================== */
bool 
L2bemcGamma2009::decisionUser(int token, int *myL2Result){
  // INPUT: token + comput() results stored internally
  // OUTPUT: yes/now + pointer to  L2Result


  // get pointers to internal private event storage
  L2bemcGammaEvent2009 *btowEve=mBtow+token;
  bool triggerDecision=(btowEve->resultBlob.trigger>0);

  //prescaling is done in Virtual:
  if (par_RndAcceptPrescale>0) btowEve->resultBlob.trigger+=mRandomAccept;
  
  //...... some histos just for fun
  if(btowEve->isFresh>L2bemcGammaEvent2009::kDataFresh) mhN->fill(6); // stale data

  btowEve->isFresh++; // mark the data as  stale

  if(btowEve->resultBlob.trigger&2) {
    mhN->fill(15);
    hA[6]->fill((int)btowEve->clusterET);
    hA[7]->fill(btowEve->seedTwID);  
    hA[11]->fill((int)btowEve->seedET);
    hA[12]->fill((int)(100.*btowEve->seedET/btowEve->clusterET));
  }

  if(btowEve->resultBlob.trigger&1)  mhN->fill(16);

  memcpy(myL2Result,&(btowEve->resultBlob),sizeof(L2gammaResult2009));
  return triggerDecision;
} 


/* ========================================
  ======================================== */
void
L2bemcGamma2009::finishRunUser() {  /* called once at the end of the run */
  // do whatever you want, log-file & histo-file are still open
  
  if (mLogFile){ 
    fprintf(mLogFile,"finishRunUser-%s bhla bhla\n",getName());
  }
  
  const int *hist15Data = hA[7]->getData();
  for (int i = 0; i < BtowGeom::mxEtaBin; i++) {
    for (int j = 0; j < BtowGeom::mxPhiBin; j++) {
      //printf("i %d, j %d, iXj*n %d, data %d\n", i, j, i+j*BtowGeom::mxEtaBin, hist15Data[i+j*BtowGeom::mxEtaBin]);
      hA[14]->fillW(i,j, hist15Data[i+j*BtowGeom::mxEtaBin]);
    }
  }
  
}


//=======================================
//=======================================
void 
L2bemcGamma2009::createHisto() {
  setMaxHist(16); // PMN added - histogram count does not seem to be initialiazed anywere.
  //memset(hA,0,sizeof(hA));

  hA[2]=new L2Histo(2,"BTOW-compute: #towers w/ energy /event; x: # BTOW towers; y: counts", 100); 
  hA[6]=new L2Histo(6,"BTOW-decision: accepted clust ...  ; x: ET(GeV)", 30);// title in initRun

  hA[7]=new L2Histo(7,"BTOW: accepted Seed Tower .....", 5000); // title in initRun
  hA[11]=new L2Histo(11,"BTOW: decision Cluster Seed Et; ET GeV", 30); // title in initRun
  hA[12]=new L2Histo(12,"BTOW: decision ;100*Seed Et/Cluster Et", 100); // title in initRun

  hA[14]=new L2Histo(14,"BTOW: hot tower projection", BtowGeom::mxEtaBin, BtowGeom::mxPhiBin); // title in initRun

  // printf("L2-%s::createHisto() done\n",getName());
}

//=======================================
//=======================================
void 
L2bemcGamma2009::clearEvent(int token){
  memset(wrkBtow_et,0,sizeof(wrkBtow_et)); 
  wrkBtow_tower_seed_size=0; 
  memset(&(mBtow[token].resultBlob),0, sizeof(L2gammaResult2009));
}
  
/* ========================================
  ======================================== */
void 
L2bemcGamma2009::print2(){ // full , local ADC array
  int i;
  printf("pr2-%s: ---BTOW ADC 2D array, only non-zero\n",getName());

  for(i=0;i<mxBtow;i++) {
    if(wrkBtow_et[i]<=0) continue;
    int rdo=mTower2rdo[i];
    float et=wrkBtow_et[i];
    printf("  btow: tower=%4d  rdo=%4d   et=%.3f \n",i,rdo,et);
  }

}

/* ========================================
  ======================================== */
void 
L2bemcGamma2009::print3(){ // seed list
  int i;
  printf("pr3-%s: ---seed list, size=%d\n",getName(),wrkBtow_tower_seed_size);

  for(i=0;i<wrkBtow_tower_seed_size;i++) {
    int tower=wrkBtow_tower_seed[i];
    float et=wrkBtow_et[tower];
    printf("  btow: i=%4d  tower=%4d   et=%.3f \n",i,tower,et);
  }

}

#if 0

/* ========================================
  ======================================== */
void 
L2bemcGamma2009::print4(int token, int hitSize){ // L2-algo input list
  int i;
  printf("print4 IS NOT Fully FUNCTIONAL **********************\n");
  printf("pr1-%s: ---BTOW Sorted ADC list--- size=%d\n",getName(),hitSize);
  //const HitTower *hit=globEve_btow_hit;
  for(i=0;i< hitSize;i++) {
    int adc=0;//(mEveStream_btow[token].get_hits()[wrkBtow_tower_index[i]]).adc;
    int rdo=0;//(mEveStream_btow[token].get_hits()[wrkBtow_tower_index[i]]).rdo;
    float et=wrkBtow_et[wrkBtow_tower_index[i]];
    float ene=0;//(mEveStream_btow[token].get_hits()[wrkBtow_tower_index[i]]).ene;
    printf("  tower=%2d ",wrkBtow_tower_index[i]);
    printf("  btow: i=%2d rdo=%4d  adc=%d  et=%.3f  ene=%.3f\n",i,rdo,adc,et,ene);
  }
}

#endif
/**********************************************************************
  $Log: L2bemcGamma2009.cxx,v $
  Revision 1.1  2011/03/09 16:29:07  pibero
  Added L2gamma2009

  Revision 1.6  2008/01/30 21:56:40  balewski
  E+B high-enery-filter L2-algo fuly functional

  Revision 1.5  2008/01/30 00:47:17  balewski
  Added L2-Etow-calib

  Revision 1.4  2008/01/18 23:29:13  balewski
  now L2result is exported

  Revision 1.3  2008/01/17 23:15:51  balewski
  bug in token-addressed memory fixed

  Revision 1.2  2008/01/16 23:32:35  balewski
  toward token dependent compute()

  Revision 1.1  2007/12/19 02:30:18  balewski
  new L2-btow-calib-2008


 
*/


