#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

/*********************************************************************
 * $Id: L2exampleAlgo08.cxx,v 1.6 2008/01/30 21:56:40 balewski Exp $
 * \author Jan Balewski,MIT , 2008 
 *********************************************************************
 * Descripion: see .h
  *********************************************************************
 */


#ifdef  IS_REAL_L2  //in l2-ana  environment
  #include "../L2algoUtil/L2EmcDb.h"
  #include "../L2algoUtil/L2Histo.h"
#else
  #include "../L2algoUtil/L2EmcDb.h"
  #include "../L2algoUtil/L2Histo.h"
  #include "../L2algoUtil/L2EmcGeom.h"
#endif

#include "L2exampleAlgo08.h"


//=================================================
//=================================================
L2exampleAlgo08::L2exampleAlgo08(const char* name, L2EmcDb* db, L2EmcGeom *geoX, char* outDir)  :  L2VirtualAlgo2008( name,  db,  outDir) { 
  /* called one per days
     all memory allocation must be done here
  */

  mGeom=geoX; assert(mGeom); // avaliable but not used in this example
  setMaxHist(16); // set upper range, I uses only 2^N -it is easier to remember
  createHisto();

  //------- self-consistency checks, should never fail
  // printf("ZZ %d %d\n", sizeof(L2exampleResult08), L2exampleResult08::mySizeChar);
  assert(sizeof(L2exampleResult08)== L2exampleResult08::mySizeChar);
}

/* ========================================
  ======================================== */
int 
L2exampleAlgo08::initRunUser( int runNo, int *rc_ints, float *rc_floats) {

  // unpack params from run control GUI
  par_dbg           =  rc_ints[0];
  par_seedEtThres   =  rc_floats[0];
  par_clusterEtThres=  rc_floats[1];
  par_eventEtThres  =  rc_floats[2];

  // verify consistency of input params
  int kBad=0;
  kBad+=0x00001 * (par_seedEtThres<1.0);
  kBad+=0x00002 * (par_clusterEtThres<par_seedEtThres);

  if (mLogFile) { 
    fprintf(mLogFile,"L2%s algorithm initRun(R=%d), compiled: %s , %s\n params:\n",getName(),mRunNumber,__DATE__,__TIME__);
    fprintf(mLogFile," - use  Thres/GeV seed=%.2f, clusterList=%.2f debug=%d\n", par_seedEtThres,par_clusterEtThres, par_dbg);
    fprintf(mLogFile," - accept event cluster Thres/GeV=%.2f\n",par_eventEtThres);
    fprintf(mLogFile,"initRun() params checked for consistency, Error flag=0x%04x\n",kBad);
  }

  if(kBad) return kBad;    


  // clear content of all histograms & token-dependet memory
  int i;
  for (i=0; i<mxHA;i++) if(hA[i])hA[i]->reset();
  memset(mBtow,0,sizeof(mBtow));
 
  // update tiltles of histos
  char txt[1000];
  sprintf(txt,"BTOW-compute: #seed towers ET>%.2f GeV / event; x: # BTOW towers; y: counts",par_seedEtThres);
  hA[3]->setTitle(txt);
  
  sprintf(txt,"BTOW-decision: #cluster ET>%.2f GeV / event ; x: # BTOW towers; y: counts",par_clusterEtThres);
  hA[4]->setTitle(txt);

 sprintf(txt,"BTOW-decision: acc cluster ET>%.2f GeV; x:cluster ET(GeV) ; y: counts",par_eventEtThres);
  hA[4]->setTitle(txt);
  
  

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
       assert(tow>=0); assert(tow<=mxBtow);
       assert(rdo>=0); assert(rdo<=mxBtow);
       
       mTower2rdo[ tow ] = rdo;    // returns rdo channel for given tower
       mRdo2tower[ rdo ] = tow;    // returns tower for given rdo channel
    }
  return 0; //OK

}               

/* ========================================
  ======================================== */
float
L2exampleAlgo08::sumET(int phi, int eta) {
  int tow = BtowGeom::mxEtaBin *phi + eta; // phi- changes faster
  float sum=wrkBtow_et[tow]+wrkBtow_et[tow+1];
  //  printf("tow : %d, %d --> %f %f \n",tow, tow+1,wrkBtow_et[tow],wrkBtow_et[tow+1]);
  tow+=BtowGeom::mxEtaBin;
  // printf("tow : %d, %d --> %f %f \n",tow, tow+1,wrkBtow_et[tow],wrkBtow_et[tow+1]);
  sum+=wrkBtow_et[tow]+wrkBtow_et[tow+1];
  return sum;
}
  

/* ========================================
  ======================================== */
void 
L2exampleAlgo08::computeUser(int token){
  // token range is guaranteed by virtual08-class

  /* primitive 2x2 cluster finder:
     - ignores seed towers at eta or phi boundary
     - doublcounts if 2 seeds are neighbours
  */
  
  clearEvent(token);

  // ------ PROJECT INPUT LIST TO 2D ARRAY AND SCAN FOR SEED TOWERS ----
  int i;
  //  printf("L2-%s-compute: ---BTOW ADC list--- size=%d\n",getName(),*globEve_btow_hitSize);

  const HitTower1 *hit=mEveStream_btow[token].get_hits();
  const int hitSize=mEveStream_btow[token].get_hitSize();
  for(i=0;i< hitSize;i++,hit++) {
    int tower=mRdo2tower[hit->rdo];
    wrkBtow_et[tower]=hit->et;
    if(hit->et<par_seedEtThres)continue;
    wrkBtow_tower_seed[wrkBtow_tower_seed_size++]=tower;
  }
  hA[2]->fill(hitSize);
  hA[3]->fill(wrkBtow_tower_seed_size);

  // ----------- FIND 2x2 CLUSTER AROUND EVERY SEED -----

  // get pointers to internal private event storage
  L2exampleEvent08 *btowEve=mBtow+token;

  // compute & store values
  for(i=0;i<wrkBtow_tower_seed_size;i++) {
    int seedTow=wrkBtow_tower_seed[i];
    int seedEta=seedTow%BtowGeom::mxEtaBin;
    int seedPhi=seedTow/BtowGeom::mxEtaBin;
    //    float seedET= wrkBtow_et[seedTow];
    // printf("sumE seed ET=%.3f tow=%d phiBin=%d etaBin=%d\n",seedET,seedTow,seedPhi,seedEta);
    
    // ........drop seeds close to boundaries, fix it 
    if(seedEta==0 || seedEta==BtowGeom::mxEtaBin-1) continue;
    if(seedPhi==0 || seedPhi==BtowGeom::mxPhiBin-1) continue;
    // now every seed has 4 2x2 clusters

    /* Pibero's idea to avoid double counting:
       "drop seed if a neighbour tower has more energy"
       has not been implemented - sounds great!
    */ 

    //.... find max of 4 possible clusters
    float maxET=sumET(seedPhi,seedEta);
    float sum=sumET(seedPhi-1,seedEta);
    if(maxET<sum) maxET=sum;
    sum=sumET(seedPhi-1,seedEta-1);
    if(maxET<sum) maxET=sum;
    sum=sumET(seedPhi,seedEta-1);
    if(maxET<sum) maxET=sum;
    // printf("CCC  clustET=%f \n",maxET);
    if(maxET<par_clusterEtThres)continue; 
    if(btowEve->size>=L2exampleEvent08::mxClust) continue; // overflow protection
    //........record largest cluster for this seed....
    btowEve->clusterET[btowEve->size++]=maxET;
   }// end of cluster search
  
  btowEve->isFresh=L2exampleEvent08::kDataFresh;
  // printf("compuzzzzzzzzzzzzzzzzz s=%d  tkn=%d\n",*btow_clusterET_size,token);
  
  // debugging should be off for any time critical computation
  if(par_dbg>0){
    printf("dbg=%s, btow-adcL-size=%d\n",getName(),hitSize);
    // if(par_dbg>0) print1();
    print2();
    print3();// tmp, must have token
  } 
} 


/* ========================================
  ======================================== */
bool 
L2exampleAlgo08::decisionUser(int token, void **myL2Result){
  // INPUT: token + comput() results stored internally
  // OUTPUT: yes/now + pointer to  L2Result

  // get pointers to internal private event storage
  L2exampleEvent08 *btowEve=mBtow+token;
  (*myL2Result)=&(btowEve->resultBlob);
  //  printf("eee %p\n",*myL2Result);
  int ic;  

  //...... some histos just for fun
  if(btowEve->size>= L2exampleEvent08::mxClust) mhN->fill(5);  // was overflow
  if(btowEve->isFresh>L2exampleEvent08::kDataFresh) mhN->fill(6); // stale data
  btowEve->isFresh++; // mark the data as  stale

  hA[4]->fill(btowEve->size);

  for(ic=0;ic<btowEve->size;ic++) {
    float clustET=btowEve->clusterET[ic];
    hA[5]->fill((int)clustET);
    // printf("DDD clust ET=%.3f\n", clustET);
    if(clustET<par_eventEtThres) continue;
    hA[6]->fill((int)clustET);
  }
  // printf("clustzzzzzzzzzzzzzzzzz s=%d  tkn=%d\n",btow_clusterET_size,token);

  //........ fill (some) L2Result
  btowEve->resultBlob.kTicksCompute=mComputeTimeDiff[token]/1000;
  btowEve->resultBlob.decision=0;
  btowEve->resultBlob.numberOfL2Clust=btowEve->size;

  //........ compute the final decision
  for(ic=0;ic<btowEve->size;ic++) {
    if(btowEve->clusterET[ic]<par_eventEtThres) continue;
    btowEve->resultBlob.decision=1;
    btowEve->resultBlob.clusterET=btowEve->clusterET[ic];
    goto accepted;  
  }
  return false;

  /****************/
 accepted:
  /****************/
  if(btowEve->size>= L2exampleEvent08::mxClust) mhN->fill(15);
  return true;

} 


/* ========================================
  ======================================== */
void
L2exampleAlgo08::finishRunUser() {  /* called once at the end of the run */
  // do whatever you want, log-file & histo-file are still open
  
  if (mLogFile){ 
    fprintf(mLogFile,"finishRunUser-%s bhla bhla\n",getName());
  }
  
}


//=======================================
//=======================================
void 
L2exampleAlgo08::createHisto() {
  memset(hA,0,sizeof(hA));

  hA[2]=new L2Histo(2,"BTOW-compute: #towers w/ energy /event; x: # BTOW towers; y: counts", 100); 
  hA[3]=new L2Histo(3,"BTOW-compute: #seed ....... ", 100); // title in initRun
  hA[4]=new L2Histo(4,"BTOW-decision: #clust ....... ", 50); // title in initRun

  hA[5]=new L2Histo(5,"BTOW-decision: any cluster ; x: ET(GeV)", 30);
  hA[6]=new L2Histo(6,"BTOW-decision: accepted clust ...  ; x: ET(GeV)", 30);// title in initRun

  // printf("L2-%s::createHisto() done\n",getName());
}

//=======================================
//=======================================
void 
L2exampleAlgo08::clearEvent(int token){
  memset(wrkBtow_et,0,sizeof(wrkBtow_et)); 
  memset(wrkBtow_tower_seed,0,sizeof(wrkBtow_tower_seed));
  wrkBtow_tower_seed_size=0; 
  mBtow[token].size=0; // do not set 'fresh-flag here, only after compute() finishes
  memset(&(mBtow[token].resultBlob),0, sizeof(L2exampleResult08));
}
  
/* ========================================
  ======================================== */
void 
L2exampleAlgo08::print2(){ // full , local ADC array
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
L2exampleAlgo08::print3(){ // seed list
  int i;
  printf("pr3-%s: ---seed list, size=%d\n",getName(),wrkBtow_tower_seed_size);

  for(i=0;i<wrkBtow_tower_seed_size;i++) {
    int tower=wrkBtow_tower_seed[i];
    float et=wrkBtow_et[tower];
    printf("  btow: i=%4d  tower=%4d   et=%.3f \n",i,tower,et);
  }

}

/**********************************************************************
  $Log: L2exampleAlgo08.cxx,v $
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


