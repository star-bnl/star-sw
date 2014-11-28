#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

/*********************************************************************
 * $Id: L2wBemc2012.cxx,v 1.4 2012/03/21 18:18:04 jml Exp $
 * \author Jan Balewski,MIT , 2009 
 *********************************************************************
 * Descripion: see .h
  *********************************************************************
 */
// super smart macro from Pibero, needs only final output 'long long' as argument
#define rdtscll(val) do { \
    unsigned int __a,__d; \
    __asm__ __volatile__("rdtsc" : "=a" (__a), "=d" (__d)); \
    (val) = ((unsigned long long)__a) | (((unsigned long long)__d)<<32); \
} while(0)

const float stepETH=5;// for QA histo


#ifdef  IS_REAL_L2  //in l2-ana  environment
  #include "../L2algoUtil/L2EmcDb2012.h"
  #include "../L2algoUtil/L2Histo.h"
#else
  #include "../L2algoUtil/L2EmcDb2012.h"
  #include "../L2algoUtil/L2Histo.h"
  #include "../L2algoUtil/L2EmcGeom2012.h"
#endif

#include "L2wBemc2012.h"

//=================================================
//=================================================
L2wBemc2012::L2wBemc2012(const char* name, const char *uid, L2EmcDb2012* db, L2EmcGeom2012 *geoX, char* outDir, int resOff)  :  L2VirtualAlgo2012( name,uid,  db, outDir, true, false, resOff) { 
  /* called one per day
     all memory allocation must be done here
  */

  mGeom=geoX; 
  if (!mGeom)
    criticalError((char*)"L2wBemc is broken -- can't find geom.");

  setMaxHist(32); // set upper range, I uses only 2^N -it is easier to remember
  createHisto();

  //------- self-consistency checks, should never fail
   if (sizeof(L2wResult2012)!= L2wResult2012::mySizeChar) 
    criticalError((char*)"L2wBemc has failed consistency check. sizeof(L2wResult2012)!= L2wResult2012::mySizeChar");
  
}

/* ========================================
  ======================================== */
int 
L2wBemc2012::initRunUser( int runNo, int *rc_ints, float *rc_floats) {

  // unpack params from run control GUI
  par_dbg           =  rc_ints[0];
  par_RndAcceptPrescale      =  rc_ints[1];
  par_seedEtThres   =  rc_floats[0];
  par_clustEtThres  =  rc_floats[1];

  // verify consistency of input params
  int kBad=0;
  kBad+=0x00002 * (par_clustEtThres<par_seedEtThres);

  // put notes about configuration into the log file
  if (mLogFile) { 
    fprintf(mLogFile,"L2%s algorithm initRun(R=%d), compiled: %s , %s\n params:\n",
	    getName(),mRunNumber,__DATE__,__TIME__);
    fprintf(mLogFile," - use  seedThres=%.2f (GeV), debug=%d, prescale=%d (0=off,1=100prc, 2=50proc, etc)\n",
	    par_seedEtThres,par_dbg, par_RndAcceptPrescale);
    fprintf(mLogFile," - accept event cluster Thres=%.2f (GeV)\n",
	    par_clustEtThres);
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

  sprintf(txt,"W-BTOW-accepted: acc seed tower ET>%.2f GeV; BTOW softID",par_seedEtThres);
  hA[9]->setTitle(txt);
  
  sprintf(txt,"W Btow accepted, seed tower Et>%.2f GeV; eta bin [-1,+1]; y: phi bin ~ TCP sector",par_seedEtThres);
  hA[10]->setTitle(txt);
  

  for ( int index=0; index<EmcDbIndexMax; index++ )
     {
       const L2EmcDb2012::EmcCDbItem *x = mDb->getByIndex(index);
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
L2wBemc2012::sumET(int phi, int eta) {
  int tow = BtowGeom::mxEtaBin *((phi+BtowGeom::mxPhiBin)%BtowGeom::mxPhiBin) + ((eta+BtowGeom::mxEtaBin)%BtowGeom::mxEtaBin); // phi- changes faster

  const int maxTowers = BtowGeom::mxEtaBin * BtowGeom::mxPhiBin;
  int towPlusOne;
  float sum;
  sum=0;
  sum=wrkBtow_et[tow];
  towPlusOne = tow+1;
  towPlusOne%= maxTowers;
  sum+=wrkBtow_et[towPlusOne];
  
  tow+=BtowGeom::mxEtaBin;
  tow%=maxTowers;
  
  sum+=wrkBtow_et[tow];
  
  towPlusOne = tow+1;
  towPlusOne%= maxTowers;
  sum+=wrkBtow_et[towPlusOne];
  
  return sum;
}
  

/* ========================================
  ======================================== */
void 
L2wBemc2012::computeUser(int token){

  clearEvent(token);

  // ------ PROJECT INPUT LIST TO 2D ARRAY AND SCAN FOR SEED TOWERS ----
  int i;

  // access inpute list
  const HitTower1 *hit=mEveStream_btow[token].get_hits();
  const int hitSize=mEveStream_btow[token].get_hitSize();
  // preapre output list
  L2wBemcEvent2012 *btowEve=mBtow+token;

  for(i=0;i< hitSize;i++,hit++) {
    int tower=mRdo2tower[hit->rdo];
    wrkBtow_et[tower]=hit->et;
    if(hit->et<par_seedEtThres)continue;
    wrkBtow_tower_seed[wrkBtow_tower_seed_size++]=tower;
  }
  hA[2]->fill(hitSize);
  int seedTow=-1,seedEta=-1,seedPhi=-1;
  float clustET=0;
  btowEve->isFresh=L2wBemcEvent2012::kDataFresh;

  // ----------- FIND 2x2 CLUSTER AROUND EVERY SEED -----
  for(i=0; i<wrkBtow_tower_seed_size;i++) {
    seedTow=wrkBtow_tower_seed[i];
    seedEta=seedTow%BtowGeom::mxEtaBin;
    seedPhi=seedTow/BtowGeom::mxEtaBin;
    
    //.... find first 2x2 above cluster thresh
    if (seedEta < BtowGeom::mxEtaBin) {
      clustET = sumET(seedPhi,seedEta);
      if(clustET>par_clustEtThres) goto ACCEPT;
      clustET = sumET(seedPhi-1,seedEta);
      if(clustET>par_clustEtThres) goto ACCEPT;
    }
    if (seedEta > 0 ) {
      clustET = sumET(seedPhi-1,seedEta-1);
      if(clustET>par_clustEtThres) goto ACCEPT;
      clustET = sumET(seedPhi,seedEta-1);
      if(clustET>par_clustEtThres) goto ACCEPT;
    }
  }
  //.... ABORT
  btowEve->resultBlob.seedEt=0;
  btowEve->resultBlob.clusterEt=0;
  btowEve->resultBlob.seedEtaBin=0;
  btowEve->resultBlob.seedPhiBin=0;
  btowEve->resultBlob.trigger=0;
  btowEve->seedET=0;
  btowEve->clusterET=0;
  btowEve->tkCompute=0;
  return;
  
 ACCEPT:
  btowEve->seedET=wrkBtow_et[seedTow];
  btowEve->clusterET=clustET;
  btowEve->resultBlob.seedEt   =(unsigned char)(wrkBtow_et[seedTow]*256.0/60.0);
  btowEve->resultBlob.clusterEt=(unsigned char)(clustET*256.0/60.0);
  btowEve->resultBlob.seedEtaBin=seedEta;
  btowEve->resultBlob.seedPhiBin=seedPhi;
  btowEve->resultBlob.trigger=2;
  rdtscll( btowEve->tkCompute);

  return;
}




/* ========================================
  ======================================== */
bool 
L2wBemc2012::decisionUser(int token, int *myL2Result){
  // INPUT: token + comput() results stored internally
  // OUTPUT: yes/now + pointer to  L2Result

  // get pointers to internal private event storage
  L2wBemcEvent2012 *btowEve=mBtow+token;

  // prescaling decison ws already made in Virtual:
  // if (par_RndAcceptPrescale>0 && mRandomAccept) btowEve->resultBlob.trigger+=1;
  if (mRandomAccept) btowEve->resultBlob.trigger+=1;
  
  //...... some histos just for QA
  if(btowEve->isFresh>L2wBemcEvent2012::kDataFresh) mhN->fill(6); // stale data, should never happen

  btowEve->isFresh++; // mark the data as  stale

  if(btowEve->resultBlob.trigger&2) {
    unsigned long long tkDecision;
    rdtscll(tkDecision);
    int tkDelta=tkDecision-btowEve->tkCompute;					
    hA[1]->fill(tkDelta/1000);

    hA[3]->fill((int)btowEve->seedET);
    hA[4]->fill((int)btowEve->clusterET);
    hA[5]->fill((int)(100.*btowEve->seedET/btowEve->clusterET));
    int jET=(int)(btowEve->clusterET/stepETH);
    if( jET<=1) jET=1;
    if( jET>=9) jET=9;
    hA[10+jET]->fill(btowEve->resultBlob.seedEtaBin,btowEve->resultBlob.seedPhiBin);
    hA[10]->fill(btowEve->resultBlob.seedEtaBin,btowEve->resultBlob.seedPhiBin);
  }
  // store final content in TRG data
  memcpy(myL2Result,&(btowEve->resultBlob),sizeof(L2wResult2012));
  if(par_dbg) L2wResult2012_print((L2wResult2012 *)&(btowEve->resultBlob));
  //return btowEve->resultBlob.trigger;
  return btowEve->resultBlob.trigger&2;
} 


/* ========================================
  ======================================== */
void
L2wBemc2012::finishRunUser() {  /* called once at the end of the run */
  // do whatever you want, log-file & histo-file are still open
  
  if (mLogFile){ 
    fprintf(mLogFile,"finishRunUser-%s start\n",getName());
  }
  
  // test1:
  //  hA[9]->fillW(21,1234);
  
  //......... scan for hot towers ....& repack 2D--> 1D(softId)

  for(int jh=10;jh<19;jh++) 
  {
    const L2EmcDb2012::EmcCDbItem *xB=mDb->getByIndex(402);
    const int *hiData = hA[jh]->getData();
    int hotY=0,totY=0;
    for(int i=0; i<EmcDbIndexMax; i++) {
      const L2EmcDb2012::EmcCDbItem *x=mDb->getByIndex(i);
      if(mDb->isEmpty(x)) continue;
      if (!mDb->isBTOW(x) ) continue;
      int ieta= (x->eta-1);
      int iphi= (x->sec-1)*10 + x->sub-'a' ;
      int softId=atoi(x->tube+2);
      int ix=ieta+iphi*BtowGeom::mxEtaBin;
      int yield=hiData[ix];
      totY+=yield;
      if(jh==10) hA[9]->fillW(softId,yield);// do it only for ET-integrated
      if(hotY>yield)continue;
      hotY=yield;
      xB=x;
    }

    int ieta= (xB->eta-1);
    int iphi= (xB->sec-1)*10 + xB->sub-'a' ;
    int softId=atoi(xB->tube+2);
    fprintf(mLogFile,"#BTOW hot *candidate*, hist: %d,  yield: %d, totYield: %d, softID: %d, crate: %d, chan: %d, name: %s, ieta: %d, iphi: %d\n", hA[jh]->getId(),hotY,totY,softId,xB->crate,xB->chan,xB->name,ieta,iphi);
  }

    

}


//=======================================
//=======================================
void 
L2wBemc2012::createHisto() {
  hA[1]=new L2Histo(1,(char*)"W Btow delTime (decision-compute); kTicks",300);
  
  hA[2]=new L2Histo(2,(char*)"W Btow-compute: # btow towers w/ energy /event; x: # BTOW towers; y: counts", 100); 
  hA[3]=new L2Histo(3,(char*)"W Btow-accepted: seeds ; Seed ET (GeV)", 70); 
  hA[4]=new L2Histo(4,(char*)"W Btow-accepted: clusters ; Cluster ET ET(GeV)", 70);
  hA[5]=new L2Histo(5,(char*)"W Btow-accepted: cluster shape ;100*Seed Et/Cluster Et", 105);
  // 6-8 free

  hA[9]=new L2Histo(9,(char*)"W Btow ...9 .....", 5000); // title in initRun
  hA[10]=new L2Histo(10,(char*)"W Btow ...10 ...", BtowGeom::mxEtaBin, BtowGeom::mxPhiBin); // title in initRun


  char tit[100];
  for(int j=1;j<=9;j++){
    int hid=10+j;
    float x1=j*stepETH;
    float x2=x1+stepETH;
    if(j==1) x1=0;
    if(j==9) x2=999;
    sprintf(tit,"W Btow-accept: cluster  %.0f <ET< %.0f GeV ; seed eta bin [-1,+1];  seed phi bin ~ TCP sector", x1,x2);

    hA[hid]=new L2Histo(hid,tit, BtowGeom::mxEtaBin, BtowGeom::mxPhiBin);
  }

  //20-32 free

}

//=======================================
//=======================================
void 
L2wBemc2012::clearEvent(int token){
  memset(wrkBtow_et,0,sizeof(wrkBtow_et)); 
  wrkBtow_tower_seed_size=0; 
  memset(&(mBtow[token].resultBlob),0, sizeof(L2wResult2012));
}
  
/* ========================================
  ======================================== */
void 
L2wBemc2012::print2(){ // full , local ADC array
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
L2wBemc2012::print3(){ // seed list
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
L2wBemc2012::print4(int token, int hitSize){ // L2-algo input list
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
  $Log: L2wBemc2012.cxx,v $
  Revision 1.4  2012/03/21 18:18:04  jml
  got rid of printfs from 2012 files

  Revision 1.3  2011/10/19 16:12:12  jml
  more 2012 stuff

  Revision 1.2  2011/10/19 15:39:45  jml
  2012

  Revision 1.1  2011/10/18 15:11:44  jml
  adding 2012 algorithms

  Revision 1.2  2009/11/19 15:48:49  balewski
  add (char*) to many strings to make SL5 happ, few other adjustments

  Revision 1.1  2009/03/28 19:43:53  balewski
  2009 code


 
*/


