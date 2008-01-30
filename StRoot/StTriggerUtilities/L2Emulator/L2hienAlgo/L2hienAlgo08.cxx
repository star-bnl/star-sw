#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

/*****************************************************************
 * $Id: L2hienAlgo08.cxx,v 1.2 2008/01/30 00:47:23 balewski Exp $
 * \author Jan Balewski, MIT, 2008 
 *****************************************************************
 * Descripion: see .h
  ***************************************************************
 */


#ifdef  IS_REAL_L2  //in l2-ana  environment
  #include "../L2algoUtil/L2EmcDb.h"
  #include "../L2algoUtil/L2Histo.h"
#else
  #include "../L2algoUtil/L2EmcDb.h"
  #include "../L2algoUtil/L2Histo.h"
//  #include "../L2algoUtil/L2EmcGeom.h"
#endif

#include "L2hienAlgo08.h"


//=================================================
//=================================================
L2hienAlgo08::L2hienAlgo08(const char* name, L2EmcDb* db, L2EmcGeom *geoX, char* outDir)  :  L2VirtualAlgo2008( name,  db,  outDir) { 
  /* called one per days
     all memory allocation must be done here
  */

  // geoX not used
  setMaxHist(16); // set upper range, I uses only 2^N -it is easier to remember
  createHisto();

}

/* ========================================
  ======================================== */
int 
L2hienAlgo08::initRunUser( int runNo, int *rc_ints, float *rc_floats) {

  // unpack params from run control GUI
  par_dbg        =  rc_ints[0];
  par_adcThres   =  rc_ints[1];
  par_maxList    =  rc_ints[2];

  // verify consistency of input params
  int kBad=0;
  kBad+=0x00001 * (par_adcThres<10);

  // fix unreasonable params
  if(par_maxList<10 ) par_maxList=10;
  if(par_maxList>L2hienEvent08::mxListSize ) par_maxList=L2hienEvent08::mxListSize; 

  if (mLogFile) { 
    fprintf(mLogFile,"L2%s algorithm initRun(R=%d), compiled: %s , %s\n params:\n",getName(),mRunNumber,__DATE__,__TIME__);
    fprintf(mLogFile," - use  adcThresh =%d, maxList=%d debug=%d\n", par_adcThres , par_maxList, par_dbg);
    fprintf(mLogFile,"initRun() params checked for consistency, Error flag=0x%04x\n",kBad);
  }
  if(kBad) return kBad;    
  
  // clear content of all histograms & token-dependet memory
  int i;
  for (i=0; i<mxHA;i++) if(hA[i])hA[i]->reset();
  memset(mHiEnTw,0,sizeof(mHiEnTw));


  // update tiltles of histos, add values of params
  char txt[1000];
  sprintf(txt,"compute: # towers ADC-ped>%d / event; x: # towers; y: counts",par_adcThres);
  hA[2]->setTitle(txt);

  sprintf(txt,"accepted: # towers ADC-ped>%d / event; x: # towers; y: counts",par_adcThres);
  hA[3]->setTitle(txt);

  sprintf(txt,"accepted: # towers ADC-ped>%d vs. eta ring;x: eta-bin, 0=eta=-1.0; y: counts",par_adcThres*2);
  hA[5]->setTitle(txt);

  sprintf(txt,"accepted: # towers ADC-ped>%d vs. phi ring;x: phi-bin, 0=12:30, follows TPC; y: counts",par_adcThres*2);
  hA[6]->setTitle(txt);

  sprintf(txt,"accpeted: ADC sum , towers w/ ADC-ped>%d ; ADC sum/50; y: counts",par_adcThres);
  hA[7]->setTitle(txt);

  // prepare BTOW lookup tables

  for ( int index=0; index<EmcDbIndexMax; index++ )     {
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
    
    mRdo2towerID[ rdo ] = tow; // returns towerID vs.rdo
    mTowerID2etaBin[tow]=eta; // range [0..39]
    mTowerID2phiBin[tow]=phi; // range [0..120]
  }
  return 0; //OK
  
}               

  

/* ========================================
  ======================================== */
void 
L2hienAlgo08::computeUser(int token){
  // token range is guaranteed by virtual08-class

  // -----------  SCAN FOR HIGH TOWERS ----
  
  clearEvent(token);

  int i;
  // get pointer to input list towers from calib-algo
  const HitTower1 *hit=mEveStream_btow[token].get_hits();
  const int hitSize=mEveStream_btow[token].get_hitSize();
  // printf("L2-%s-compute: ---EMC ADC list---token=%d input size=%d\n",getName(),token,hitSize);

  // get pointers to internal, token indexed, output event storage
  L2hienEvent08 *hiTwEve=mHiEnTw+token;
  unsigned int *value=hiTwEve->value;

  for(i=0;i< hitSize;i++,hit++) {
    // printf(" adc in=%d ",hit->adc);
    if(hit->adc<par_adcThres) continue;
    if(hiTwEve->size>=par_maxList) break;
    int softID=mRdo2towerID[hit->rdo];
    // printf("*");
    (*value)= ((hit->adc)<<16 )+ softID;
    // printf("A %p %x  adc=%d softID=%d \n", value, *value, hit->adc,softID); 
    hiTwEve->size++;
    value++;
  }
  //  printf("L2-%s-compute: saved: size%d val[0]=0x%x p=%p\n",getName(),hiTwEve->size,hiTwEve->value[0],hiTwEve->value); 

  hiTwEve->isFresh=L2hienEvent08::kDataFresh;
  hA[2]->fill(hiTwEve->size);
} 


/* ========================================
  ======================================== */
bool 
L2hienAlgo08::decisionUser(int token, void **myL2Result){
  // INPUT: token + comput() results stored internally
  // OUTPUT: yes-always &  NO pointer to  L2Result=empty

  // get pointers to internal private event storage
  L2hienEvent08 *hiTwEve=mHiEnTw+token;
  (*myL2Result)=0;// empty

  //printf("L2-%s-decision: ---EMC ADC list---token=%d size=%d val[0]=0x%x p=%p\n",getName(),token,hiTwEve->size,hiTwEve->value[0],hiTwEve->value);

  //...... some histos for QA 
  if(hiTwEve->size>= par_maxList) mhN->fill(5);  // was overflow
  if(hiTwEve->isFresh>L2hienEvent08::kDataFresh) mhN->fill(6); // stale data
  hiTwEve->isFresh++; // mark the data as  stale 

  hA[3]->fill(hiTwEve->size);
  unsigned int *value=hiTwEve->value;
  int ic;
  int adcSum=0;
  int adcQaThres=2*par_adcThres;
  for(ic=0;ic<hiTwEve->size;ic++,value++) {
    int adc=(*value)>>16;
    int softID=(*value)&0xffff;
    adcSum+=adc;
    //printf(" got adc=%d softID=%d, ",adc,softID);
    hA[4]->fill(adc/10);
    if(adc<adcQaThres) continue;
    hA[5]->fill(mTowerID2etaBin[softID]);
    hA[6]->fill(mTowerID2phiBin[softID]);
  }
  hA[7]->fill(adcSum/50);

  // printf("\n");

  //........ fill (some) L2Result
  // btowEve->resultBlob.kTicksCompute=mComputeTimeDiff[token]/1000;
  //  btowEve->resultBlob.decision=0;
  // btowEve->resultBlob.numberOfL2Clust=btowEve->size;

  
  // debugging should be off for any time critical computation
  if(par_dbg>0){
    print2(token);

  } 

  /****************/
  /* accept always */
  /****************/
  return true;

} 


/* ========================================
  ======================================== */
void
L2hienAlgo08::finishRunUser() {  /* called once at the end of the run */
  // do whatever you want, log-file & histo-file are still open
  
  if (mLogFile){ 
    fprintf(mLogFile,"finishRunUser-%s bhla bhla\n",getName());
  }
  
}


//=======================================
//=======================================
void 
L2hienAlgo08::createHisto() {
  memset(hA,0,sizeof(hA));

  hA[2]=new L2Histo(2,"compute: #towers w/ .....energy /event; x: #  towers; y: counts", 100); // title set in initRun
  hA[3]=new L2Histo(3,"decision: #towers w/ .....energy /event; x: #  towers; y: counts", 100); // title set in initRun

  hA[4]=new L2Histo(4,"accepted: towers ET ; x: ADC/10", 200);
  hA[5]=new L2Histo(5,"accepted: #towers w/ ..... vs. eta ring",40); // title set in initRun
  hA[6]=new L2Histo(6,"accepted: #towers w/ .... vs. phi ring",120); // title set in initRun
  hA[7]=new L2Histo(7,"accepted: ADC sum ...",200); // title set in initRun

  // printf("L2-%s::createHisto() done\n",getName());
}

//=======================================
//=======================================
void 
L2hienAlgo08::clearEvent(int token){
  mHiEnTw[token].size=0; // do not set 'fresh-flag here, only after compute() finishes
  //  memset(&(mBtow[token].resultBlob),0, sizeof(L2hienResult08));
}
  
/* ========================================
  ======================================== */
void 
L2hienAlgo08::print2(int token){ // full , local ADC array
  assert(token>0);
  assert(token<L2eventStream2008::mxToken);

  int  hiSize=getListSize(token);
  const unsigned int  *value=getListData(token);
  
  printf("pr2-%s: dump %d acceted towers for token=%d\n softID   ADC-ped\n",getName(),hiSize, token);
  for(int ic=0;ic<hiSize;ic++,value++) {
    int adc=(*value)>>16;
    int softID=(*value)&0xffff;
    printf("%4d %d, \n",softID,adc);
  }
}


/**********************************************************************
  $Log: L2hienAlgo08.cxx,v $
  Revision 1.2  2008/01/30 00:47:23  balewski
  Added L2-Etow-calib

  Revision 1.1  2008/01/29 00:17:13  balewski
  new algo filtering high-energy towers



 
*/


