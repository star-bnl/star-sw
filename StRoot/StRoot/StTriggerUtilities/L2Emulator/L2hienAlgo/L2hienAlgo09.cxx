#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

/***********************************************************
 * $Id: L2hienAlgo09.cxx,v 1.1 2011/09/22 20:44:10 pibero Exp $
 * \author Jan Balewski, MIT, 2008 
 ***********************************************************
 * Descripion: see .h
 **********************************************************
 */

#ifdef  IS_REAL_L2  //in l2-ana  environment
  #include "../L2algoUtil/L2EmcDb.h"
  #include "../L2algoUtil/L2Histo.h"
#else    //full path needed for cvs'd code
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2EmcDb.h"
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2Histo.h"
#endif

#include "L2hienAlgo09.h"
#include "L2hienResult2009.h"

//=================================================
//=================================================
L2hienAlgo09::L2hienAlgo09(const char* name, L2EmcDb* db, L2EmcGeom *geoX, char* outDir, int resOff, L2VirtualAlgo2009::EmcSwitch  beSwitch)  :  L2VirtualAlgo2009( name,  db, outDir, (beSwitch==kIsBtow),(beSwitch==kIsEtow), resOff ) { 
  /* called one per days
     all memory allocation must be done here
  */

  // geoX is not used, ignore

  if(!(beSwitch==kIsBtow || beSwitch==kIsEtow))
    {
      criticalError("L2hienAlgo called with broken beSwitch.  L2hien cannot run.\n");
    }

  if (beSwitch==kIsBtow)
    {
      if (sizeof(L2hienResult2009)!= L2hienResult2009::mySizeChar) 
	criticalError("L2hienAlgo09 (Barrel) has failed consistency check. sizeof(L2hienResult2009)!= L2hienResult2009::mySizeChar");
    }
  mSwitch=beSwitch;
  setMaxHist(16); // set upper range, I uses only 2^N -it is easier to remember
  createHisto(); // identical for B or EEMC

}

/* ========================================
  ======================================== */
int 
L2hienAlgo09::initRunUser( int runNo, int *rc_ints, float *rc_floats) {

  // unpack params from run control GUI
  par_dbg        =  rc_ints[0];
  par_adcThres   =  rc_ints[1];
  par_maxList    =  rc_ints[2];
  par_RndAcceptPrescale = 0;//rc_ints[3];  Isn't currently in RC, so can't have it here.

  // verify consistency of input params
  int kBad=0;
  kBad+=0x00001 * (par_adcThres<10);

  // fix unreasonable params
  if(par_maxList<10 ) par_maxList=10;
  if(par_maxList>L2hienList09::mxListSize ) par_maxList=L2hienList09::mxListSize; 

  if (mLogFile) { 
    fprintf(mLogFile,"L2%s algorithm initRun(R=%d), compiled: %s , %s\n params for ",getName(),mRunNumber,__DATE__,__TIME__);
    if(mSwitch==kIsBtow)  fprintf(mLogFile," BTOW setup:\n");
    if(mSwitch==kIsEtow)  fprintf(mLogFile," ETOW setup:\n");
    fprintf(mLogFile," - use  adcThresh =%d, maxList=%d debug=%d\n", par_adcThres , par_maxList, par_dbg);
    fprintf(mLogFile,"initRun() params checked for consistency, Error flag=0x%04x\n",kBad);
  }
  if(kBad) return -kBad;    
  
  // clear content of all histograms & token-dependet memory
  int i;
  for (i=0; i<mxHA;i++) if(hA[i])hA[i]->reset();
  memset(mHiEnTw,0,sizeof(mHiEnTw));


  // update titles of histos, add values of params
  char txt[1000];
  sprintf(txt,"compute: # towers w/ ADC-ped>%d / event; x: # towers; y: counts",par_adcThres);
  hA[2]->setTitle(txt);

  sprintf(txt,"accepted: # towers w/ ADC-ped>%d / event; x: # towers; y: counts",par_adcThres);
  hA[3]->setTitle(txt);

  sprintf(txt,"accepted: # towers w/ ADC-ped>%d vs. eta ring;x: BTOW eta-bin, 0=eta=-1.0; y: counts",par_adcThres*2);
  if(mSwitch==kIsEtow) 
    sprintf(txt,"accepted: # towers w/ ADC-ped>%d vs. eta ring;x: ETOW eta-bin, 0=eta=+2.0; y: counts",par_adcThres*2);
   hA[5]->setTitle(txt);

  sprintf(txt,"accepted: # towers w/ ADC-ped>%d vs. phi ring;x: phi-bin, 0=12:30, follows TPC; y: counts",par_adcThres*2);
  hA[6]->setTitle(txt);

  sprintf(txt,"accepted: ADC sum over towers w/ ADC-ped>%d ; ADC sum/16; y: counts",par_adcThres);
  hA[7]->setTitle(txt);

  // only one set is instantiated , but memory is reserved for both

  // clear lookup tables for every new run
  memset(mRdo2towerID_B,0,sizeof(mRdo2towerID_B));
  memset(mTowerID2etaBin_B,0,sizeof(mTowerID2etaBin_B));
  memset(mTowerID2phiBin_B,0,sizeof(mTowerID2phiBin_B));

  memset(mRdo2towerID_E,0,sizeof(mRdo2towerID_E));
  memset(mTowerID2etaBin_E,0,sizeof(mTowerID2etaBin_E));
  memset(mTowerID2phiBin_E,0,sizeof(mTowerID2phiBin_E));

  if(mSwitch==kIsBtow) // prepare BTOW  lookup tables
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
       if (tow<0 || tow>mxBtow || rdo<0 || rdo>mxBtow) return -101;
      
      mRdo2towerID_B[ rdo ] = tow; // returns towerID vs.rdo
      mTowerID2etaBin_B[tow]=eta; // range [0..39]
      mTowerID2phiBin_B[tow]=phi; // range [0..119]
    }


  if(mSwitch==kIsEtow) // prepare ETOW  lookup tables
    for ( int index=0; index<EmcDbIndexMax; index++ )     {
      const L2EmcDb::EmcCDbItem *x = mDb->getByIndex(index);
      if ( x==0 ) continue;
      if ( !mDb->isETOW(x) ) continue; 
      int sec = x->sec - 1;
      int sub = 8192; 
      sub = x->sub - 'A';
      int eta = x->eta - 1;
      int phi = EtowGeom::mxSubs *sec + sub;
      int tow = EtowGeom::mxEtaBin *phi + eta; // phi- changes faster
      int rdo = x->rdo;
      if (tow<0 || tow>mxEtow || rdo<0 || rdo>mxEtow) return -101;
      
      mRdo2towerID_E[ rdo ] = tow; // returns towerID vs.rdo
      mTowerID2etaBin_E[tow]=eta; // range [0..11]
      mTowerID2phiBin_E[tow]=phi; // range [0..59]
    }

  return 0; //OK  
}               

  

/* ========================================
  ======================================== */
void 
L2hienAlgo09::computeUser(int token){
  // token range is guaranteed by virtual09-class
  // -----------  SCAN FOR HIGH TOWERS ----
  
  // reset # of towers for this token
  mHiEnTw[token].size=0;// do not set 'fresh-flag here, only after compute() finishes
  int i;
  // get pointer to input list towers from calib-algo
  const HitTower1 *hit=0;
  int   hitSize=0;
  int  *mRdo2towerID=0;

   if(mSwitch==kIsBtow) { //...... map pointers to Barrel
     hit    =mEveStream_btow[token].get_hits();
     hitSize=mEveStream_btow[token].get_hitSize();
     mRdo2towerID=mRdo2towerID_B;
   } else { //...... map pointers to Endcap
     hit    =mEveStream_etow[token].get_hits();
     hitSize=mEveStream_etow[token].get_hitSize();
     mRdo2towerID=mRdo2towerID_E;
   }

  // printf("L2-%s-compute: ---EMC ADC list---token=%d input size=%d\n",getName(),token,hitSize);
  // get pointers to internal, token indexed, output event storage
  // output is just one per instance, same format for B & ETOW
  L2hienList09 *hiTwEve=mHiEnTw+token;
  unsigned int *value=hiTwEve->value;
  unsigned int tempValue=0;
  unsigned int totalADC=0;
  int resultMaxTowers=0;
  int softID=0;
  L2hienResult2009 *hienResult=mHienResult+token;
  if (mSwitch==kIsBtow) 
    {
      resultMaxTowers=L2hienResult2009::maxTowers; //only grab the barrel towers.
      //memset to zero the Result?
    }

  for(i=0;i< hitSize;i++,hit++) {

    softID=mRdo2towerID[hit->rdo];
    totalADC+=hit->adc;
    tempValue=((hit->adc)<<16)+softID;

    if ((mSwitch==kIsBtow) && (i<L2hienResult2009::maxTowers))
      hienResult->value[i]=tempValue;
    if (hiTwEve->size<par_maxList && hit->adc>=par_adcThres)
      {
	//(*value)= ((hit->adc)<<16 )+ softID; // store composite value
	(*value)=tempValue; //same as the commented line, but no need to do same computation twice.
	hiTwEve->size++;
	value++;
      }

    // printf(" adc in=%d ",hit->adc);
    if(hiTwEve->size>=par_maxList && i>=L2hienResult2009::maxTowers) break; // overflow protection
    // printf("A %p %x  adc=%d softID=%d \n", value, *value, hit->adc,softID); 
  }

  unsigned int nTow;
  if (hitSize>255) nTow=255;
  else nTow=hitSize;

  if (totalADC>16777215) totalADC=16777215; //16777215=256*256*256=24 bits.
  hienResult->header=(nTow<<24)+totalADC; 

  //  printf("L2-%s-compute: saved: size%d val[0]=0x%x p=%p\n",getName(),hiTwEve->size,hiTwEve->value[0],hiTwEve->value); 

  hiTwEve->isFresh=L2hienList09::kDataFresh;
  hA[2]->fill(hiTwEve->size);
} 


/* ========================================
  ======================================== */
bool 
L2hienAlgo09::decisionUser(int token, int *myL2Result){
  // INPUT: token + comput() results stored internally
  // OUTPUT: always YES & put first ten towers above threshold into myL2Result.

  //don't unless you have something useful to say, and then talk to Ross first:  (*myL2Result)=0;// empty

  // get pointers to internal private event storage
  L2hienList09 *hiTwEve=mHiEnTw+token;
  L2hienResult2009 *hienResult=mHienResult+token;
  //printf("L2-%s-decision: ---EMC ADC list---token=%d size=%d val[0]=0x%x p=%p\n",getName(),token,hiTwEve->size,hiTwEve->value[0],hiTwEve->value);

  //...... some histos for QA 

  hA[3]->fill(hiTwEve->size);
  if(hiTwEve->size>= par_maxList) mhN->fill(5); // was overflow
  if(hiTwEve->isFresh>L2hienList09::kDataFresh) mhN->fill(6); // stale data
  hiTwEve->isFresh++; // mark local data as stale 

  // scan for very hot towers
  int adc4QaThres=(par_adcThres/8); // it is 2x higher than minThres
  int * mTowerID2etaBin=0 ,  *mTowerID2phiBin=0;
  if(mSwitch==kIsBtow) { //...... map pointers to Barrel
    mTowerID2etaBin=mTowerID2etaBin_B;
    mTowerID2phiBin=mTowerID2phiBin_B;
  } else { //...... map pointers to Endcap
    mTowerID2etaBin=mTowerID2etaBin_E;
    mTowerID2phiBin=mTowerID2phiBin_E;
  }
  
  unsigned int *value=hiTwEve->value;
  int ic;
  int adcSum4=0;
  for(ic=0;ic<hiTwEve->size;ic++,value++) {
    int adc4=(*value)>>(16+4); // reduced resolution
    int softID=(*value)&0xffff;
    adcSum4+=adc4;
    //printf(" got adc=%d softID=%d, ",adc,softID);
    hA[4]->fill(adc4);
    if(adc4<adc4QaThres) continue;
    hA[5]->fill(mTowerID2etaBin[softID]);
    hA[6]->fill(mTowerID2phiBin[softID]);
  }
  if(adcSum4>0) hA[7]->fill(adcSum4);

  // printf("\n");

  
  // debugging should be off for any time critical computation
  if(par_dbg>0){
    print2(token);

  } 

  //end of histogramming code



  //set L2Result:
  if(mSwitch==kIsBtow) //only if we're the barrel instance of this, otherwise we'd clobber.
    {
      memcpy(myL2Result,hienResult,sizeof(L2hienResult2009));
      hienResult->header=0; //mark it as used by setting totalADC=nTowers=0;
    }
  /****************/
  /* accept always */
  /****************/
  return true; 

} 


/* ========================================
  ======================================== */
void
L2hienAlgo09::finishRunUser() {  /* called once at the end of the run */
  // do whatever you want, log-file & histo-file are still open
  
  if (mLogFile){ 
    fprintf(mLogFile,"finishRunUser-%s bhla bhla\n",getName());
  }
  
}


//=======================================
//=======================================
void 
L2hienAlgo09::createHisto() {
  memset(hA,0,sizeof(hA));

  hA[2]=new L2Histo(2,"compute: #towers w/ .....energy /event; x: #  towers; y: counts", 35); // title set in initRun
  hA[3]=new L2Histo(3,"decision: #towers w/ .....energy /event; x: #  towers; y: counts", 35); // title set in initRun

  int nEtaBin=BtowGeom::mxEtaBin;
  int nPhiBin=BtowGeom::mxPhiBin;
  if(mSwitch==kIsEtow) {
    nEtaBin=EtowGeom::mxEtaBin;
    nPhiBin=EtowGeom::mxPhiBin; 
  }
  hA[4]=new L2Histo(4,"accepted: towers ET (watch units!) ; x: (ADC-ped)/16", 150);
  hA[5]=new L2Histo(5,"accepted: #towers w/ ..... vs. eta ring",nEtaBin); // title set in initRun
  hA[6]=new L2Histo(6,"accepted: #towers w/ .... vs. phi ring",nPhiBin); // title set in initRun
  hA[7]=new L2Histo(7,"accepted: ADC sum ...",150); // title set in initRun

  // printf("L2-%s::createHisto() done\n",getName());
}

  
/* ========================================
  ======================================== */
void 
L2hienAlgo09::print2(int token){ // full , local ADC array
  if (!(token>0 && token<L2eventStream2009::mxToken))
    {
      printf("L2hienAlgo09::print2 called with token out of bounds!\n");
      return;
    }

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
  $Log: L2hienAlgo09.cxx,v $
  Revision 1.1  2011/09/22 20:44:10  pibero
  *** empty log message ***

  Revision 1.5  2008/02/01 00:16:43  balewski
  add mxListSize to BTOW/ETOW calibration

  Revision 1.4  2008/01/31 00:51:34  balewski
  bug fix

  Revision 1.3  2008/01/30 21:56:43  balewski
  E+B high-enery-filter L2-algo fuly functional

  Revision 1.2  2008/01/30 00:47:23  balewski
  Added L2-Etow-calib

  Revision 1.1  2008/01/29 00:17:13  balewski
  new algo filtering high-energy towers



 
*/


