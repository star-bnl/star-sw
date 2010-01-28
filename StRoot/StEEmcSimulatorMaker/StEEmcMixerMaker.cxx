#include <stdlib.h>
#include <string.h>
#include <TFile.h>

#include <Stiostream.h>
#include <StMessMgr.h>  
#include <StEventTypes.h>
#include <StEvent.h>

#include "StEEmcSimulatorMaker/StEEmcFastMaker.h"
#include "StEEmcSimulatorMaker/StEEmcSlowMaker.h"
#include "StEEmcMixerMaker.h"
#include "StEEmcUtil/database/EEmcDbItem.h"
#include "StEEmcUtil/database/StEEmcDb.h"
#include "StEEmcUtil/database/cstructs/eemcConstDB.hh" // def of status bits 
#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"

#include "StDAQMaker/StDAQReader.h"
#include "StEmcRawMaker/StEemcRaw.h"

#include "StEmcRawMaker/StEEmcPrint.h"


ClassImp(StEEmcMixerMaker)

//-------------------------------------------------------------------
//-------------------------------------------------------------------
StEEmcMixerMaker::StEEmcMixerMaker(const char *name):StMaker(name){
    panicOff=false; // once activated disables Endcap embedding
    mEEDb = 0;
}

//-------------------------------------------------------------------
//-------------------------------------------------------------------
StEEmcMixerMaker::~StEEmcMixerMaker() {

}

//-------------------------------------------------------------------
//-------------------------------------------------------------------
Int_t StEEmcMixerMaker::Finish() { 
  return StMaker::Finish(); 
}

//-------------------------------------------------------------------
//-------------------------------------------------------------------
Int_t 
StEEmcMixerMaker::Init(){
  mEEDb=(StEEmcDb*)GetDataSet("StEEmcDb");
  if( mEEDb==0){
    panicOff=true;
    LOG_FATAL<< "::Init()\n\n Fatal Error - Eemc_DbMaker is not in the chain,\n  panicOff="<<panicOff<<endm;
    return  kStErr;
  }
  assert(!strcmp(mEEDb->ClassName(),"StEEmcDb"));
  return StMaker::Init();
}  

//-------------------------------------------------------------------
//-------------------------------------------------------------------
Int_t 
StEEmcMixerMaker::Make(){
/* Embedding is performed in the level of StEmcCollection.  
   Data input is daq file and MC is from EEmc fast Simulator StEEmcFastMaker. 
*/

  LOG_INFO <<"::Make() start...."<<endm;
  StEvent*  mEvent = (StEvent*)GetInputDS("StEvent");
  if(mEvent==0)  panicOff=true;
  StEmcCollection* ecolA =(StEmcCollection*)mEvent->emcCollection();
  if(ecolA==0)    {
    LOG_WARN<<"::Make(), raw2pixels() no emc collection, skip"<<endm;
    return   kStErr;
  }
  
  if(panicOff) {
       LOG_FATAL<< "::Make()\n\n Fatal Error was encounter earlier, Endcap embedding disabled  panicOff="<<panicOff<<endm;
    return  kStErr;
  }

/*
   Fill data from StEEMCReader to StEmcRawData first, then from raw blocks 
   to StEmcCollection of StEvent using the mehtods copyRawData() and 
   raw2pixels() of StEemcRaw. They are changed from private to pubilc in 
   StEemcRaw for being used legally by StEEmcMixerMaker. Also, raw2pixels 
   is modified to accept three parameters: raw2pixels(bool mixer, 
   StEmcCollection*, StEvent*), allowing input of embedded StEmcCollection. 
   If mixer=false (default) , raw2pixels builds a StEmcCollection of a StEvent 
   from DAQ as event/ecolA is built here. If mix=true, the second input 
   StEEmcCollection* is used to build an embedded StEvent.  
    
*/  

  StEEmcPrint eemcPrint;
  eemcPrint.setMode(15);//bits= 1:Tower, 2:Pre, 4:SmdU, 8:SmdV, 15:all
  
  LOG_DEBUG<<"::Make() -------------- print data: Ecoll-A ---- real backg eve  ---------"<<endm;
  if(Debug()) eemcPrint.print(ecolA);

    
  /* If the second source is the EEMC simulator,
     it owns the StEmcCollection from simulator.
  */
  StEmcCollection *ecolB = 0;
  StEEmcFastMaker *sim = (StEEmcFastMaker*)GetMakerInheritsFrom("StEEmcFastMaker");
  // one can use fast simu as the source even if slow simu is used, since slow simu is set up in the overwrite mode.
  if(!sim)  {
    LOG_WARN<<"::Make() No fast EEmcSimulator found, nothing to embed"<<endm; return  kStWarn; }
  ecolB = sim->GetLocalEmcCollection();
  
  if(!ecolB) {
    LOG_WARN<<"::Make() No second EmcCollection to embed"<<endm;   return kStWarn; }
  
  
  LOG_DEBUG <<"::Make() -------------- print data: Ecoll-B ----- M-C physics probe eve -----------"<<endm;      
  if(Debug()) eemcPrint.print(ecolB); 
  
  
  LOG_DEBUG<<GetName() <<"::Make() -------------- print data: Ecoll-A+B ----- before mrging -----"<<endm;   
  if(Debug()) eemcPrint.printChange(ecolA,ecolB,Form("before merging"));

  if(!mergeADCs(ecolA,ecolB))    return  kStErr;
  
  mMixerEmcCollection = ecolA;
  
  LOG_DEBUG <<"::Make() --------------  print Ecoll-A after mixing ----------------"<<endm;
  if(Debug())   eemcPrint.print(ecolA);
  
  LOG_DEBUG <<"::Make() -------------- print data: Ecoll-A+B ----- after mrging -----"<<endm;   
  if(Debug())    eemcPrint.printChange(ecolA,ecolB,Form("after merging"));
  return kStOK; 
}


//--------------------------------------------------------------------
//--------------------------------------------------------------------
bool
StEEmcMixerMaker::mergeADCs(StEmcCollection*emccolA,StEmcCollection*emccolB){   
/*!
This method adds the ADCs from the the second StEmcCollection 
into the first StEmcCollection in event for all EEMC subdetectors
*/
  LOG_DEBUG  <<"::Make() merging ADCs ..."<<endm;

/* 
 For EEMC, module = sector(1-12). 
 Tower (detector-name eemc): sub(1-5), eta(1-12)  
 Pre/Post (detector-name eprs): sub(1-15), eta(1-12)  
          Pre1 sub(1-5), Pre2: sub(6-10), Post: sub(11-15)
 Smd U & V (detector-name: esmdu and esmdv): sub=1, eta=strip(1-288) 
*/                   

  // output of merging: collA=collA+collB
  for(int det = kEndcapEmcTowerId; det<= kEndcapSmdVStripId; det++){
    // Loop over all four sub detectors
    StDetectorId id = StDetectorId(det);
    StEmcDetector* detectorA=emccolA->detector(id);
    StEmcDetector* detectorB=emccolB->detector(id);
    if(!detectorA) 
      LOG_WARN<<"detectorA not loaded"<<endm;
    if(!detectorB) 
      LOG_WARN<<"detectorB not loaded"<<endm;
    
    if(!detectorA  ||  !detectorB) continue;// nothing to mix for such layer 
    for(int secID=1; secID<=kEEmcNumSectors; secID++){ 
      //    if(secID!=6) continue;//tmp
      StEmcModule* sectorA = detectorA->module(secID);
      StEmcModule* sectorB = detectorB->module(secID);
      StSPtrVecEmcRawHit& rawHitA=sectorA->hits();
      StSPtrVecEmcRawHit& rawHitB=sectorB->hits();
 
      // clone pointers to hits collection B  
      vector<StEmcRawHit*> myHitB;
      for(UInt_t k2=0;k2<rawHitB.size();k2++)  
	myHitB.push_back(rawHitB[k2]);
      vector<StEmcRawHit*>::iterator hitB;
      
      // printf("\nmixIn idet=%d sect=%d Nhit A=%d B=%d\n",det,secID,rawHitA.size(),myHitB.size());

      /*....................
	1) merge hits for pixels present in both collections */
      
      for(UInt_t k1=0;k1<rawHitA.size();k1++)    {
	/* do not bother with differences between BEMC & EEMC
	   numbering scheme, pretend it is barrel */
	uint Bmod=rawHitA[k1]->module();

	if((int)Bmod==!secID) {
	  LOG_FATAL << "::Make()\n\n Fatal Error "<<Bmod<<"= Bmod==!secID ="<<secID<<" StEvent internal consistency failed - corrupted event file, abort"<<endm;
	  panicOff=true;
	  return false;

	}

	  // StEvent internal consistency check
	uint Beta=rawHitA[k1]->eta();
	int  Bsub=rawHitA[k1]->sub();

	// erase old energy for every hit, use ADC2E-maker to get energy back
	rawHitA[k1]->setEnergy(-654.3210); // just in case, make more non-physical 
		
	for( hitB=myHitB.begin() ; hitB< myHitB.end(); hitB++) {
	  if( (*hitB)->module()!=Bmod) continue;
	  if( (*hitB)->eta()   !=Beta) continue;
	  if( (*hitB)->sub()   !=Bsub) continue;
	  // ......... found match to the same element
          int  adc=(*hitB)->adc();

	  myHitB.erase(hitB); // to shrink the list for next search
	  /*  Note, *hitB is now a stray pointer, do NOT use it 
	    for anything. The loop must terminate with 'break',
	    since hitB++ makes no sense any more.
 
	    It is assumed there is only one hit per pixel in 
	    collectionB and it was just found.
	  */
	  hitB=(vector<StEmcRawHit*>::iterator)NULL; // siher ist siher

          const EEmcDbItem *x=mEEDb->StBarrelIndex2Item(det,Bmod,Beta,Bsub);
          if(!x) break;// DB info not avaliable, drop this secondary hit
	  
          /* it is assumed GEANT energy deposit is stored 
	     in secondary collection
	  */
         int   adcAdd=adc;
          if(adcAdd<=0) break;
          int adcSum=rawHitA[k1]->adc() +adcAdd;
	  //printf("D: %d %g %d %d  %s\n",rawHitA[k1]->adc(),deB,adcAdd,adcSum,x->name);
          rawHitA[k1]->setAdc(adcSum);
	  break;
	} // loop overB

      } // loop overA
      // printf("mixMid idet=%d sect=%d Nhit A=%d B=%d\n",det,secID,rawHitA.size(),myHitB.size());
    
 
      /*  2) verify no hits remain in  collection B.
	  some hits may be left for real events:
	  * in 2004 or earlier (due to the code implementation)
	  * in 2005+ if a crate is masked out due to any form of corruption or being OFF
	  */

     if(myHitB.size())
       LOG_WARN<<Form("%s::Make() merging:  %d  hits in  collB  for sect=%d are dropped\n since those channels are not avaliable in collA, \n a create is probably masked out\n",GetName(), myHitB.size(),secID)<<endm;
     // printf("mixEnd idet=%d sect=%d Nhit A=%d B=%d\n",det,secID,rawHitA.size(),myHitB.size());
     
    } // loop over sector
  } // loop over detectors
  
  return true; // all finished w/o problems
}



///////////////////////////////////////////////////////////////////////////
//
// $Id: StEEmcMixerMaker.cxx,v 1.10 2010/01/28 14:16:46 jwebb Exp $
// $Log: StEEmcMixerMaker.cxx,v $
// Revision 1.10  2010/01/28 14:16:46  jwebb
// (1) Use "Form" to get around deprecated conversion from string constant to
//     char *
// (2) eemcPrint called on debug only.
//
// Revision 1.9  2009/11/23 23:44:32  ogrebeny
// At Pibero's request, for the embedding infrastructure.
//
// Revision 1.8  2009/02/05 20:06:52  ogrebeny
// Changed StEEmcDbMaker -> StEEmcDb
//
// Revision 1.7  2007/04/28 17:56:02  perev
// Redundant StChain.h removed
//
// Revision 1.6  2007/03/23 03:26:23  balewski
// Corretions from Victor
//
// Revision 1.5  2007/02/07 02:24:34  balewski
// fix logic error found by Wei-Ming
//
// Revision 1.4  2007/01/24 21:07:03  balewski
// 1) no cout or printf, only new Logger
// 2) EndcapMixer:
//    - no assert()
//    - locks out on first fatal error til the end of the job
//
// Revision 1.3  2006/12/22 15:20:45  balewski
// ignore M-C hits for crates masked for real events, as suggested by Wei-Ming
//
// Revision 1.2  2006/12/13 13:24:38  balewski
// fix wrong header path
//
// Revision 1.1  2006/12/12 20:29:13  balewski
// added hooks for Endcap embedding
//
// Revision 1.1.1.1  2005/05/31 18:53:25  wzhang
// First version
//
//
///////////////////////////////////////////////////////////////////////////
