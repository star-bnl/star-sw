#include <stdlib.h>
#include <string.h>
#include <TFile.h>

#include <Stiostream.h>
#include <StMessMgr.h>  
#include <StEventTypes.h>
#include <StEvent.h>
#include <StChain.h>

#include "StEEmcSimulatorMaker/StEEmcFastMaker.h"
#include "StEEmcSimulatorMaker/StEEmcSlowMaker.h"
#include "StEEmcMixerMaker.h"
#include "StEEmcDbMaker/EEmcDbItem.h"
#include "StEEmcDbMaker/StEEmcDbMaker.h"
#include "StEEmcDbMaker/cstructs/eemcConstDB.hh" // def of status bits 
#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"

#include "StDAQMaker/StDAQReader.h"
#include "StEmcRawMaker/StEemcRaw.h"

#include "StEmcRawMaker/StEEmcPrint.h"


ClassImp(StEEmcMixerMaker)

//-------------------------------------------------------------------
//-------------------------------------------------------------------
StEEmcMixerMaker::StEEmcMixerMaker(const char *name):StMaker(name){
  
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
  mEEDb=(StEEmcDbMaker*)GetMaker("eemcDb");
  if(mEEDb==0) mEEDb=(StEEmcDbMaker*)GetMaker("eeDb"); // try another name
  if( mEEDb==0){
    gMessMgr->Warning()<<GetName()<<
      "::Init()\n\n Fatal Error - Eemc_DbMaker is not in the chain,\n aborting"<<endm;
    //    exit(1);
    return  kStErr;
  }
  return StMaker::Init();
}  

//-------------------------------------------------------------------
//-------------------------------------------------------------------
Int_t 
StEEmcMixerMaker::Make(){
/* Embedding is performed in the level of StEmcCollection.  
   Data input is daq file and MC is from EEmc fast Simulator StEEmcFastMaker. 
*/

  LOG_INFO<<GetName() <<"::Make() "<<endm;
  StEvent*  mEvent = (StEvent*)GetInputDS("StEvent");
  assert(mEvent);
  StEmcCollection* ecolA =(StEmcCollection*)mEvent->emcCollection();
  assert(ecolA);
  if(ecolA==0)
    {
      gMessMgr->Message("","W") << GetName()<<"::raw2pixels() no emc collection, skip"<<endm;
      return   kStErr;
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
  eemcPrint.setMode(15);//bits 1:Tower, 2:Pre, 4:SmdU, 8:SmdV, 15:all
  if(Debug()) {
    gMessMgr->Info() <<GetName() <<"::Make() -------------- print data: Ecoll-A ---- real backg eve  ---------"<<endm;
    eemcPrint.print(ecolA);
  }
  /* If the second source is the EEMC simulator,
     it owns the StEmcCollection from simulator.
  */
  StEmcCollection *ecolB = 0;
  StEEmcFastMaker *sim = (StEEmcFastMaker*)GetMaker("EEmcFastSim");
  // one can use fast simu as the source even if slow simu is used, since slow simu is set up in the overwrite mode.
  if(!sim)  {
    gMessMgr->Warning() <<GetName() <<"::Make() No fast EEmcSimulator found, nothing to embed"<<endm; return  kStWarn; }
  ecolB = sim->GetLocalEmcCollection();

  if(!ecolB) {
    gMessMgr->Warning() <<GetName() <<"::Make() No second EmcCollection to embed"<<endm;   return kStWarn; }
  
  if(Debug()) {
    LOG_INFO <<GetName() <<"::Make() -------------- print data: Ecoll-B ----- M-C physics probe eve -----------"<<endm;      
    eemcPrint.print(ecolB);
  }
 
  LOG_INFO <<GetName() <<"::Make() -------------- print data: Ecoll-A+B ----- before mrging -----"<<endm;   
  eemcPrint.printChange(ecolA,ecolB,"before merging");

  mergeADCs(ecolA,ecolB);

  mMixerEmcCollection = ecolA;
  
  if(Debug()) {
    gMessMgr->Info() <<GetName() <<"::Make() --------------  print Ecoll-A after mixing ----------------"<<endm;
    eemcPrint.print(ecolA);
  }
  
  LOG_INFO <<GetName() <<"::Make() -------------- print data: Ecoll-A+B ----- after mrging -----"<<endm;   
  eemcPrint.printChange(ecolA,ecolB,"after merging");
  return kStOK; 
}


//--------------------------------------------------------------------
//--------------------------------------------------------------------
void
StEEmcMixerMaker::mergeADCs(StEmcCollection*emccolA,StEmcCollection*emccolB){   
/*!
This method adds the ADCs from the the second StEmcCollection 
into the first StEmcCollection in event for all EEMC subdetectors
*/
  gMessMgr->Info()<<GetName() <<"::Make() merging ADCs ..."<<endm;

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
      gMessMgr->Warning()<<"detectorA not loaded"<<endm;
    if(!detectorB) 
      gMessMgr->Warning()<<"detectorB not loaded"<<endm;
    
    if(!detectorA  ||  !detectorB) continue;// nothing to mix for such leyer 
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
	   numbereing scheme, pretend it is barrel */
	uint Bmod=rawHitA[k1]->module();

	if((int)Bmod==!secID) {
	  gMessMgr->Warning()<<GetName()<<
	    "::Make()\n\n Fatal Error "<<Bmod<<"= Bmod==!secID ="<<secID<<" StEvent internal consistency failed - corrupted event file, abort"<<endm;
	  // exit(1);
	  return ;

	}

	  // StEvent internal consistency check
	uint Beta=rawHitA[k1]->eta();
	int  Bsub=rawHitA[k1]->sub();

	// erase old energy for every hit, use ADC2E-maker to get energy back
	rawHitA[k1]->setEnergy(654.3210); // just in case
		
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
    
 
     //  2) copy all remining hits from collection B to A, expected none
  
     if(myHitB.size())
       LOG_ERROR<<GetName() <<"::Make() For real events in 2005+ should be NO leftover hits,\n is="<<myHitB.size()
		<<" StDetId="<<det<<" sect="<<secID
		<<" sth is very Wrong, Jan B. "<<endm;
     
     for( hitB=myHitB.begin() ; hitB< myHitB.end(); hitB++) {
       
       const EEmcDbItem *x=mEEDb->StBarrelIndex2Item(det,(*hitB)->module(),(*hitB)->eta(),(*hitB)->sub());
       if(!x) continue;// DB info not avaliable, drop secondary hit 
	 int adc=(*hitB)->adc();
       if(adc<=0) continue;
       float ene=666.555;// ADC2E-maker to get energy
       StEmcRawHit* hit 
	 =  new StEmcRawHit(id,(*hitB)->module(),(*hitB)->eta(),(*hitB)->sub(),adc,ene);
       detectorA->addHit(hit);
     }
     // printf("mixEnd idet=%d sect=%d Nhit A=%d B=%d\n",det,secID,rawHitA.size(),myHitB.size());
     
    } // loop over sector
  } // loop over detectors
  
}



///////////////////////////////////////////////////////////////////////////
//
// $Id: StEEmcMixerMaker.cxx,v 1.2 2006/12/13 13:24:38 balewski Exp $
// $Log: StEEmcMixerMaker.cxx,v $
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
