// *-- Author : J.Balewski, A.Ogawa, P.Zolnierczuk
// 
// $Id: StEEmcFastMaker.cxx,v 1.15 2005/06/09 20:04:23 balewski Exp $

#include "StChain.h"
#include "St_DataSetIter.h"
#include "StEventTypes.h"

#include "StEEmcFastMaker.h"


#include "StEEmcUtil/EEevent/EEeventDst.h"
#include "StEEmcUtil/EEevent/EEsectorDst.h"
#include "StEEmcUtil/EEevent/EEtwHitDst.h"
#include "StEEmcUtil/EEevent/EEsmdHitDst.h"

#include "StEEmcUtil/EEmcMC/EEmcMCData.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"


ClassImp(StEEmcFastMaker)

//--------------------------------------------
void 
StEEmcFastMaker::Clear(Option_t *) {
  meeve->Clear();  
  if(mEmcCollectionIsLocal) { // special use
    delete mLocalStEmcCollection;
    mLocalStEmcCollection=0;
  }

  StMaker::Clear();
}

//--------------------------------------------
StEEmcFastMaker::StEEmcFastMaker(const char *name):StMaker(name){
  /// Class Constructor.  
  SetEmcCollectionLocal(false);
  mLocalStEmcCollection=0;
  mevIN= new EEmcMCData;
  meeve=new EEeventDst;

  //--
  //-- Define the sampling fraction and set the gains for converting
  //-- geant energy response to ADC response.
  //--
  //-- NOTE: Gains can be changed after the fact by using the 
  //-- StMuEEmcSimuReMaker to "remake" the ADC response of a muDst
  //-- in a chain before any analysis on Monte Carlo is performed.
  //-- 
  //--
  msamplingFraction=0.05; 
  // towers are gain matched to fixed E_T
  maxAdc=4095;
  maxEtot=60;  // in GeV
  const float feta[kEEmcNumEtas]= {1.95,1.855,1.765,1.675,1.59,1.51,1.435,1.365,1.3,1.235,1.17,1.115}; 
  
  int i;
  
  mfixTgain=new float [kEEmcNumEtas];
  for (i=0;i<kEEmcNumEtas;i++) {
    mfixTgain[i]=maxAdc/maxEtot/cosh(feta[i])/msamplingFraction;
  }


  mfixSMDgain=23000;
  mfixPgain=23000;


}

//--------------------------------------------
StEEmcFastMaker::~StEEmcFastMaker(){
 delete  mevIN;
 delete  meeve;
 delete [] mfixTgain;
 if(mEmcCollectionIsLocal){
    mLocalStEmcCollection->Clear();
    delete mLocalStEmcCollection;
 }
}

//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
Int_t 
StEEmcFastMaker::Init(){
  printf("\n\n%s::Init() \n\n",GetName());
  if(mEmcCollectionIsLocal) { // special use
    printf("%s::Init() use local EmcCollection\n",GetName());
  }  
   return StMaker::Init();
}

//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
Int_t 
StEEmcFastMaker::Make(){
  
  gMessMgr->Info()<<GetName() <<"::Make() , mEmcCollectionIsLocal="<<mEmcCollectionIsLocal<<endm;
  meeve->clear();
  
  int nh=-1;
  if ( (nh = mevIN->readEventFromChain(this)) >0) {
    gMessMgr->Info()<<GetName() <<"  RAW geant EEMC hits="<<nh<<endm;
    if(Debug())mevIN->print();
  } else {
    gMessMgr->Info()<<GetName() <<"   RAW geant EEMC not seen"<<endm;
    return kStOK;
  }
  
  EEeventDst eeveRaw;    // raw M-C hits 
  
  // generation of TTree
  mevIN->write(&eeveRaw); // Clear & Store RAW EEevent
  if(Debug()) {printf("%s::  raw eeveRaw.print():\n",GetName());eeveRaw.print();}
  
  eeveRaw.sumRawMC(meeve); //sum hits with any detector
  if(Debug()){  printf("%s::  summed eeve.print():\n",GetName());meeve->print();}

  StEmcCollection *emcColl=0;
  if(mEmcCollectionIsLocal) { // special use
    mLocalStEmcCollection=new StEmcCollection();
    emcColl=mLocalStEmcCollection;
  } else { // standard action
    StEvent *stevent =   (StEvent *) (StEvent *) GetInputDS("StEvent");
    assert(stevent); // do sth to provide StEvent first       
    emcColl=stevent->emcCollection();
    if(emcColl==0) {
      emcColl=new StEmcCollection();
      stevent->setEmcCollection(emcColl);
      gMessMgr->Message("","W") << GetName()<<"::Make() has added a non existing StEmcCollection()"<<endm;
    }
  }  

  mEE2ST(meeve, emcColl);

 return kStOK;
}


//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
void  
StEEmcFastMaker::mEE2ST(EEeventDst* eevt, StEmcCollection* emcC){
  int mxSector = kEEmcNumSectors;

  eevt->print();
  assert(emcC);
  if(Debug())printf("EE2ST got emcCollection\n");
  
  for(int det = kEndcapEmcTowerId; det<= kEndcapSmdVStripId; det++){
      
    StDetectorId id = StDetectorId(det);
    StEmcDetector* d = new StEmcDetector(id,mxSector);
    emcC->setDetector(d);
    TClonesArray* tca;    
    if(Debug()) printf("EE2ST() copy hits from %d EEMC sectors, det=%d\n",eevt->getNSectors(),det);

    for(int isec=0; isec<mxSector; isec++){ // over used sectors
      int secID=isec+1;
      EEsectorDst* EEsec = (EEsectorDst*)eevt->getSec(secID);
      if(EEsec==0) continue;
      if(Debug()) printf("EE2ST() isec=%d sec_add=%p  secID=%d det=%d\n",isec,(void*)EEsec,secID,det);

      switch (det){
      case kEndcapEmcTowerId: //..............................     
	tca = EEsec->getTwHits();      
	for(int j=0; j<=tca->GetLast(); j++){
	  EEtwHitDst* t = (EEtwHitDst *) tca->At(j);
	  int eta=t->eta();
	  int sub=t->sub()-'A'+1;

	  // FAST SIMU:
	  int adc=(int) (t->energy() * mfixTgain[eta-1]);
	  
	  StEmcRawHit* h = new StEmcRawHit(id,secID,eta,sub,adc,t->energy());
	  d->addHit(h);
	  //	  printf("yyy secID=%d, id2=%d\n",isec,h->module());
	   if(Debug()) printf("Tw   %c  %d  %f  %d \n",t->sub(),t->eta(),t->energy(),adc);
	} break;
	
      case kEndcapEmcPreShowerId: {//............................
	tca = EEsec->getPre1Hits();      
	for(int j=0; j<=tca->GetLast(); j++){
	  EEtwHitDst* t = (EEtwHitDst *)(* tca)[j];
	  int eta=t->eta();
	  int sub=t->sub()-'A'+1;
	  int adc= (int) (t->energy()* mfixPgain);
	  StEmcRawHit* h = new StEmcRawHit(id,secID,eta,sub,adc,t->energy());
	  d->addHit(h);
	   if(Debug()) printf("Pr1   %c  %d  adc=%d e=%f\n",t->sub(),t->eta(),adc,t->energy());
	}

	tca = EEsec->getPre2Hits();      
	for(int j=0; j<=tca->GetLast(); j++){
	  EEtwHitDst* t = (EEtwHitDst *)(* tca)[j];
	  int eta=t->eta();
	  int sub=t->sub()-'A'+5+1;
	  int adc= (int) (t->energy()* mfixPgain);
	  StEmcRawHit* h = new StEmcRawHit(id,secID,eta,sub,adc,t->energy());
	  d->addHit(h);
	    if(Debug())printf("Pr2   %c  %d  %d %f\n",t->sub(),t->eta(),adc,t->energy());
	}

	tca = EEsec->getPostHits();      
	for(int j=0; j<=tca->GetLast(); j++){
	  EEtwHitDst* t = (EEtwHitDst *)(* tca)[j];
	  int eta=t->eta();
	  int sub=t->sub()-'A'+10+1;
	  int adc= (int) (t->energy()* mfixPgain);
	  StEmcRawHit* h = new StEmcRawHit(id,secID,eta,sub,adc,t->energy());
	  d->addHit(h);
	   if(Debug()) printf("Post   %c  %d  %d %f\n",t->sub(),t->eta(),adc,t->energy());
	}
      } break;

      case kEndcapSmdUStripId: { //............................
	tca = EEsec->getSmdUHits(); 
	
	for(int j=0; j<=tca->GetLast(); j++){
	  EEsmdHitDst* t = (EEsmdHitDst *)(* tca)[j];
	  int eta=t->strip();
	  int sub=1;
	  int adc= (int) (t->energy()* mfixPgain);
	  StEmcRawHit* h = new StEmcRawHit(id,secID,eta,sub,adc,t->energy());
	  d->addHit(h);
	  if(Debug()) printf("SMDU     %d  %d %f\n",t->strip(),adc,t->energy());
	}
      } break;

      case kEndcapSmdVStripId:  {//............................

	tca = EEsec->getSmdVHits();
	for(int j=0; j<=tca->GetLast(); j++){
	  EEsmdHitDst* t = (EEsmdHitDst *)(* tca)[j];
	  int eta=t->strip();
	  int sub=1;
	  int adc= (int) (t->energy()* mfixSMDgain);
	  StEmcRawHit* h = new StEmcRawHit(id,secID,eta,sub,adc,t->energy());
	  if(Debug())  printf("SMDV    %d  %d  %f\n",t->strip(),adc,t->energy());
	  d->addHit(h);
	} 
      }break;
      default:
	assert(1==2); // the det is out of range, this code gaves up
      }   
    }
  }
}



/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

Float_t StEEmcFastMaker::getSamplingFraction()
{
  // Returns the sampling fraction used by the fast simulator
  // to simulate ADC response of the towers.
  return msamplingFraction;
}

Float_t *StEEmcFastMaker::getTowerGains()
{
  // Returns the array of tower gains used by the fast simulator
  // to simulate ADC response of the towers.
  return mfixTgain;
}

Float_t StEEmcFastMaker::getSmdGain()
{
  // Returns the (single) constant "gain" used to convert geant
  // energy to ADC response in the SMD.
  return mfixSMDgain;
}

Float_t StEEmcFastMaker::getPreshowerGain()
{
  // Returns the (single) constant "gain" used to convert geant
  // energy in the Pre- and Postshower detectors.
  return mfixPgain;
}

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

// $Log: StEEmcFastMaker.cxx,v $
// Revision 1.15  2005/06/09 20:04:23  balewski
// upgrade for embedding
//
// Revision 1.14  2005/06/03 19:20:47  balewski
// *** empty log message ***
//
// Revision 1.13  2004/10/20 22:46:36  balewski
// add emcCollection if not exist
//
// Revision 1.12  2004/05/26 21:28:37  jwebb
// o Changes to StEEmcFastMaker to provide methods to get sampling fraction,
//   gains, etc...
//
// o StMuEEmcSimuMaker is now just a shell of its former self
//
// o Added StMuEEmcSimuReMaker.  This maker takes a muDst as input, and uses
//   the database maker to "massage" the ADC response, to better simulate
//   the calorimeter as installed.  For now, it simply uses the geant
//   energy response, combined with a single sampling fraction and the
//   database gains and pedestals to come up with a new ADC response.
//
// Revision 1.11  2004/04/08 21:33:25  perev
// Leak off
//
// Revision 1.10  2004/04/08 16:28:08  balewski
// *** empty log message ***
//
// Revision 1.9  2004/03/25 18:13:56  balewski
// cleanup
//
// Revision 1.8  2004/03/24 19:37:55  balewski
// be quiet
//
// Revision 1.7  2003/11/12 19:58:31  balewski
// bug for pre2 in StEvent fixed (was either copy of pre1 or garbage)
//
// Revision 1.6  2003/09/11 05:49:17  perev
// ansi corrs
//
// Revision 1.5  2003/02/21 15:31:18  balewski
// do not kill the chain (it is against my will, JB)
//
// Revision 1.4  2003/02/20 05:15:51  balewski
// *** empty log message ***
//
// Revision 1.3  2003/02/18 19:56:03  balewski
// add pedestals
//
// Revision 1.2  2003/02/14 00:04:31  balewski
// remove few printouts
//
// Revision 1.1  2003/01/28 23:12:59  balewski
// star
//





