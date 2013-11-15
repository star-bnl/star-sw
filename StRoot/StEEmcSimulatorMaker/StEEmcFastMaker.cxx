// *-- Author : J.Balewski, A.Ogawa, P.Zolnierczuk
// 
// $Id: StEEmcFastMaker.cxx,v 1.23 2010/08/05 21:23:45 stevens4 Exp $

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
  mUseFullTower = true; // default value for BFC, for consumption by the EEMC slow simulator
  mUseFullPreShower = false;
  mUseFullSmdu = false;
  mUseFullSmdv = false;
  mevIN= new EEmcMCData;
  meeve= new EEeventDst;

  
  mfixTgain=new float [kEEmcNumEtas];
  for (Int_t i=0;i<kEEmcNumEtas;i++) {
    mfixTgain[i]=getTowerGains()[i];
  }



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
  LOG_INFO<<"::Init() \n"<<  endm; 
  if(mEmcCollectionIsLocal) { // special use
     LOG_INFO<<"::Init() use local EmcCollection\n"<<  endm; 
  }  
   return StMaker::Init();
}

//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
Int_t 
StEEmcFastMaker::Make(){
  
  LOG_INFO <<"::Make() , mEmcCollectionIsLocal="<<mEmcCollectionIsLocal<<endm;
  meeve->clear();
  
  int nh=-1;
  if ( (nh = mevIN->readEventFromChain(this)) >0) {
    LOG_INFO  <<"  RAW geant EEMC hits="<<nh<<endm;
    //tmp  if(Debug())mevIN->print();
  } else {
    LOG_INFO <<"   RAW geant EEMC not seen"<<endm;
    return kStOK;
  }
  
  EEeventDst eeveRaw;    // raw M-C hits 
  
  // generation of TTree
  mevIN->write(&eeveRaw); // Clear & Store RAW EEevent
  //tmp  LOG_DEBUG<< Form("::  raw eeveRaw.print():\n",GetName());eeveRaw.print();}
  
  eeveRaw.sumRawMC(meeve); //sum hits with any detector
  //tmp if(Debug()){  printf("%s::  summed eeve.print():\n",GetName());meeve->print();}

  StEmcCollection *emcColl=0;
  if(mEmcCollectionIsLocal) { // special use
    mLocalStEmcCollection=new StEmcCollection();
    emcColl=mLocalStEmcCollection;
  } else { // standard action
    StEvent *stevent =   (StEvent *) GetInputDS("StEvent");
    assert(stevent); // do sth to provide StEvent first       
    emcColl=stevent->emcCollection();
    if(emcColl==0) {
      emcColl=new StEmcCollection();
      stevent->setEmcCollection(emcColl);
      LOG_WARN<<"::Make() has added a non existing StEmcCollection()"<<endm;
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

  //tmp eevt->print();
  assert(emcC);
  LOG_DEBUG<<  Form("EE2ST got emcCollection\n")<<endm;
  
  for(int det = kEndcapEmcTowerId; det<= kEndcapSmdVStripId; det++){
      
    StDetectorId id = StDetectorId(det);
    StEmcDetector* d = new StEmcDetector(id,mxSector);
    emcC->setDetector(d);
    TClonesArray* tca;    
    LOG_DEBUG<<  Form("EE2ST() copy hits from %d EEMC sectors, det=%d\n",eevt->getNSectors(),det)<<endm;

    for(int isec=0; isec<mxSector; isec++){ // over used sectors
      int secID=isec+1;
      EEsectorDst* EEsec = (EEsectorDst*)eevt->getSec(secID);
      //if(EEsec==0) continue;
      LOG_DEBUG<<  Form("EE2ST() isec=%d sec_add=%p  secID=%d det=%d\n",isec,(void*)EEsec,secID,det)<<endm;

      switch (det){
      case kEndcapEmcTowerId: {//..............................     
	bool hasHit[kEEmcNumSubSectors][kEEmcNumEtas];
	memset(hasHit,0,sizeof(hasHit));
	if (EEsec) {
	  tca = EEsec->getTwHits();      
	  for(int j=0; j<=tca->GetLast(); j++){
	    EEtwHitDst* t = (EEtwHitDst *) tca->At(j);
	    int eta=t->eta();
	    int sub=t->sub()-'A'+1;

	    // FAST SIMU:
	    int adc=(int) (t->energy() * mfixTgain[eta-1]);
	    if(adc<0) adc=0; if (adc> getMaxAdc()) adc=getMaxAdc();

	    StEmcRawHit* h = new StEmcRawHit(id,secID,eta,sub,adc,t->energy());
	    d->addHit(h);
	    hasHit[sub-1][eta-1] = true;
	    //	  printf("yyy secID=%d, id2=%d\n",isec,h->module());
	    LOG_DEBUG<<  Form("Tw   %c  %d  %f  %d \n",t->sub(),t->eta(),t->energy(),adc)<<endm;
	  }
	}
	if (mUseFullTower) {
	  // Fill rest of detector with empty hits (ADC=0, E=0)
	  for (int sub = 1; sub <= kEEmcNumSubSectors; ++sub)
	    for (int eta = 1; eta <= kEEmcNumEtas; ++eta)
	      if (!hasHit[sub-1][eta-1]) d->addHit(new StEmcRawHit(id,secID,eta,sub,0,0));
	}
      } break;

      case kEndcapEmcPreShowerId: {//............................
	bool hasHit[3*kEEmcNumSubSectors][kEEmcNumEtas];
	memset(hasHit,0,sizeof(hasHit));
	if (EEsec) {
	  tca = EEsec->getPre1Hits();      
	  for(int j=0; j<=tca->GetLast(); j++){
	    EEtwHitDst* t = (EEtwHitDst *)(* tca)[j];
	    int eta=t->eta();
	    int sub=t->sub()-'A'+1;
	    int adc= (int) (t->energy()* getPreshowerGain());
	    if(adc<0) adc=0; if (adc> getMaxAdc()) adc=getMaxAdc();

	    StEmcRawHit* h = new StEmcRawHit(id,secID,eta,sub,adc,t->energy());
	    d->addHit(h);
	    hasHit[sub-1][eta-1] = true;
	    LOG_DEBUG<<  Form("Pr1   %c  %d  adc=%d e=%f\n",t->sub(),t->eta(),adc,t->energy())<<endm;
	  }

	  tca = EEsec->getPre2Hits();      
	  for(int j=0; j<=tca->GetLast(); j++){
	    EEtwHitDst* t = (EEtwHitDst *)(* tca)[j];
	    int eta=t->eta();
	    int sub=t->sub()-'A'+5+1;
	    int adc= (int) (t->energy()* getPreshowerGain());
	    if(adc<0) adc=0; if (adc> getMaxAdc()) adc=getMaxAdc();

	    StEmcRawHit* h = new StEmcRawHit(id,secID,eta,sub,adc,t->energy());
	    d->addHit(h);
	    hasHit[sub-1][eta-1] = true;
	    LOG_DEBUG<<  Form("Pr2   %c  %d  %d %f\n",t->sub(),t->eta(),adc,t->energy())<<endm;
	  }

	  tca = EEsec->getPostHits();      
	  for(int j=0; j<=tca->GetLast(); j++){
	    EEtwHitDst* t = (EEtwHitDst *)(* tca)[j];
	    int eta=t->eta();
	    int sub=t->sub()-'A'+10+1;
	    int adc= (int) (t->energy()* getPreshowerGain());
	    if(adc<0) adc=0; if (adc> getMaxAdc()) adc=getMaxAdc();

	    StEmcRawHit* h = new StEmcRawHit(id,secID,eta,sub,adc,t->energy());
	    d->addHit(h);
	    hasHit[sub-1][eta-1] = true;
	    LOG_DEBUG<<  Form ("Post   %c  %d  %d %f\n",t->sub(),t->eta(),adc,t->energy())<<endm;
	  }
	}
	if (mUseFullPreShower) {
	  // Fill rest of detector with empty hits (ADC=0, E=0)
	  for (int sub = 1; sub <= 15; ++sub)
	    for (int eta = 1; eta <= kEEmcNumEtas; ++eta)
	      if (!hasHit[sub-1][eta-1]) d->addHit(new StEmcRawHit(id,secID,eta,sub,0,0));
	}
      } break;

      case kEndcapSmdUStripId: { //............................
	bool hasHit[kEEmcNumStrips];
	memset(hasHit,0,sizeof(hasHit));
	if (EEsec) {
	  tca = EEsec->getSmdUHits(); 
	
	  for(int j=0; j<=tca->GetLast(); j++){
	    EEsmdHitDst* t = (EEsmdHitDst *)(* tca)[j];
	    int eta=t->strip();
	    int sub=1;
	    int adc= (int) (t->energy()* getSmdGain());
	    if(adc<0) adc=0; if (adc> getMaxAdc()) adc=getMaxAdc();

	    StEmcRawHit* h = new StEmcRawHit(id,secID,eta,sub,adc,t->energy());
	    d->addHit(h);
	    hasHit[eta-1] = true;
	    LOG_DEBUG<<  Form("SMDU     %d  %d %f\n",t->strip(),adc,t->energy())<<endm;
	  }
	}
	if (mUseFullSmdu) {
	  // Fill rest of detector with empty hits (ADC=0, E=0)
	  for (int eta = 1; eta <= kEEmcNumStrips; ++eta)
	    if (!hasHit[eta-1]) d->addHit(new StEmcRawHit(id,secID,eta,1,0,0));
	}
      } break;

      case kEndcapSmdVStripId:  {//............................
	bool hasHit[kEEmcNumStrips];
	memset(hasHit,0,sizeof(hasHit));
	if (EEsec) {
	  tca = EEsec->getSmdVHits();
	  for(int j=0; j<=tca->GetLast(); j++){
	    EEsmdHitDst* t = (EEsmdHitDst *)(* tca)[j];
	    int eta=t->strip();
	    //int sub=1;
	    int sub=2;
	    int adc= (int) (t->energy()*getSmdGain());
	    if(adc<0) adc=0; if (adc> getMaxAdc()) adc=getMaxAdc();

	    StEmcRawHit* h = new StEmcRawHit(id,secID,eta,sub,adc,t->energy());
	    LOG_DEBUG<<  Form("SMDV    %d  %d  %f\n",t->strip(),adc,t->energy())<<endm;
	    d->addHit(h);
	    hasHit[eta-1] = true;
	  } 
	}
	if (mUseFullSmdv) {
	  // Fill rest of detector with empty hits (ADC=0, E=0)
	  for (int eta = 1; eta <= kEEmcNumStrips; ++eta)
	    if (!hasHit[eta-1]) d->addHit(new StEmcRawHit(id,secID,eta,2,0,0));
	}
      } break;
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
  // Updated to 4.8% from Ilya's study http://drupal.star.bnl.gov/STAR/node/16426
  return 0.048;
}

Float_t *StEEmcFastMaker::getTowerGains()
{
  // Returns the array of tower gains used by the fast simulator
  // to simulate ADC response of the towers.

  // towers are gain matched to fixed E_T: ADC=4096 --> ET=60 GeV
  const float feta[kEEmcNumEtas]= {1.95,1.855,1.765,1.675,1.59,1.51,1.435,1.365,1.3,1.235,1.17,1.115}; 
  //  static Float_t mygains[kEEmcNumEtas];  
  Float_t *mygains=new Float_t[kEEmcNumEtas]; // small memory leak
  int i;  
  Float_t msamplingFraction = getSamplingFraction();
  Int_t maxEtot=getMaxET();
  Int_t maxAdc=getMaxAdc();
  for (i=0;i<kEEmcNumEtas;i++) {
    mygains[i]=maxAdc/maxEtot/cosh(feta[i])/msamplingFraction;
  }
  return mygains;
}

Float_t StEEmcFastMaker::getSmdGain()
{
  // Returns the (single) constant "gain" used to convert geant
  // energy to ADC response in the SMD.
  return 23000; //(adc=g*de )
}

Float_t StEEmcFastMaker::getPreshowerGain()
{
  // Returns the (single) constant "gain" used to convert geant
  // energy in the Pre- and Postshower detectors.
   return 23000; //(adc=g*de )
}

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

// $Log: StEEmcFastMaker.cxx,v $
// Revision 1.23  2010/08/05 21:23:45  stevens4
// Update sampling fraction to 4.8%
//
// Revision 1.22  2010/07/29 16:12:03  ogrebeny
// Update after the peer review
//
// Revision 1.21  2009/12/09 20:38:00  ogrebeny
// User-switchable function added to always create all hits, even if ADC=0. Requested by Pibero for the trigger simulator.
//
// Revision 1.20  2007/04/28 17:56:01  perev
// Redundant StChain.h removed
//
// Revision 1.19  2007/03/23 03:26:23  balewski
// Corretions from Victor
//
// Revision 1.18  2007/02/16 04:08:43  balewski
// bug fix: adc was not limitted to [0,4095], fixed for all layers
//
// Revision 1.17  2007/01/24 21:07:01  balewski
// 1) no cout or printf, only new Logger
// 2) EndcapMixer:
//    - no assert()
//    - locks out on first fatal error til the end of the job
//
// Revision 1.16  2007/01/12 23:57:13  jwebb
// Calculation of ideal gains moved into static member function getTowerGains()
// to allow slow simulator to access them.
//
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





