// *-- Author : J.Balewski, A.Ogawa, P.Zolnierczuk
// 
// $Id: StEEmcFastMaker.cxx,v 1.12 2004/05/26 21:28:37 jwebb Exp $

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

void StEEmcFastMaker::Clear(Option_t *)
{
  meeve->Clear();
  StMaker::Clear();
}	
//--------------------------------------------

StEEmcFastMaker::StEEmcFastMaker(const char *name):StMaker(name)
{
  /// Class Constructor.  

  mlocalStEvent=0;
  mdbg=0;
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
}

//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
Int_t StEEmcFastMaker::Init(){
  printf("\n\n%s::Init() \n\n",GetName());

  // Create tables
  // Create Histograms    
   return StMaker::Init();
}

//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
Int_t StEEmcFastMaker::Make(){
  
  static int first=1;
  printf("%s::Make()\n",GetName());
  meeve->clear();
  
  int nh=-1;
  if ( (nh = mevIN->readEventFromChain(this)) >0) {
    printf("%s  RAW geant EEMC hits =%d \n",GetName(),nh);
    if(first && mdbg>1)mevIN->print();
    first=0;
  } else {
    printf("%s  no geant EEMC hits found\n",GetName());
    return kStOK;
  }

  
  EEeventDst eeveRaw;    // raw M-C hits 
  
  // generation of TTree
  mevIN->write(&eeveRaw); // Clear & Store RAW EEevent
  if(mdbg) {printf("%s::  raw eeveRaw.print():\n",GetName());eeveRaw.print();}
  
  eeveRaw.sumRawMC(meeve); //sum hits with any detector
  if(mdbg){  printf("%s::  summed eeve.print():\n",GetName());meeve->print();}
   
  StEvent *stevent = mlocalStEvent;
  if(stevent==0) {
    //printf("Access full StEvent ...\n");
    stevent =   (StEvent *) (StEvent *) GetInputDS("StEvent");
    assert(stevent); // do sth to provide StEvent first
    // printf("check existence of emcCollection... StEvent=%p\n",stevent);
    assert(stevent->emcCollection()); 
    
  }  

  //  SetDumEE(eeve);

  mEE2ST(meeve, stevent);
  
  if(mdbg>2) { // test copying back 
    EEeventDst eeve2;   // after EE2St and ST2EE
    mST2EE(&eeve2, stevent);  
    printf( "***** before\n");
    meeve->print();
    printf( "***** after\n");
    eeve2.print();
  }

 return kStOK;
}


//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
void  StEEmcFastMaker::SetLocalStEvent(){
  mlocalStEvent = new StEvent;
  StEmcCollection* stemc = new StEmcCollection;
  mlocalStEvent->setEmcCollection(stemc);
}


//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
void  StEEmcFastMaker::mEE2ST(EEeventDst* eevt, StEvent* stevt){
  int mxSector = kEEmcNumSectors;
  assert(stevt); // fix dumm input
  if(mdbg)printf("EE2ST() start %p\n",(void*)stevt);
  eevt->print();

  StEmcCollection* emcC =(StEmcCollection*)stevt->emcCollection();
   if(mdbg)printf("EE2ST got emcCollection\n");
  
  for(int det = kEndcapEmcTowerId; det<= kEndcapSmdVStripId; det++){
      
    StDetectorId id = StDetectorId(det);
    StEmcDetector* d = new StEmcDetector(id,mxSector);
    emcC->setDetector(d);
    TClonesArray* tca;    
    if(mdbg) printf("EE2ST() copy hits from %d EEMC sectors, det=%d\n",eevt->getNSectors(),det);

    for(int isec=0; isec<mxSector; isec++){ // over used sectors
      int secID=isec+1;
      EEsectorDst* EEsec = (EEsectorDst*)eevt->getSec(secID);
      if(EEsec==0) continue;
      if(mdbg) printf("EE2ST() isec=%d sec_add=%p  secID=%d det=%d\n",isec,(void*)EEsec,secID,det);

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
	   if(mdbg) printf("Tw   %c  %d  %f  %d \n",t->sub(),t->eta(),t->energy(),adc);
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
	   if(mdbg) printf("Pr1   %c  %d  adc=%d e=%f\n",t->sub(),t->eta(),adc,t->energy());
	}

	tca = EEsec->getPre2Hits();      
	for(int j=0; j<=tca->GetLast(); j++){
	  EEtwHitDst* t = (EEtwHitDst *)(* tca)[j];
	  int eta=t->eta();
	  int sub=t->sub()-'A'+5+1;
	  int adc= (int) (t->energy()* mfixPgain);
	  StEmcRawHit* h = new StEmcRawHit(id,secID,eta,sub,adc,t->energy());
	  d->addHit(h);
	    if(mdbg)printf("Pr2   %c  %d  %d %f\n",t->sub(),t->eta(),adc,t->energy());
	}

	tca = EEsec->getPostHits();      
	for(int j=0; j<=tca->GetLast(); j++){
	  EEtwHitDst* t = (EEtwHitDst *)(* tca)[j];
	  int eta=t->eta();
	  int sub=t->sub()-'A'+10+1;
	  int adc= (int) (t->energy()* mfixPgain);
	  StEmcRawHit* h = new StEmcRawHit(id,secID,eta,sub,adc,t->energy());
	  d->addHit(h);
	   if(mdbg) printf("Post   %c  %d  %d %f\n",t->sub(),t->eta(),adc,t->energy());
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
	  if(mdbg) printf("SMDU     %d  %d %f\n",t->strip(),adc,t->energy());
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
	  if(mdbg)  printf("SMDV    %d  %d  %f\n",t->strip(),adc,t->energy());
	  d->addHit(h);
	} 
      }break;
      default:
	assert(1==2); // the det is out of range, this code gaves up
      }   
    }
  }
}


//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
void  StEEmcFastMaker::mST2EE(EEeventDst* evt, StEvent* stevt){
  printf("ST2EE: started\n");
  printf("ST2EE: is not matched to EE2ST, fix the code first (J.B.)\n");
  assert(1==2); // not working method, fix it if you need it
  assert(stevt);
  printf("ST2EE:found StEvent\n");
  //  StTpcHitCollection* tpch = (StTpcHitCollection*)stevt->tpcHitCollection();
  StEmcCollection* emcC =(StEmcCollection*)stevt->emcCollection();
  assert(emcC);
  printf("ST2EE:found EmcCollection\n");

  evt->clear();
  for(int det = kEndcapEmcTowerId; det<= kEndcapSmdVStripId; det++){
    printf("ST2EE() det=%d \n",det);

    StDetectorId id = StDetectorId(det);
    StEmcDetector* d = emcC->detector(id);
    if(d==0) {
      printf("ST2EE() Found no detector collection, skipping id=%d\n",id);
      continue;
    }
    printf("ST2EE() det=%d add_d=%p\n",det,(void*)d);

    if(d->numberOfModules() < 1) {
      printf("ST2EE() Found no modules in the detector collection, skipping id=%d\n",id);
      continue;
    }
    for(unsigned int isec=0; isec<d->numberOfModules(); isec++){
      StEmcModule* stmod =  d->module(isec);
      if(stmod==0) { printf("ST2EE() couldn't get sector from StEvent, sector=%d\n",isec); continue;}
      StSPtrVecEmcRawHit & h = stmod->hits();
      if(h.size()>0){
	EEsectorDst* sec = evt->getSec((int)isec);
	if(sec==0) { 
	  printf("ST2EE() couldn't find a sector from EEevent, Adding sector=%d\n",isec);
	  sec = evt->addSectorDst(isec);
	}
	switch (det){
	case kEndcapEmcTowerId:     
	  for(unsigned int j=0; j<h.size() ;j++){
	    printf("Tw  %c %d %f\n",h[j]->sub()+'A', h[j]->eta(),h[j]->energy());
	    sec->addTwHit(h[j]->sub()+'A', h[j]->eta(),h[j]->energy());
	  } break;
	case kEndcapEmcPreShowerId: 
	  for(unsigned int j=0; j<h.size() ;j++){
	  int k = h[j]->sub();
	  printf("Pre %d %d %f\n", k, h[j]->eta(),h[j]->energy());
	  if(k<5)       { sec->addPre1Hit(k+'A',    h[j]->eta(),h[j]->energy()); } 
	  else if(k<10) { sec->addPre2Hit(k-5+'A',  h[j]->eta(),h[j]->energy()); } 
	  else          { sec->addPostHit(k-10+'A', h[j]->eta(),h[j]->energy()); } 
	  } break;
	case kEndcapSmdUStripId:
	  for(unsigned int j=0; j<h.size() ;j++){
	    printf("SmU %d %d %f\n",h[j]->eta(),h[j]->sub(),h[j]->energy());
	    sec->addSmdUHit(h[j]->eta(), h[j]->energy());
	  } break;
	case kEndcapSmdVStripId:
	  for(unsigned int j=0; j<h.size() ;j++){
	    printf("SmV %d %d %f\n",h[j]->eta(),h[j]->sub(),h[j]->energy());
	    sec->addSmdVHit(h[j]->eta(), h[j]->energy());
	  } break;
	default:
	  assert(1==2);

	}   
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





