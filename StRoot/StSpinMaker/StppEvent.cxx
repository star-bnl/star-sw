//////////////////////////////////////////////////////////////////////
//
// $Id: StppEvent.cxx,v 1.8 2002/06/24 13:22:59 akio Exp $
// $Log: StppEvent.cxx,v $
// Revision 1.8  2002/06/24 13:22:59  akio
// numerous bug fix & updates
//
// Revision 1.7  2002/05/16 21:55:05  akio
// bug fixed for unpol bunch spin bit
//
// Revision 1.6  2002/02/15 14:50:13  jeromel
// Re-enabled changes. StEvent commited as well.
//
// Revision 1.5  2002/02/13 17:15:10  jeromel
// Commented out recent addition from Akio to prevent compilation collapse.
// Need StEvent addition.
//
// Revision 1.4  2002/02/11 20:30:48  akio
// Many updates, including very first version of jet finder.
//
// Revision 1.3  2002/01/24 17:38:33  akio
// add L3 info, zdc info & fix phi/psi confusion
//
// Revision 1.2  2002/01/17 02:06:13  akio
// fixed bug in L3 weighted phi
//
// Revision 1.1  2002/01/16 20:22:53  akio
// First version
//
//
// Revision 1.0  2001/06/14 Akio Ogawa
//
//////////////////////////////////////////////////////////////////////
#include <iostream.h>

//#include "Rtypes.h"
#include "StEventTypes.h"
#include "StEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StTriggerDetectorCollection.h"
#include "StCtbTriggerDetector.h"
#include "StppEvent.h"
#include "StEtGridFlat.h"
#include "StJet.h"

ClassImp(StppEvent);

extern "C" void fpdpi0mass_(int*,float*,float*);

StppEvent::StppEvent(){
  tracks = new TClonesArray ("StMuTrack",200);  //warning hardcoded # of track limits!
  trackChoice = 0; //primary track is default
#ifdef _Jet_
  jets = new TClonesArray ("StJet",20);  //warning hardcoded # of jets limits!
  jetR    = 0.7;
  jetSeed = 1.5;
  jetCut  = 0.2;
#endif 
  infoLevel = 0;
  BunchIdDifference=-999;
  clear();
}

StppEvent::~StppEvent() {
  clear() ;
  tracks->Delete();
  delete tracks ;
#ifdef _Jet_
  jets->Delete();
  delete jets;
#endif 
} 

void StppEvent::clear(){
  runN = 1;
  eventN = 0;
  token = 0;
  triggerWord = 0;
  time = 0;
  bunchId = 0;
  bunchId7bit = 0;
  doubleSpinIndex = 0;

  if(tracks) tracks->Clear();
  nPrimTrack = 0;
  nGoodTrack = 0;
  xVertex = -9999.0;
  yVertex = -9999.0;
  zVertex = -9999.0;
  LCP = 0;
  sumPt = 0.0;
  vectorSumPt = 0.0;
  weightedEta = 0.0;
  weightedPhi = 0.0;

  bbcAdcSumEast = 0;
  bbcAdcSumWest = 0;
  bbcNHitEast=0;
  bbcNHitWest=0;
  zVertexBbc=0.0;

  fpdAdcSumNorth  = 0;
  fpdAdcSumSouth  = 0;
  fpdAdcSumTop    = 0;
  fpdAdcSumBottom = 0;
  fpdAdcSumPres1  = 0;
  fpdAdcSumPres2  = 0;
  fpdAdcSumSmdX   = 0;
  fpdAdcSumSmdY   = 0;
  fpdSouthVeto    = 0;
  fpdPi0Mass      = 0;
  fpdPi0E         = 0;
  fpdPi0Eta       = 0;
  fpdPi0Phi       = 0;
  fpdPi0EShare    = 0;  

  ctbAdcSum = 0.0;
  ctbNHit=0;
  zdcEast = 0;
  zdcWest=0;
  zdcTdcEast = 0;
  zdcTdcWest = 0;
  zdcRatioEast = 0.0;
  zdcRatioWest = 0.0;

  svtNHit=0;
  emcHighTower = 0.0;  
  
#ifdef _Jet_
  nJets = 0;
  jets->Clear();
#endif
}

#ifndef __CINT__
Int_t StppEvent::fill(StEvent *event){

  if(!event && !mudst){
    cout << "StppEvent::fill()   Error:Neither StEvent nor Mudst exist!" << endl;
    return 1;
  }
  
  StMuEvent* muevent;
  if(!event && mudst) {muevent = mudst->event(); }
  
  //event info
  if(event){
    eventN = event->id() ;
    runN   = event->runId(); 
    time   = event->time(); 
  }else{
    eventN = muevent->eventInfo().id() ;
    runN   = muevent->eventInfo().runId(); 
    time   = muevent->eventInfo().time(); 
  }
  
  // Get primary tracks
  nPrimTrack = 0; 
  TClonesArray* mutracks;
  if(mudst){
    switch(trackChoice){
    case 0: mutracks = mudst->primaryTracks(); break;
    case 1: mutracks = mudst->globalTracks(); break;
    case 2: mutracks = mudst->l3Tracks(); break;
    }
    nPrimTrack = mutracks->GetEntries();   
    for(int i=0; i<=mutracks->GetLast(); i++){
      new((*tracks)[i]) StMuTrack((const StMuTrack &) *mutracks->UncheckedAt(i));
    }
  }else if(event){
    //getting tracks from StEvent
    StSPtrVecTrackNode* exnode = 0;
    StTrackType type;
    switch(trackChoice){
    case 0: exnode = &(event->trackNodes()); type=primary; break;
    case 1: exnode = &(event->trackNodes()); type=global;  break;
    case 2:
      StL3Trigger* l3 = event->l3Trigger();
      if(l3) exnode = &(l3->trackNodes());
      type=global;
      break;
    }    
    Int_t nnode=exnode->size();
    for( Int_t in=0; in<nnode; in++ ) {
      UInt_t nprim = (*exnode)[in]->entries(type);
       if(nprim==1){
	 new((*tracks)[nPrimTrack++]) StMuTrack(event, (*exnode)[in]->track(type));
       }
    }
  }

  //fill some track related summary
  nGoodTrack = 0; 
  float maxpt = 0.0;
  float sumPx=0.0;
  float sumPy=0.0;
  sumPt=0.0; weightedEta=0.0; 
  for(int i=0; i<=tracks->GetLast(); i++) {
    StMuTrack *t = (StMuTrack *)(* tracks)[i];
    float pt = t->pt();
    if(t->id() > 0 && t->nHits() > 20 && pt > 0.2 && fabs(t->eta())<1.4){
      if(pt > maxpt){
	maxpt = pt;
	LCP = i;
      }
      sumPt += pt;
      sumPx += pt * cos(t->phi());
      sumPy += pt * sin(t->phi());
      weightedEta += pt * t->eta();
      nGoodTrack++;
    }
  }
  
  if(sumPt>0.0){
    vectorSumPt = sqrt(sumPx*sumPx+sumPy*sumPy);
    weightedPhi = (float)atan2((double)sumPy,(double)sumPx);
    weightedEta/= sumPt; 
  }else{
    vectorSumPt = -9.0; weightedPhi=-9.0; weightedEta=-9.0;
  }
  //create at least 1 track even if there is none
  if(nPrimTrack==0) new((*tracks)[0]) StMuTrack();
  if (infoLevel > 1){
    cout << "StppEvent : number of primary tracks " << nPrimTrack << endl;
  }
  
  // Get primary vertex
  StThreeVectorF vtx(-999.0, -999.0, -999.0);
  if(event){
    StPrimaryVertex* v = event->primaryVertex();
    if(v) vtx = v->position();
  }else{
    vtx = muevent->primaryVertexPosition();
  }  
  xVertex = vtx.x(); yVertex = vtx.y(); zVertex = vtx.z();
  if (infoLevel > 1){
    cout << "StppEvent : primary vertex " 
	 << xVertex << " "  << yVertex << " " << zVertex << endl;
  }

  // Getting detector infos
  StCtbTriggerDetector* ctb = 0;
  StZdcTriggerDetector* zdc = 0;
  StBbcTriggerDetector* bbc = 0;
  StFpdCollection*      fpd = 0;
  StL0Trigger*          l0  = 0;
  if(event) {
    StTriggerDetectorCollection* trg = event->triggerDetectorCollection();
    if(trg){
      ctb = &(trg->ctb());
      zdc = &(trg->zdc());
      bbc = &(trg->bbc());
    }      
    fpd = event->fpdCollection();
    l0 = event->l0Trigger();
  }else{
    ctb = &(muevent->ctbTriggerDetector());
    zdc = &(muevent->zdcTriggerDetector());
    bbc = &(muevent->bbcTriggerDetector());
    fpd = &(muevent->fpdCollection());
    l0  = &(muevent->l0Trigger());
  }

  //CTB
  if(ctb){
    ctbAdcSum = 0.0;
    ctbNHit = 0;
    for(unsigned int i=0; i<ctb->numberOfTrays(); i++){
      for(unsigned int j=0; j<ctb->numberOfSlats(); j++){
	ctbAdcSum += ctb->mips(i,j);
	if(ctb->mips(i,j)>0) ctbNHit++;
      }
    }
  }

  //ZDC
  if(zdc){
    zdcEast = (Int_t)zdc->adc(4);
    zdcWest = (Int_t)zdc->adc(0);
    zdcTdcEast = (Int_t)zdc->adc(8);
    zdcTdcWest = (Int_t)zdc->adc(9);
    if(zdcEast>0) {zdcRatioEast = zdc->adc(7)/zdcEast;} else {zdcRatioEast=0.0;}
    if(zdcWest>0) {zdcRatioWest = zdc->adc(3)/zdcWest;} else {zdcRatioWest=0.0;}
  }

  //BBC
  if(bbc){
    bbcAdcSumEast = bbc->adcSumEast();
    bbcAdcSumWest = bbc->adcSumWest();
    bbcNHitEast = bbc->nHitEast();
    bbcNHitWest = bbc->nHitWest();
    zVertexBbc = bbc->zVertex();
  }

  //FPD
  if(fpd){
    token = fpd->token();
    fpdAdcSumNorth  = fpd->sumAdcNorth();
    fpdAdcSumSouth  = fpd->sumAdcSouth();
    fpdAdcSumTop    = fpd->sumAdcTop();
    fpdAdcSumBottom = fpd->sumAdcBottom();
    fpdAdcSumPres1  = fpd->sumAdcPreShower1();
    fpdAdcSumPres2  = fpd->sumAdcPreShower2();
    fpdAdcSumSmdX   = fpd->sumAdcSmdX();
    fpdAdcSumSmdY   = fpd->sumAdcSmdY();
    fpdSouthVeto    = fpd->southVeto();
  }

  //l0 trigger info
  if(l0){
    token = l0->triggerToken();
    triggerWord = l0->triggerWord();
    bunchId = l0->bunchCrossingId();
    bunchId7bit = l0->bunchCrossingId7bit(runN);
    doubleSpinIndex = l0->spinBits();
    // unpol bunch xing now will get spin index==15
    if(runN >= 3020032 && doubleSpinIndex==0 && bunchId7bit%2==1){doubleSpinIndex=15;}
    // checking bunch ids
    if(BunchIdDifference==-999){
      BunchIdDifference = bunchId - bunchId7bit;
      if(BunchIdDifference<0)  BunchIdDifference+=120;
    }
    int diff = bunchId - bunchId7bit;
    if(diff<0) diff+=120;
    else if (BunchIdDifference != diff){
      cout << "2 bunch Id did not agree "<< bunchId<<" - "<<bunchId7bit<<" = "
	   <<diff<<" != "<<BunchIdDifference<<endl;
    }
    cout << "AKIO-Run#/Token/Unix-Time/BunchID/7Bit/SpinBits:" 
	 << runN << " " << token << " " << time << " " 
	 << bunchId << " " << bunchId7bit << " " << doubleSpinIndex  << endl;
  }

#ifdef _Jet_
  //simple jet finder
  nJets = 0;
  StEtGridFlat *g = new StEtGridFlat;
  g->createKeys();
  for(int i=0; i<=tracks->GetLast(); i++){
    StMuTrack *t = (StMuTrack*)(*tracks)[i];
    if(t->flag()>0 && t->nHits()>20 && t->pt()>0.2 && fabs(t->eta())<1.4) g->add(t);
  }
  //g->sort();
  //g->print();
  StJet* jet;
  while( (jet = g->findJet(jetR,jetSeed,jetCut,nJets)) != 0){
    StJet* jj = new((*jets)[nJets]) StJet(jet);
    delete jet;
    jj->print();
    nJets++;
  }
  //g->sort();
  //g->print();
  delete g;
#endif
  
  //example fpd analysis
  //print out fpd infos
  fpd->dump();
  //print out bbc infos
  bbc->dump();

  //calling fortran pi0 finder from FPD
  float result[10]; 
  int iadc[256];
  unsigned short * adc = fpd->adc();
  for(int i=0; i<256; i++){iadc[i] = (int) adc[i];}
  fpdpi0mass_(iadc, &zVertex, result);
  fpdPi0Mass   = result[0];
  fpdPi0E      = result[1];
  fpdPi0Eta    = result[2];
  fpdPi0Phi    = result[3]; 
  fpdPi0EShare = result[4];
  if(fpdPi0Mass>0){
    for(int i=0; i<5; i++){cout << result[i] << " ";}; cout<< endl;
  }

  //end
  return 0;
}
#endif /*__CINT__*/

Int_t StppEvent::correctedBunchId(){
  if (bunchId7bit>113) return (bunchId7bit-113)/2;
  else                 return (bunchId7bit+7)/2;
}
