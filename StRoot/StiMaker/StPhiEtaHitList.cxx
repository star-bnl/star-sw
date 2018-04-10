#include <string.h>
#include <assert.h>
#include <stdio.h>

#include "TH1.h"
#include "TObjArray.h"
#include "StMessMgr.h"
#include "StPhiEtaHitList.h"
#include "StMaker.h"
//________________________________________________________________________________
const Char_t *StPhiEtaHitList::names[3] = {"Active","Fired","Track"};
static Int_t _debug = 0;
void StPhiEtaHitList::SetDebug(Int_t k) {_debug = k;}
Int_t StPhiEtaHitList::Debug() {return _debug;}
Double_t StPhiEtaHitList::W(Double_t energy) {
  Double_t W = 0;
  if (energy >  0.1) W = 1; // 1 ==> 0.1 < energy <=  0.5 Mip	 
  if (energy >  0.5) W = 2; // 2 ==> 0.5 < energy <=  1.0 Hadron 
  if (energy >  1.0) W = 3; // 3 ==> 1.0 < energy <=  4.0 Electon
  if (energy >  4.0) W = 4; // 4 ==> 4.0 < energy <= 10.0 Tower	 
  if (energy > 10.0) W = 5; // 5 ==>10.0 < energy <= 20.0 W	 
  if (energy > 20.0) W = 6; // 6 ==>20.0 < energy <=100.0 Z	 
  if (energy >100.0) W = 7; // 7 ==>       energy > 100.0 ?      
  return W;
}
//________________________________________________________________________________
StPhiEtaHitList::StPhiEtaHitList() : Wmatch(4), Wveto(0.75) {
  memset(beg, 0, end - beg + 1);
}
StPhiEtaHitList::~StPhiEtaHitList() {
  TH2C **histList[3] = {&mActive, &mFired, &mTrack};
  for (Int_t i = 0; i < 3; i++) {
    SafeDelete(*histList[i]);
  }
}
//________________________________________________________________________________
Bool_t StPhiEtaHitList::isMatched(Int_t iBin){ 
  Char_t active = getActive(iBin);
  Char_t fired  = getFired(iBin);
  Char_t track  = getTrack(iBin);
  Bool_t match = kFALSE;
  if (active && fired && track) match = kTRUE;
  return match;
}
//________________________________________________________________________________
Int_t StPhiEtaHitList::addTrack(Float_t eta, Float_t phi) {
  if (phi < phiMin) phi += 2*TMath::Pi(); 
  Int_t iBin = mTrack->Fill(phi,eta);
  if (Debug()) {
    LOG_INFO << "StPhiEtaHitList::addTrack add track at iBin = " << iBin << " with phi = " << phi << " eta = " << eta << endm;
  }
  return iBin;
}
//________________________________________________________________________________
Bool_t StPhiEtaHitList::isVetoed(Int_t iBin) { 
  Char_t active = getActive(iBin);
  Char_t fired  = getFired(iBin);
  Char_t track  = getTrack(iBin);
  Bool_t veto = kFALSE;
  if (active && ! fired && track) veto = kTRUE;
  return veto;
}
//________________________________________________________________________________
#include "StDetectorDbMaker/St_tofStatusC.h"
#include "StBTofCollection.h"
StBtofHitList *StBtofHitList::fgInstance = 0;
//________________________________________________________________________________
StBtofHitList::StBtofHitList() : StPhiEtaHitList() { 
  fgInstance = this;
  Int_t    nPhi   = mxHalfTray*mxCell;
  phiMin = 0.5;
  Double_t phiMax = phiMin + nPhi;
  Int_t    nEta   = 2*mxModule;
  Double_t etaMin =  - mxModule;
  Double_t etaMax =  + mxModule;
  TH2C **histList[3] = {&mActive, &mFired, &mTrack};
  for (Int_t i = 0; i < 3; i++) {
    *histList[i] = new TH2C(Form("%sBToF",names[i]),Form("List of %s cells in BToF",names[i]),nPhi,phiMin,phiMax,nEta,etaMin,etaMax);
    (*histList[i])->SetDirectory(0);
    (*histList[i])->SetMarkerColor(i+1);
  }
}
//________________________________________________________________________________
StBtofHitList::~StBtofHitList() {fgInstance = 0;}
//________________________________________________________________________________
void StBtofHitList::initRun() {
  LOG_INFO <<Form("StBtofHitList::initRun() start")<<endm;
  if (Debug() > 1) {
    St_tofStatusC::instance()->Table()->Print(0,1);
  }
  Int_t nB=0; Int_t nA=0;
  for(Int_t tray = 1; tray <= mxTray;tray++) 
    for(Int_t module = 1; module <= mxModule;module++) 
      for(Int_t cell = 1; cell <= mxCell;cell++) {
        nB++;
	if (Debug()) {
	  LOG_INFO << "StBtofHitList::initRun() tray = " << tray
		   << " module " << module 
		   << " cell "   << cell 
		   << " status " << St_tofStatusC::instance()->status(tray,module,cell) << endm;
	}
	if (St_tofStatusC::instance()->status(tray,module,cell) != 1) continue;
	nA++;
	mActive->Fill(Phi(tray,module,cell),Eta(tray,module,cell));
      }
  LOG_INFO <<" StBtofHitList::initRun() done,  active="<<nA<<" of "<<nB<<" BTOF channels" <<endm;
}
//________________________________________________________________________________
void StBtofHitList::build ( StBTofCollection *btofColl){
  if(!btofColl || !btofColl->hitsPresent()) {
    LOG_INFO << " No available BTOF hits for this event ... " << endm;
    return;
  }
  StSPtrVecBTofHit& tofHits = btofColl->tofHits();
  for(size_t i=0;i<tofHits.size();i++) { //loop on hits in modules
    StBTofHit *aHit = tofHits[i];
    if(!aHit) continue;
    Int_t t = aHit->tray();
    Int_t m = aHit->module();
    Int_t c = aHit->cell();
    Double_t phi = Phi(t,m,c);
    Double_t eta = Eta(t,m,c);
    Int_t iBin = mFired->Fill(phi,eta);
    if (StPhiEtaHitList::Debug()) {
      LOG_INFO << "StBtofHitList::build add fired at iBin = " << iBin << " with phi = " << phi << " eta = " << eta << endm;
    }
  }
}
//________________________________________________________________________________
Int_t StBtofHitList::addBtofTrack(Int_t t, Int_t m, Int_t c) {
  Double_t phi = Phi(t,m,c);
  Double_t eta = Eta(t,m,c);
  Int_t iBin = mTrack->Fill(phi,eta);
  if (StPhiEtaHitList::Debug()) {
    LOG_INFO << "StBtofHitList::addBtofTrack add track at iBin = " << iBin << " with phi = " << phi << " eta = " << eta 
	     << " tray = " << t << " module = " << m << " cell = " << c
	     << endm;
  }
  return iBin;
}
//________________________________________________________________________________
Bool_t StBtofHitList::isMatched(IntVec ibinVec) {
  Bool_t match = kFALSE;
  for(size_t i=0;i<ibinVec.size();i++) {
    Int_t iBin = ibinVec[i];
    match |= isMatched(iBin);
  }
  return match;
}
//________________________________________________________________________________
Bool_t StBtofHitList::isVetoed(IntVec ibinVec) {
  Int_t nA = 0;
  Bool_t veto = kTRUE;
  for(size_t i=0;i<ibinVec.size();i++) {
    Int_t iBin = ibinVec[i];
    if(getActive(iBin) > 0) {
      nA++;
      if(getTrack(iBin) >0 && getFired(iBin) == 0) veto &= kTRUE;
      else veto &= kFALSE;
    }
  }
  if(nA==0) return kFALSE;
  else return veto;
}
//________________________________________________________________________________
Float_t StBtofHitList::getWeight(IntVec ibinVec) {
  const Float_t Wdunno=1;
  Int_t nA = 0;
  for(size_t i=0;i<ibinVec.size();i++) {
    Int_t iBin = ibinVec[i];
    if(getActive(iBin) > 0) nA++;
  }  
  if(nA==0) return Wdunno;
  if(isMatched(ibinVec)) return Wmatch;
  if(isVetoed(ibinVec)) return Wveto;
  return Wdunno;
}
#include <tables/St_g2t_ctf_hit_Table.h>
#include <StTriggerData.h>
//________________________________________________________________________________
StCtbHitList *StCtbHitList::fgInstance = 0;
//________________________________________________________________________________
StCtbHitList::StCtbHitList() : StPhiEtaHitList(), // CTB clibration: 2 MeV==5 ADC
			   mCtbThres_mev(1),//to reject slats with low dE for M-C data
			   mCtbThres_ch(2) {//to reject slats with low ADC for real data
  fgInstance = this;
  Int_t    nPhi   = 60;
  phiMin = -TMath::Pi()/60.;
  Double_t phiMax =  phiMin + TMath::Pi();
  Int_t    nEta   = 4;
  Double_t etaBins[5] = {-0.97, -0.5, 0.0, 0.5, 0.97};
  TH2C **histList[3] = {&mActive, &mFired, &mTrack};
  for (Int_t i = 0; i < 3; i++) {
    *histList[i] = new TH2C(Form("%sCtb",names[i]),Form("List of %s cells in Ctb",names[i]),nPhi,phiMin,phiMax,nEta,etaBins);
    (*histList[i])->SetDirectory(0);
    (*histList[i])->SetMarkerColor(i+1);
  } 
  mgeantE = new TH2F("geantECtb","geant deposited energy in Ctb", nPhi,phiMin,phiMax,nEta,etaBins);
}
//________________________________________________________________________________
void StCtbHitList::initRun(Float_t fac){
  //  const Float_t  mCtbEtaSeg=0.5,  mCtbPhiSeg=TMath::Pi()/30; // NEVER chang this two , JB
  LOG_INFO <<"  StCtbHitList::initRun(), gain change factor="<<fac<<endm;
  mCtbThres_mev*=fac; // tmp to test cuts
  mCtbThres_ch=(int) (fac*mCtbThres_ch);  // tmp to test cuts
  
  LOG_INFO 
    <<"  StCtbHitList::initRun() CtbThres_Ch (real)="<<mCtbThres_ch
    <<"  or  CtbThres_MeV (M-C)="<<mCtbThres_mev
    <<endm;
  StPhiEtaHitList::initRun();
  // real data events
  Int_t slat, tray;
  for (slat = 0; slat < mxSlat; slat++)
    for ( tray = 0; tray < mxTray; tray++) {
      Float_t phi,eta;
      ctb_get_slat_from_data(slat,tray,phi,eta);
      mActive->Fill(phi,eta);
    }
  
}
//________________________________________________________________________________
void StCtbHitList::buildFromMC(TDataSet *gds) {
  // CTB clibration: 2 MeV==5 ADC
  LOG_INFO <<" StCtbHitList::buildFromMC thr/MeV="<<mCtbThres_mev<<endm;
  if(gds==0) return ;
  // -------------- E X T R A C T    C T B   H I T S   --------------------
  //access the CTB data  from GEANT
  St_g2t_ctf_hit *g2t_ctb_hit = (St_g2t_ctf_hit *) gds->Find("g2t_ctb_hit");
  if(g2t_ctb_hit == 0){
    LOG_INFO << "StCtbHitList::buildMC() No CTB Hits in MC table for this event" << endm;
    LOG_INFO << "g2t_ctb_hit = " << g2t_ctb_hit << endm;
    return ;
  }
  if (g2t_ctb_hit->GetNRows() == 0)    LOG_INFO <<" StCtbHitList::buildMC() Empty geant/ctb data set "<<endm;
  g2t_ctf_hit_st *ctb_hit = g2t_ctb_hit->GetTable();
  //assert(ctb_hit);
  if (! ctb_hit){
    LOG_WARN << "StCtbHitList::buildMC() no CTB hits" << endm;
    return ;
  }
  Int_t i;
  Int_t iBin = -1;
  for (i = 0; i < g2t_ctb_hit->GetNRows(); i++,ctb_hit++){
    Float_t de_mev=ctb_hit->de*1000.;
    //    if(de_mev>0.5)  printf("CTB Hit i=%d  de/MeV=%f parent=%d\n",i,de_mev ,ctb_hit->track_p);
    Int_t iPhi1,iEta1;
    Float_t phi, eta;
    ctb_get_slat_from_geant(ctb_hit->volume_id,iPhi1,iEta1,phi,eta);
    iBin = mgeantE->Fill(phi,eta,de_mev);
  }
  if (iBin > 0) {
    Int_t nx = mActive->GetNbinsX();
    Int_t ny = mActive->GetNbinsY();
    for (Int_t ix = 1; ix <= nx; ix++)
      for (Int_t iy = 1; iy <= ny; iy++) {
	Int_t iBin = mActive->GetBin(ix,iy);
	if(mgeantE->GetBinContent(iBin) < mCtbThres_mev)  continue; // ignore low energy CTB slat
	mFired->AddBinContent(iBin);
      }
  }
}
//________________________________________________________________________________
void  StCtbHitList::ctb_get_slat_from_geant(Int_t volume, Int_t &i_phi, Int_t &i_eta, Float_t & phi, Float_t &eta) {
//  volume_id = 1000*numbv(1)+100*numbv(3)+numbv(2) 
//                   [1-2]        [1-2]    [1-60]
    long i1 ;
    i1     = volume/100; // 10*numbv(1)+numbv(3)
    i_phi  = volume%100; //    numbv(2) 
    if ( i1 < 20 ) {
       i_phi = 14 - i_phi ;
       if ( i_phi < 1 ) i_phi = i_phi + 60 ;
       if ( i1 == 11 ) i_eta = 3 ;
         else 
       if ( i1 == 12 ) i_eta = 4 ;
    }
    else if ( i1 > 20 ) {
       i_phi = i_phi - 42 ;
       if ( i_phi < 1 ) i_phi = i_phi + 60 ;
       if ( i1 == 21 ) i_eta = 2 ;
          else 
       if ( i1 == 22 ) i_eta = 1 ;
    }
    Float_t phiD = 6*i_phi;
    if (phiD <   0) phiD += 360;
    if (phiD > 360) phiD -= 360;
    phi = TMath::DegToRad()*phiD;
    eta = -0.75 + 0.5*(i_eta-1);
}
//________________________________________________________________________________
void  StCtbHitList::buildFromData(StTriggerData *trgD){
  
  LOG_INFO << " StCtbHitList::buildFromData CtbThres_Ch thres="<<mCtbThres_ch << endm;
  
  // access CTB from Akio's Maker
  
  if(!trgD){
    LOG_WARN << "StCtbHitList::buildFromData: no trigData in real data" << endm;
    return ;
  }
  Int_t slat, tray;
  Float_t phi, eta;
  for ( slat = 0; slat < mxSlat; slat++)
    for ( tray = 0; tray < mxTray; tray++) {
      Float_t adc = trgD->ctbTraySlat(tray,slat,0);
      if(adc<mCtbThres_ch) continue;
      ctb_get_slat_from_data(slat, tray, phi, eta);
      Int_t iBin = mFired->Fill(phi,eta);
      if (StPhiEtaHitList::Debug()) {
	LOG_INFO << "StCtbHitList::buildFromData add fired at iBin = " << iBin << " with phi = " << phi << " eta = " << eta << endm;
      }
    }
}
//________________________________________________________________________________
void  StCtbHitList::ctb_get_slat_from_data(Int_t slat, Int_t tray, Float_t & phiRad, Float_t &eta) {
  Float_t phiZero1 = 72 ; // magic lines from Pablo & Herb
  Float_t phiZero2 = 108 ;
  Float_t deltaPhi = 6 ;
  // tray=0,1
  // slat=0,...,119
  
  Int_t iz ;
  Float_t phi ;
  
  if ( tray < 60 )  {
    phi = phiZero1 - tray * deltaPhi ;
    iz = 0 ;
  }  else {
    phi = phiZero2 + (tray-60) * deltaPhi ;
    iz = 1 ;
  }
  if ( phi <   0. ) phi += 360 ;
  if ( phi > 360. ) phi -= 360 ;
  
  phiRad=phi/180*TMath::Pi();
  eta=(1-2*iz)*(1+2*slat)*0.25;
  // printf("CTB hit: slat=%d, tray=%d,  phiDeg=%f/deg, eta=%f\n",slat,tray,phi,eta);
  
}
#include "StEmcRawMaker/defines.h" 
#include "StEmcDetector.h"
#include "StEmcModule.h"
#include "StEmcRawHit.h"
//Rxy = 222, 242, 262 cm
//________________________________________________________________________________
StBemcHitList *StBemcHitList::fgInstance = 0;
//________________________________________________________________________________
StBemcHitList::StBemcHitList() : StPhiEtaHitList(), kSigPed(5.0) {
  fgInstance = this;
  myTable = new StBemcTables();
  geomB = StEmcGeom::instance("bemc");
  Int_t    nPhi   = 120;
  phiMin = 0;
  Double_t phiMax = phiMin + 2*TMath::Pi();
  Int_t    nEta   = 40;
  Double_t etaMin = -1;
  Double_t etaMax =  1;
  TH2C **histList[3] = {&mActive, &mFired, &mTrack};
  for (Int_t i = 0; i < 3; i++) {
    *histList[i] = new TH2C(Form("%sBemc",names[i]),Form("List of %s cells in Bemc",names[i]),nPhi,phiMin,phiMax,nEta,etaMin,etaMax);
    (*histList[i])->SetDirectory(0);
    (*histList[i])->SetMarkerColor(i+1);
  }
}
//________________________________________________________________________________
void StBemcHitList::initRun() {
  LOG_INFO <<Form("StBemcHitList::initRun() start")<<endm;
  StPhiEtaHitList::initRun();
  // .. grab table
  StMaker*mk = StMaker::GetChain()->GetMaker("db");
  assert(mk);
  myTable->loadTables(mk  );
  
  Int_t nB=0,nA=0;
  Int_t id;
  for(id=1; id<=BTOWSIZE; id++) {
    
    //........... querry BTOW DB/geom
    Int_t  status;
    myTable->getStatus(BTOW, id, status);
    Int_t m,e,s;
    geomB->getBin(id,m,e,s);
    Float_t eta=0,phi=0;
    geomB->getEta(m,e,eta);
    geomB->getPhi(m,s,phi);  // -pi <= phi < pi
    if( phi < phiMin) phi+=2*TMath::Pi(); // I want phi in [0,2Pi]
    nB++;
    if( status!= 1)  continue; // drop broken towers
    mActive->Fill(phi,eta);
    nA++;
  }
  LOG_INFO <<" StBemcHitList::initRun() done,  active="<<nA<<" of "<<nB<<" BTOW towers" <<endm;
}
//________________________________________________________________________________
void StBemcHitList::build ( StEmcDetector*det, Float_t adcMin){
  for(Int_t m = 1; m <= BEMCMODULES;m++) { //loop on modules...
    StEmcModule* module = det->module(m);
    assert(module);
    StSPtrVecEmcRawHit& rawHit=module->hits();
    for(UInt_t k=0;k<rawHit.size();k++) { //loop on hits in modules
      if (rawHit[k]->energy() <= 0) continue;
      // check geometry
      Int_t m=rawHit[k]->module();
      Int_t e=rawHit[k]->eta();
      Int_t s=abs(rawHit[k]->sub());
      Float_t eta=0,phi=0;
      geomB->getEta(m,e,eta);
      geomB->getPhi(m,s,phi);  // -pi <= phi < pi
      if( phi < phiMin                ) phi += 2*TMath::Pi(); // I want phi in [0,2Pi]
      if (phi > phiMin + 2*TMath::Pi()) phi -= 2*TMath::Pi(); 
#if 0
      Int_t id;
      geomB->getId(m,e,s,id); // to get the software id    
      Float_t ped,sig,calib;
      myTable->getPedestal(BTOW, id, 0, ped,sig); 
      Float_t rawAdc = rawHit[k]->adc();
      if( rawAdc<ped+ kSigPed*sig ) continue;
      Float_t adc = rawAdc -ped;
      if(adc< adcMin) continue;
      myTable->getCalib(BTOW, id, 1, calib);
      Float_t energy = calib * (rawAdc - ped);  
      Float_t energy = rawHit[k].energy();
      Double_t W = 1;
      if (energy >  0.3) W = 2; // MIP
      if (energy >  1.0) W = 3; // Above  
      if (energy >  4.0) W = 4; // lowest HT threshold (4 GeV for Run7)
      if (energy > 30.0) W = 5; //
#endif
      Double_t w = W(rawHit[k]->energy());
      if (w > 0) {
	Int_t iBin = mFired->Fill(phi,eta,w);
	if (StPhiEtaHitList::Debug()) {
	  LOG_INFO << "StBemcHitList::build add fired at iBin = " << iBin << " with phi = " << phi << " eta = " << eta 
		   << " energy " << rawHit[k]->energy() << " with W = " << w 
		   << endm;
	}
      }
    }
  }
  
};
#include "StEEmcUtil/database/StEEmcDb.h"
#include "StEEmcUtil/database/EEmcDbItem.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"


#include "StEmcDetector.h"
#include "StEmcModule.h"
#include "StEmcRawHit.h"


//Z=270, 288,306 cm 
//________________________________________________________________________________
StEemcHitList *StEemcHitList::fgInstance = 0;
//________________________________________________________________________________
StEemcHitList::StEemcHitList(StEEmcDb* x, UInt_t y, EEmcGeomSimple *z) :
  StPhiEtaHitList(), eeDb(x), geomE(z), killStatEEmc(y) {
  assert(eeDb);
  assert(geomE);
  fgInstance = this;
  Float_t  kSigPed=5.0; 
  eeDb-> setThreshold( kSigPed);
  Int_t    nPhi   = 60;
  phiMin = -TMath::Pi()/60.;
  Double_t phiMax =  phiMin + 2*TMath::Pi();
  // the first 13 entries mark the bounds of the 12 eta Bins. 
  const Float_t *etaHL= geomE->getEtaBinRangeArray();
  Int_t nEta = 12;
  Double_t etaBins[13];
  for (Int_t i = 0; i < 13; i++) {
    etaBins[i] =  etaHL[12-i];
  }
  TH2C **histList[3] = {&mActive, &mFired, &mTrack};
  for (Int_t i = 0; i < 3; i++) {
    *histList[i] = new TH2C(Form("%sEemc",names[i]),Form("List of %s cells in Eemc",names[i]),nPhi,phiMin,phiMax,nEta,etaBins);
    (*histList[i])->SetDirectory(0);
    (*histList[i])->SetMarkerColor(i+1);
  } 
  LOG_INFO     <<"  StEemcHitList::use kSigPed="<<kSigPed    <<endm;
}
//________________________________________________________________________________
void StEemcHitList::initRun(){
  LOG_INFO <<" StEemcHitList::initRun()"<<endm;
  StPhiEtaHitList::initRun();
  
  // clear old lookup table
  Int_t sec,sub,etaB;
  // fill in new association
  Int_t nB=0,nA=0;
  for(sec=1;sec<=MaxSectors;sec++)
    for(sub='A';sub<='E';sub++)
      for(etaB=1;etaB<=MaxEtaBins;etaB++) {
	//Db ranges: sec=1-12,sub=A-E,eta=1-12; slow method
	const EEmcDbItem *x=eeDb->getT(sec,sub,etaB);
	if(x==0) continue;
	nB++;
	if(x->fail ) continue;  // drop broken channels
	if(x->stat &  killStatEEmc) continue; // other problems
	
	Int_t ieta=x->eta-1; // typical endcap bin numbering
	Int_t isec=x->sec-1, isub=x->sub-'A';
	Float_t eta=geomE->getEtaMean(ieta);  
	Float_t phi=geomE->getPhiMean(isec,isub); //   // -pi <= phi < pi
	if( phi < phiMin) phi+=2*TMath::Pi(); // I want phi in [0,2Pi]
	mActive->Fill(phi,eta);
	nA++;
      }
  LOG_INFO <<" StEemcHitList::initRun() done,  active="<<nA<<" of "<<nB<<" ETOW  towers" <<endm; 
}
//________________________________________________________________________________
void StEemcHitList::build ( StEmcDetector*det, Float_t adcMin){
  for(UInt_t mod=1;mod<=det->numberOfModules();mod++) {
    StEmcModule*     module=det->module(mod);
    //printf("ETOW sector=%d nHit=%d\n",mod,module->numberOfHits());
    StSPtrVecEmcRawHit&     rawHit=  module->hits();
    Int_t sec=mod; // range 1-12
    for(UInt_t k=0;k<rawHit.size();k++){ // over cesctors
      if (rawHit[k]->energy() <= 0) continue;
      StEmcRawHit *h=rawHit[k];
      Char_t sub='A'+h->sub()-1;// range A-E
      Int_t eta=h->eta();// range 1-12
      //Db ranges: sec=1-12,sub=A-E,eta=1-12; slow method
      const EEmcDbItem *x=eeDb->getT(sec,sub,eta);
      if(! x) continue;
      if(x->fail ) continue;  // drop broken channels
      if(x->stat &  killStatEEmc) continue; // other problems
      // functioning chan
      Int_t ieta=x->eta-1; // typical endcap bin numbering
      Int_t isec=x->sec-1;
      Int_t isub=x->sub-'A';
      Float_t Phi=geomE->getPhiMean(isec,isub); //   // -pi <= phi < pi
      Float_t Eta=geomE->getEtaMean(ieta);  
#if 0
      Int_t rawAdc=h->adc();
      if(rawAdc< x->thr) continue; // still only ped
      Float_t adc=rawAdc - x->ped ;
      if(adc < adcMin) continue; // too low response for MIP
      // printf("add Eemc hit %s adc=%.1f\n",x->name,adc);
#endif
      Double_t w = W(rawHit[k]->energy());
      if (w > 0) {
	Int_t iBin = mFired->Fill(Phi,Eta,w);
	if (StPhiEtaHitList::Debug()) {
	  LOG_INFO << "StEemcHitList::build add fired at iBin = " << iBin << " with phi = " << Phi << " eta = " << Eta 
		   << " energy " << rawHit[k]->energy() << " with W = " << w 
		   << endm;
	}
      }
    }
  }
}

