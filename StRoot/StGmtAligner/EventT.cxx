#include <assert.h>
#include "TH1.h"
#include "TH2.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TF1.h"
#include "StEvent.h"
#include "StPrimaryVertex.h"
#include "StEventInfo.h"
#include "StEventSummary.h"
#include "StTrack.h"
#include "StTrackNode.h"
#include "StPrimaryTrack.h"
#include "StGlobalTrack.h"
#include "StTrackDetectorInfo.h"
#include "StTrackGeometry.h"
#include "StDcaGeometry.h"
#include "TGeoMatrix.h"
#include "StarRoot/THelixTrack.h"
#include "EventT.h"
#include "TrackT.h"
#include "HitT.h"
#include "TKey.h"
#include "TDirectory.h"
#include "TClass.h"
#include "TRVector.h"
#include "TRSymMatrix.h"
#include "StGmtHit.h"
#include "StGmtHitCollection.h"
#include "StGmtCollection.h"
#define __OnlyGlobal__
ClassImp(EventTHeader);
ClassImp(EventT);
ClassImp(TrackT);
ClassImp(HitT);  

TClonesArray *EventT::fgTracks = 0;
TClonesArray *EventT::fgHits = 0;
THashList *EventT::fRotList = 0;


static Int_t _debug = 0;
//______________________________________________________________________________
EventT::EventT() : fIsValid(kFALSE)
{
  // Create an EventT object.
  // When the constructor is invoked for the first time, the class static
  // variable fgTracks is 0 and the TClonesArray fgTracks is created.
  
  if (!fgTracks) fgTracks = new TClonesArray("TrackT", 1000);
  fTracks = fgTracks;
  fNtrack = 0;
  if (!fgHits) fgHits = new TClonesArray("HitT", 1000);
  fHits = fgHits;
  fNhit = 0;
}

//______________________________________________________________________________
EventT::~EventT()
{
  Clear();
  SafeDelete(fRotList);
}

//______________________________________________________________________________
Int_t  EventT::Build(StEvent *pEventT, Double_t pCut) {
  static const Int_t NoFitPointCutForGoodTrackT = 15;
  Clear();
  Int_t iok = 1;
  fIsValid = kFALSE;
  if (! pEventT) return iok;
  StGmtCollection* GmtCollection = pEventT->gmtCollection();
  if (! GmtCollection) { cout << "No GMT Collections" << endl; return iok;}
  StSPtrVecTrackNode& theNodes = pEventT->trackNodes();
  UInt_t nnodes = theNodes.size();
  if (! nnodes) { cout << "No tracks" << endl; return iok;}
  StEventInfo*      info = pEventT->info();
  Int_t ev = 0, run = 0, time = 0;
  if (info) {
    ev   = info->id();
    run  = info->runId();
    time = info->time();
  }
  StEventSummary* summary = pEventT->summary();
  Double32_t field = 0;
  if (summary) field = summary->magneticField();
  SetHeader(ev,run,time,field);
  SetFlag(1);
  Int_t nTotMatch = 0;
  //  Create and Fill the TrackT objects
  for (UInt_t i=0; i<nnodes; i++) {
    StTrackNode *node = theNodes[i];
    if (! node) continue;
    StTrack *Track = node->track(global);
    if (! Track) continue;
    StGlobalTrack *gTrack = (StGlobalTrack *) Track;
    StDcaGeometry *dca = gTrack->dcaGeometry();
    if (! dca) continue;
    StPrimaryTrack *pTrack = (StPrimaryTrack *) node->track(primary);
#ifndef __OnlyGlobal__
    if (! pTrack) continue;
    Track = (StTrack *) pTrack;
#endif
    StThreeVectorD g3 = Track->geometry()->momentum();
    Double_t pT = g3.perp();
    Double_t pMom = g3.mag();
    if (pMom < pCut) continue;
    TrackT *track = AddTrackT();
    Double_t InvpT = 0;
    Double_t TanL = 999999;
    if (TMath::Abs(pT) > 1.e-7) {
      InvpT = Track->geometry()->charge()/pT;
      TanL = g3.z()/pT;
    }
    track->SetInvpT(InvpT);
    track->SetPhi(TMath::ATan2(g3.y(),g3.x()));
    track->SetTanL(TanL);
    static const Double_t EC = 2.9979251E-4;
    Double_t Rho = - EC*InvpT*field;
    track->SetRho(Rho);
    track->SetLength(Track->length());
    StTrackDetectorInfo* dinfo=Track->detectorInfo();
    track->SetNpoint(dinfo->numberOfPoints());
    track->SetNPpoint(Track->numberOfPossiblePoints());
    track->SetN(0);
    StPhysicalHelixD helixO = Track->outerGeometry()->helix();
    Double_t R_tof[2]= {210., 216.};  // inner and outer surfaces
    pair<Double_t,Double_t> shR = helixO.pathLength(0.5*(R_tof[0]+R_tof[1]));
    if (TMath::Abs(shR.first) > 200 && TMath::Abs(shR.second) > 200) continue; 
    Double_t stepR = (shR.first > 0) ? shR.first : shR.second;
    StThreeVectorD xyzR = helixO.at(stepR);
    Double_t phiR = TMath::RadToDeg()*xyzR.phi();
    if (_debug) 
      cout << "\t shR " << shR.first << "\t" << shR.second << "\tstepR " << stepR 
	   << "\txyzR\t" << xyzR << "\tphiR\t" << phiR << endl;
    Int_t NoHitPerTrack = 0;
    THashList *fRotList = RotMatrices();
    if (! fRotList) continue;
    TIter next(fRotList);
    TGeoHMatrix *comb = 0;
    while ((comb = (TGeoHMatrix *) next())) {
      TString combName(comb->GetName());
      if (! combName.BeginsWith("R")) continue;
      Int_t Id;
      sscanf(comb->GetName()+1,"%i",&Id);
      UInt_t module = Id;
      StGmtHitCollection* GmtHitCollection = GmtCollection->getHitCollection(module);
      if (! GmtHitCollection) { cout << "No GMT HitT Collections for mudule " << module << endl; continue;}
      StSPtrVecGmtHit& hitvec = GmtHitCollection->getHitVec();
      UInt_t NoHits = hitvec.size();
      if (! NoHits) continue;
      if (_debug) {
	cout << comb->GetName() << "\tmodule = " << module << endl;
	comb->Print();
      }
      static Double_t dz[2] = {50.00, 2.10};
      static Double_t dx[2] = {50.00, 3.65};
      Int_t k = 0; // gmt
      Double_t *rot = comb->GetRotationMatrix();
      Double_t *tra = comb->GetTranslation();
      const StThreeVectorD unit(1.,0.,0.);
      const StThreeVectorD zero(0.,0.,0.);
      StThreeVectorD normal(rot[2],      rot[5],      rot[8]);
      StThreeVectorD middle(tra);
      if (_debug) cout << "middle:" << middle << "\tnormal:" << normal << endl;
      comb->LocalToMaster(zero.xyz(),middle.xyz());
      comb->LocalToMasterVect(unit.xyz(), normal.xyz());
      if (_debug) cout << "middle:" << middle << "\tnormal:" << normal << endl;
      Double_t phiM = TMath::RadToDeg()*middle.phi();
      if (_debug) cout << "phiR = " << phiR << "\tphiM = " << phiM << endl;
      Double_t dPhi = phiR - phiM; 
      if (dPhi >  360) dPhi -= 360;
      if (dPhi < -360) dPhi += 360;
      if (TMath::Abs(dPhi) > 15) continue;
      if (_debug) cout << "zR = " << xyzR.z() << "\tzM = " << tra[2] << endl;
      if (TMath::Abs(xyzR.z() -  tra[2]) > 20) continue;
      Double_t sh = helixO.pathLength(middle, normal); 
      if (_debug) {
	cout << "StHelix sh " << sh 
	     << "\t shR " << shR.first << "\t" << shR.second
	     << endl;
      }
      StThreeVectorD xyzG = helixO.at(sh); if (_debug) cout << "StHelix xyzG\t" << xyzG << endl;
      StThreeVectorD dR = xyzR - xyzG; if (_debug) cout << "dR\t" << dR << " dist = " << dR.magnitude() << endl;
      if (dR.magnitude() > 50) continue;
      if (sh < -5e2 || sh > 5e2) continue;
      if (_debug) { 
	StThreeVectorD dX = xyzG - helixO.at(0); 
	cout << "Qi: " << Track->geometry()->charge() 
	     << "\tQo: " << Track->outerGeometry()->charge()
	     << "\tdX " << dX << endl;
	cout << *dca << endl;
      }
      Double_t uvPred[3];
      comb->MasterToLocal(xyzG.xyz(),uvPred);
      TRVector xyzL(3,uvPred); if (_debug) cout << "StHelix xyzL\t" << xyzL << endl;
      Double_t dirGPred[3] = {helixO.cx(sh),helixO.cy(sh),helixO.cz(sh)};
      Double_t dxyzL[3];
      comb->MasterToLocalVect(dirGPred,dxyzL);
      Double_t tuvPred[2] = {dxyzL[1]/dxyzL[0], dxyzL[2]/dxyzL[0]};
      if (_debug) cout << "StHelix tU/tV =  " << tuvPred[0] << "\t" << tuvPred[1] << endl; 
      if (TMath::Abs(uvPred[1]) > dx[k] + 1.0) continue;
      if (TMath::Abs(uvPred[2]) > dz[k] + 1.0) continue;
      
      for (UInt_t l = 0; l < NoHits; l++) {
	StHit *hit = hitvec[l];
	if (hit) {
	  //if (hit->flag()>=4) continue;
	  //if (hit->flag()< 0) continu;
	  //	  cout << "hitFlag=" << hit->flag() << endl;
	  
	  HitT *h = AddHitT();
	  h->SetHitLength(sh);
	  h->SetHitLength(stepR);
	  h->SetHitdR(dR.magnitude());
	  h->SetHitFlag(UInt_t(hit->flag()));
	  h->SetUVPred (uvPred[1],uvPred[2]);
	  h->SettUVPred(tuvPred[0],tuvPred[1]);
	  h->SetXyzG(xyzG.xyz());
	  h->SetDirG(dirGPred);
	  SetHitT(h, hit, comb, track);
	  NoHitPerTrack++;
	  h->SetHitPerTrack(NoHitPerTrack);
	  //	    SetHitT(h, hit, comb, track, &TPDeriv);
	}
      }
    }
    nTotMatch += NoHitPerTrack;
  }
  fIsValid = kTRUE;
  if (nTotMatch) iok = 0;
  return iok;
}  
//______________________________________________________________________________
TrackT *EventT::AddTrackT()
{
  // Add a new track to the list of tracks for this event.
  // To avoid calling the very time consuming operator new for each track,
  // the standard but not well know C++ operator "new with placement"
  // is called. If tracks[i] is 0, a new TrackT object will be created
  // otherwise the previous TrackT[i] will be overwritten.
  
  TClonesArray &tracks = *fTracks;
  TrackT *track = new(tracks[fNtrack++]) TrackT();
  //Save reference to last TrackT in the collection of Tracks
  return track;
}
//______________________________________________________________________________
HitT *EventT::AddHitT()
{
  // Add a new hit to the list of hits for this event.
  // To avoid calling the very time consuming operator new for each hit,
  // the standard but not well know C++ operator "new with placement"
  // is called. If hits[i] is 0, a new HitT object will be created
  // otherwise the previous HitT[i] will be overwritten.
  
  TClonesArray &hits = *fHits;
  HitT *hit = new(hits[fNhit++]) HitT();
  //Save reference to last HitT in the collection of Hits
  return hit;
}

//______________________________________________________________________________
void EventT::Clear(Option_t * /*option*/)
{
  fTracks->Clear("C"); //will also call TrackT::Clear
  fHits->Clear("C"); //will also call HitT::Clear
}

//______________________________________________________________________________
void EventT::Reset(Option_t * /*option*/)
{
  // Static function to reset all static objects for this event
  //   fgTracks->Delete(option);
  
  delete fgTracks; fgTracks = 0;
  delete fgHits; fgHits = 0;
}

//______________________________________________________________________________
void EventT::SetHeader(Int_t i, Int_t run, Int_t date, Double32_t field)
{
  fNtrack = 0;
  fNhit = 0;
  fEvtHdr.Set(i, run, date, field);
}
//________________________________________________________________________________
void EventT::Print(Option_t *opt) const {
  cout << "Run/EventT\t" << fEvtHdr.GetRun() << "/" << fEvtHdr.GetEvtNum() << "\tDate " << fEvtHdr.GetDate() 
       << "\tField " << fEvtHdr.GetField() << endl;
  cout << "Total no. tracks " << GetTotalNoTracks() << "\tRecorded tracks " << GetNtrack() 
       << "\tRecorded hits " << GetNhit() << endl;
  TRVector vertex(3,GetVertex());
  TRSymMatrix cov(3,GetCovMatrix());
  cout << "Primary vertex " << vertex << endl;
  cout << "Its cov. matrix " << cov << endl;
  for (UInt_t i = 0; i < GetNtrack(); i++) {cout << i << "\t"; GetTrackT(i)->Print();}
  for (UInt_t i = 0; i < GetNhit(); i++) {cout << i << "\t"; GetHitT(i)->Print();}
  
}
//________________________________________________________________________________
HitT *EventT::SetHitT(HitT *h, StHit *hit, TGeoHMatrix *comb, TrackT *track) {
  UInt_t B = 0, L = 0, l = 0, W = 0, H = 0;
  Int_t rdo = 0;
  h->SetRDO(rdo);
  if (hit->detector() == kGmtId) {
    StGmtHit *ht = (StGmtHit *) hit;
    B = ht->getModule();
    h->SetId(B,L,l,W,H);
    h->SetuvD(ht->getLocalY(), ht->getLocalX());
    h->SetuvDError(ht->getErrorLocalY(), ht->getErrorLocalX());
    h->SetSigma(ht->getSigmaY(), ht->getSigmaX());
    h->SetSigmaError(ht->getErrorSigmaY(), ht->getErrorSigmaX());
    h->SetAdc(ht->getAdcY(), ht->getAdcX());
    h->SetAdcError(ht->getErrorAdcY(), ht->getErrorAdcX());
    h->SetUsedInFit(hit->usedInFit());
  }
  StThreeVectorF position = hit->position();
  Double_t xyzG[3] = {position.x(),position.y(),position.z()};
  h->SetGC(xyzG[0],xyzG[1],xyzG[2]);
  Double_t xyzL[3] = {0,0,0};
  comb->MasterToLocal(xyzG,xyzL);
  //	  if (TMath::Abs(xyzL[2]) > 0.1) continue;
  Double_t uvw[3] = {h->GetU(),h->GetV(),0};
  comb->LocalToMaster(uvw,xyzG);
  h->Set(xyzG,uvw);
  Double_t *rot = comb->GetRotationMatrix();
  h->SetWG(rot[2],rot[5],rot[8]);
  //  Int_t IdH = GetIndexOfHitT(h);
  Int_t IdH = fNhit - 1;
  track->SetHitTId(IdH);
  Double_t invpT = track->GetInvpT();
  if (TMath::Abs(invpT) < 1e-7) invpT = 1e-7;
  h->SetpT(1./invpT);
  h->SetMom(track->GetMomentum());
  h->SetWG(rot[2],rot[5],rot[8]);
  TGeoHMatrix *rotL = (TGeoHMatrix *) RotMatrices()->FindObject(Form("WL%s",comb->GetName()+1));
  Double_t xyzLadder[3] = {0,0,0};
  if (rotL) {
    
    rotL->LocalToMaster(uvw,xyzLadder);
    h->SetL(xyzLadder[0],xyzLadder[1],xyzLadder[2]); 
    Double_t uvwP[3] = {h->GetPredU(),h->GetPredV(),0};
    rotL->LocalToMaster(uvwP,xyzLadder);
    h->SetXyzL(xyzLadder);
  } else {
    
    cout << Form("WL%s",comb->GetName()+1) << " has not been found" << endl;
    h->SetL(xyzLadder[0],xyzLadder[1],xyzLadder[2]);
    h->SetXyzL(xyzLadder);
  }
  return h;
}
//________________________________________________________________________________
void TrackT::Print(Option_t *opt) const {
  cout << "TrackT: InvpT " << fInvpT << "\tTanL " << fTanL 
       << "\tPhi " << fPhi << "\tRho " << fRho 
       << "\tNpoint " << fNpoint << "\tNsp " << fNsp << endl;
  for (UInt_t i = 0; i < fNsp; i++) cout << "\t" << fIdHitT[i];
  cout << endl;
}
//________________________________________________________________________________
void HitT::SetId(Int_t B, Int_t L, Int_t l, Int_t W, Int_t H) {
  barrel = B; layer = L; ladder = l; wafer = W; hybrid = H;
}
//________________________________________________________________________________
void HitT::Print(Option_t *opt) const {
  cout << "HitT: Id " << Id << "\tpT = " << pT << "\tmomentum " << pMom << endl;
  TRVector glob(3,&xG); cout << "Global :" << glob << endl;
  cout << "Local      u/v/w " << u << "/ " << v << "/ " << w << endl;
  cout << "Prediction uP/vP " << uP << "/ " << vP << "\ttuP/tvP " << tuP << "/ " << tvP << endl;
}
//________________________________________________________________________________
void EventT::RestoreListOfRotations() {
  if (fRotList) return;
  if (! gDirectory) return;
  fRotList = new THashList(100,0);
  fRotList->SetOwner();
  TIter nextkey(gDirectory->GetListOfKeys() );
  TKey *key;
  while ((key = (TKey*) nextkey())) {
    TObject *obj = key->ReadObj();
    if ( obj->IsA()->InheritsFrom( "TGeoHMatrix" ) ) {
      fRotList->Add(obj);
    }
  }
}
//________________________________________________________________________________
void TBase::Loop(Int_t Nevents) {  
#if 1
  struct PlotPar_t {
    Char_t *Name;
    Char_t *Title;
    Int_t    nx;
    Int_t    ny;
    Double_t xmin; 
    Double_t xmax; 
    Double_t ymin;
    Double_t ymax;  
  };
  const  PlotPar_t plotUP = // plots for uP
    { "uP","track u", 320, 3, -5.,5., 0.,3. };
  
  const  PlotPar_t plotDu = // plots for u-uP
    { "Du","Du before cut", 250, 3, -2.,2., 0.,3. };
  
  const  PlotPar_t plotDuv = // plots for du & dv
    { "Du","Du cuts", 200, 3, -1.,1., 0.,3. };
  
  TFile *fOut = new TFile(fOutFileName,"recreate");
  TString Name;
  TString Title;
  TString uName;
  TString uTitle;
  enum {NM = 8}; // no. of modules
  //              B 
  TH1F *LocPlots[NM];
  TH1F *  uPlots[NM];
  TH1F *    hpT = new TH1F(   "Pt",   "pt", 200, -2., 2.);
  TH1F *    hpM = new TH1F( "Ptot", "ptot", 200,  0., 5.);
  TH1F * uPAll  = new TH1F("UPall","uPall", plotUP.nx, plotUP.xmin, plotUP.xmax);
  TH1F * uAll   = new TH1F("Uall", "ua",    plotUP.nx, plotUP.xmin, plotUP.xmax);
  TH1F * duB[NM][2];
  TH1F * dvB[NM];
  TH1F * uCuts[NM];
  TH1F * xCuts[NM];
  TH1F * uCut   = new TH1F("Ucut","uc", plotDu.nx, plotDu.xmin, plotDu.xmax);
  TH1F * vCut   = new TH1F("Vcut","vc", 200, -3., 3.);
  TH2F * dMin   = new TH2F("DMin","vumin",100,-0.75,0.75,100,-0.75,0.75); 
  TH1F * vMin   = new TH1F("VMin","vmin", plotDuv.nx, plotDuv.xmin, plotDuv.xmax);
  TH1F * uMin   = new TH1F("UMin","umin", plotDuv.nx, plotDuv.xmin, plotDuv.xmax);
  TH1F * uMinC  = new TH1F("UMinC","umC", plotDuv.nx, plotDuv.xmin, plotDuv.xmax);
  memset(LocPlots,0,NM*sizeof(TH1F *));
  memset(  uPlots,0,NM*sizeof(TH1F *));
  for (int M = 0; M < NM; M++) {// over gmt Modules
    uName  = Form("UModule%i", M+1);
    uTitle = Form("du for M%i", M+1);
    duB[M][0] =  new TH1F(uName, uTitle, plotDuv.nx, plotDuv.xmin, plotDuv.xmax );
    uName  = Form("VModule%i", M+1);
    uTitle = Form("dv for M%i", M+1);
    dvB[M] =  new TH1F(uName, uTitle, plotDuv.nx, plotDuv.xmin, plotDuv.xmax );
    uName  = Form("UModule%iVcut", M+1);
    uTitle = Form("du for M%i after Vcut", M+1);
    duB[M][1] =  new TH1F(uName, uTitle, plotDuv.nx, plotDuv.xmin, plotDuv.xmax );
    Int_t module    = M;
    uName  = plotUP.Name;
    uName += Form("M%i", module);
    uTitle = Form("uP for Module %i", module);
    uPlots[module] = 
      new TH1F(uName, uTitle, plotUP.nx, plotUP.xmin, plotUP.xmax );
    uName  = Form("%sM%i", plotDu.Name, module);
    uTitle = Form("u-uP for M %i", module);
    uCuts[module]= 
      new TH1F(uName, uTitle, plotDu.nx, plotDu.xmin, plotDu.xmax );
    uName  = Form("%sxM%i", plotDu.Name, module);
    uTitle = Form("u-uP corr M %i", module);
    xCuts[module]= 
      new TH1F(uName, uTitle, plotDu.nx, plotDu.xmin, plotDu.xmax );
  }
  Long64_t nentries = fChain->GetEntriesFast();
  if (Nevents > 0 && nentries > Nevents) nentries = Nevents;
  Long64_t nbytes = 0, nb = 0;
  Int_t TreeNo = -1;
  TString currentFile("");
  
  for (Long64_t jentry=0; jentry<nentries;jentry++) {
    Long64_t ientry = LoadTree(jentry);
    if (ientry < 0) break;
    nb = fChain->GetEntry(jentry);   nbytes += nb;
    if (! jentry%1000 || TreeNo != fChain->GetTreeNumber()) {
      cout << "Read event \t" << jentry 
	   << " so far, switch to file " << fChain->GetCurrentFile()->GetName() 
	   << endl;
      cout << " current TreeNo: " << TreeNo
	   <<  " new TreeNo: " << fChain->GetTreeNumber() << endl;
      TreeNo = fChain->GetTreeNumber();
    }
    //     if (VertexZCut > 0 && TMath::Abs(fVertex[2]) > VertexZCut) continue;
    UInt_t Ntrack = fEvent->GetTracks()->GetEntriesFast();
    //     int k_used[100000] = {0};    
    for (UInt_t trk = 0; trk < Ntrack; trk++) {
      TrackT *track = (TrackT *)fEvent->GetTracks()->UncheckedAt(trk);
      if (! track) continue;
      Int_t Npoints = track->GetNpoint();
#if 0
      if (minNoFitPoints > 0 && Npoints%100 < minNoFitPoints) continue;
      if (UseSsd && Npoints < 1000) continue; 
      if (UseSvt && Npoints <  100) continue; 
#endif
      Int_t Nsp = track->GetN();
      double dvmin = 1000.;
      double dumin = 1000.;
      int kmin;
      for (Int_t hit = 0; hit < Nsp; hit++) {
	Int_t k = track->GetHitTId(hit) - 1;
	//	 assert(k>=0);
	HitT *hitT = (HitT *) fEvent->GetHitT(k);
	if ( k < 0) cout <<" k <0:"<<k<<" hit="<<hit<<" Nsp="<<Nsp<< endl;
	if ( k < 0) continue;
	Int_t module  = hitT->Barrel();
	Double32_t u = hitT->GetU();       
	Double32_t v = hitT->GetV();
	Double32_t uP = hitT->GetPredU();       
	Double32_t vP = hitT->GetPredV();
	Double32_t du = u - uP;
	Double32_t dv = v - vP;
#if 0	   
	hpT->Fill(hitT->GetXyzPGl());
	hpM->Fill(hitT->GetpMom());
	if (TMath::Abs(hitT->GetpT()) < 0.2) continue;
#endif
	if ( TMath::Abs(dv) < TMath::Abs(dvmin) ) {dvmin = dv; dumin= du; kmin = k;}
	uPAll->Fill( uP );
	uPlots[module]->Fill( uP );
	
	duB[module][0]->Fill(du);
	dvB[module]->Fill(dv);
	vCut->Fill(dv);
	//	if (TMath::Abs(dv) > rCut ) continue;
	uCut->Fill(du);
	
	duB[module][1]->Fill(du);
	//	if (TMath::Abs(du) > 2.*rCut) continue;
	
      } //hits loop
      if (TMath::Abs(dvmin) < 1000.) {
	dMin->Fill(dvmin,dumin);
	vMin->Fill(dvmin);
	uMin->Fill(dumin);
	//	if (TMath::Abs(dvmin) < rCut ) uMinC->Fill(dumin);
      }	 
    } //track loop
  } //jentry loop (event loop)
  fOut->Write();
#endif
} //end of Loop()
