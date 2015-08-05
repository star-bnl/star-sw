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
    StGlobalTrack *gTrackT = (StGlobalTrack *) node->track(global);
    if (! gTrackT) continue;
    StThreeVectorD g3 = gTrackT->geometry()->momentum();
    Double_t pT = g3.perp();
    Double_t pMom = g3.mag();
    if (pMom < pCut) continue;
    TrackT *track = AddTrackT();
    Double_t InvpT = 0;
    Double_t TanL = 999999;
    if (TMath::Abs(pT) > 1.e-7) {
      InvpT = gTrackT->geometry()->charge()/pT;
      TanL = g3.z()/pT;
    }
    track->SetInvpT(InvpT);
    track->SetPhi(TMath::ATan2(g3.y(),g3.x()));
    track->SetTanL(TanL);
    static const Double_t EC = 2.9979251E-4;
    Double_t Rho = - EC*InvpT*field;
    track->SetRho(Rho);

    track->SetN(0);
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
      if (_debug) {
	cout << comb->GetName() << "\tmodule = " << module << endl;
	comb->Print();
      }
      StGmtHitCollection* GmtHitCollection = GmtCollection->getHitCollection(module);
      if (! GmtHitCollection) { cout << "No GMT HitT Collections for mudule " << module << endl; continue;}
      StSPtrVecGmtHit& hitvec = GmtHitCollection->getHitVec();
      UInt_t NoHits = hitvec.size();
      if (! NoHits) continue;
      static Double_t dz[2] = {50.00, 2.10};
      static Double_t dx[2] = {50.00, 3.65};
      Int_t k = 0; // gmt
#if 0
      // check straight line
      const Double_t *X = &XyzDirRho[0]; // Global
      Double_t x0[3];               // Local
      comb->MasterToLocal(X,x0);
      Double_t CosL = 1./TMath::Sqrt(1. + XyzDirRho[3]*XyzDirRho[3]);
      Double_t SinL = XyzDirRho[3]*CosL;
      Double_t CosP = TMath::Cos(XyzDirRho[4]);
      Double_t SinP = TMath::Sin(XyzDirRho[4]);
      Double_t dir[3], d[3];
      dir[0] = CosL*CosP;
      dir[1] = CosL*SinP;
      dir[2] = SinL;
      comb->MasterToLocalVect(dir,d);
      if (TMath::Abs(d[2]) < 1.e-7) continue;
      Double_t s = - x0[2]/d[2]; if (_debug) cout << "straight line s " << s << endl;;
      if (s < 0) continue;
      TRVector uvP(3,x0);
      TRVector tuvP(2,d[0]/d[2],d[1]/d[2]);
      TRVector DD(3,d);
      DD *= s;
      uvP += DD;                  if (_debug) cout << "straight line uvP\t" << uvP << "\ttuvP \t" << tuvP << endl;
      //                        svt   ssd
      if (TMath::Abs(uvP[0]) > dx[k] + 1.0) continue;
      if (TMath::Abs(uvP[1]) > dz[k] + 1.0) continue;
      // 	if (_debug) {
      StThreeVectorD n, m;
      comb->LocalToMasterVect(unit.xyz(),n.xyz());
      comb->LocalToMaster(zero.xyz(),m.xyz());
#endif
      Double_t *rot = comb->GetRotationMatrix();
      Double_t *tra = comb->GetTranslation();
      const StThreeVectorD unit(0.,0.,1.);
      const StThreeVectorD zero(0.,0.,0.);
      const StThreeVectorD normal(rot[2],      rot[5],      rot[8]);
      const StThreeVectorD middle(tra);
      StPhysicalHelixD helixO = gTrackT->outerGeometry()->helix();
      Double_t sh = helixO.pathLength(middle, normal); if (_debug) cout << "StHelix sh " << sh << endl;
      if (sh <= 0 || sh > 1e3) continue;
      StThreeVectorD xyzG = helixO.at(sh); if (_debug) cout << "StHelix xyzG\t" << xyzG << endl;
      Double_t xyzGPred[3] = {xyzG.x(),xyzG.y(),xyzG.z()};
      Double_t uvPred[3];
      comb->MasterToLocal(xyzG.xyz(),uvPred);
      TRVector xyzL(3,uvPred); if (_debug) cout << "StHelix xyzL\t" << xyzL << endl;
      Double_t dirGPred[3] = {helixO.cx(sh),helixO.cy(sh),helixO.cz(sh)};
      Double_t dxyzL[3];
      comb->MasterToLocalVect(dirGPred,dxyzL);
      Double_t tuvPred[2] = {dxyzL[0]/dxyzL[2], dxyzL[1]/dxyzL[2]};
      if (_debug) cout << "StHelix tU/tV =  " << tuvPred[0] << "\t" << tuvPred[1] << endl; 
      if (TMath::Abs(uvPred[0]) > dx[k] + 1.0) continue;
      if (TMath::Abs(uvPred[1]) > dz[k] + 1.0) continue;

      for (UInt_t l = 0; l < NoHits; l++) {
	StHit *hit = hitvec[l];
	if (hit) {
	  //if (hit->flag()>=4) continue;
	  //if (hit->flag()< 0) continu;
	  //	  cout << "hitFlag=" << hit->flag() << endl;
	  HitT *h = AddHitT();
	  h->SetHitFlag(UInt_t(hit->flag()));
	  h->SetUVPred (uvPred[0],uvPred[1]);
	  h->SettUVPred(tuvPred[0],tuvPred[1]);
	  h->SetXyzG(xyzGPred);
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
    h->SetuvD(ht->getLocalX(), ht->getLocalY());
    h->SetuvDError(ht->getErrorLocalX(), ht->getErrorLocalY());
    h->SetSigma(ht->getSigmaX(), ht->getSigmaY());
    h->SetSigmaError(ht->getErrorSigmaX(), ht->getErrorSigmaY());
    h->SetAdc(ht->getAdcX(), ht->getAdcY());
    h->SetAdcError(ht->getErrorAdcX(), ht->getErrorAdcY());
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
#ifdef __USE_GLOBAL__

    Double_t uvwPGl[3] = {h->GetPredGlU(),h->GetPredGlV(),0};
    rotL->LocalToMaster(uvwPGl,xyzLadder);
    h->SetXyzGlL(xyzLadder);
#endif
  } else {
    
    cout << Form("WL%s",comb->GetName()+1) << " has not been found" << endl;
    h->SetL(xyzLadder[0],xyzLadder[1],xyzLadder[2]);
    h->SetXyzL(xyzLadder);
#ifdef __USE_GLOBAL__
    h->SetXyzGlL(xyzLadder);
#endif
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
