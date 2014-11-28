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
#include "StSvtHit.h"
#include "StSsdHit.h"
#include "TGeoMatrix.h"
#include "StarRoot/THelixTrack.h"
#include "EventT.h"
#include "TrackT.h"
#include "HitT.h"
#include "TKey.h"
#include "TDirectory.h"
#include "TClass.h"
//#include "StSvtPool/SvtMatchedTree/SvtMatchedTree.h"
#include "TRVector.h"
#include "TRSymMatrix.h"
#include "StSvtBarrelHitCollection.h"
#include "StSvtHitCollection.h"
#include "StSvtLadderHitCollection.h"
#include "StSsdLadderHitCollection.h"
#include "StSsdWaferHitCollection.h"
#include "StSsdHitCollection.h"
#include "StDbUtilities/St_svtRDOstrippedC.h"
#include "StDedxPidTraits.h"
// #include "StDbUtilities/StSvtCoordinateTransform.hh"
// #include "StDbUtilities/StSvtLocalCoordinate.hh"
// #include "StDbUtilities/StSvtWaferCoordinate.hh"
#include "StDbUtilities/St_svtHybridDriftVelocityC.h"
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
Int_t  EventT::Build(StEvent *pEventT, UInt_t MinNoHits, Double_t pCut) {
  static const Int_t NoFitPointCutForGoodTrackT = 15;
  Int_t iok = 1;
  fIsValid = kFALSE;
  if (! pEventT) return iok;
  UInt_t NprimVtx = pEventT->numberOfPrimaryVertices();
  if (! NprimVtx) return iok;
  StPrimaryVertex *pVertex=0;
  Int_t ibest = -1;
  Int_t nBestTracks = -1;
  Int_t nGoodTpcTracks;
  for (UInt_t ipr=0; ipr < NprimVtx ; ipr++) {
    pVertex = pEventT->primaryVertex(ipr);
    if (! pVertex) continue;
    UInt_t nDaughters = pVertex->numberOfDaughters();
    nGoodTpcTracks = 0;
    for (UInt_t i=0; i < nDaughters; i++) {
      StTrack* pTrackT = pVertex->daughter(i);
      if ( pTrackT->fitTraits().numberOfFitPoints(kTpcId) >=  NoFitPointCutForGoodTrackT) nGoodTpcTracks++;
    }  
    if (nBestTracks < nGoodTpcTracks) {nBestTracks = nGoodTpcTracks; ibest = ipr;}
  }
  if (ibest < 0) return iok;
  pVertex = pEventT->primaryVertex(ibest);
  StSvtHitCollection* SvtHitCollection = pEventT->svtHitCollection();
  StSsdHitCollection* SsdHitCollection = pEventT->ssdHitCollection();
  St_svtRDOstrippedC *svtRDOs = St_svtRDOstrippedC::instance();
  if (! SvtHitCollection && ! SsdHitCollection) { cout << "No SVT & SSD HitT Collections" << endl; return iok;}
  const StThreeVectorF& xyzP = pVertex->position();
  fVertex[0] = xyzP.x();
  fVertex[1] = xyzP.y();
  fVertex[2] = xyzP.z();
  StMatrixF vCM = pVertex->covariantMatrix();
  fCovariantMatrix[0] = vCM(1,1); // left triangular
  fCovariantMatrix[1] = vCM(1,2);
  fCovariantMatrix[2] = vCM(2,2);
  fCovariantMatrix[3] = vCM(1,3);
  fCovariantMatrix[4] = vCM(2,3);
  fCovariantMatrix[5] = vCM(3,3);
  fNPTracks = pVertex->numberOfDaughters();
  //Save current Object count
  Clear();
  
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
  //  Create and Fill the TrackT objects
  for (UInt_t t = 0; t < fNPTracks; t++) {
    StTrack *pTrackT = pVertex->daughter(t);
    if (! pTrackT) continue;
    StTrackNode *node = pTrackT->node();
    if (! node) continue;
#ifdef __USE_GLOBAL__
    StGlobalTrack *gTrackT = (StGlobalTrack *) node->track(global);
    if (! gTrackT) continue;
#endif
    StTrackDetectorInfo*    dInfo = pTrackT->detectorInfo();
    if (! dInfo) continue;
    static StDetectorId ids[2] = {kSvtId, kSsdId};
    UInt_t Nsp = dInfo->numberOfPoints(ids[0]) + dInfo->numberOfPoints(ids[1]);
    if (MinNoHits > 0 && Nsp < MinNoHits) continue;
    UInt_t npoints = dInfo->numberOfPoints() + 100*(dInfo->numberOfPoints(ids[0]) + 10*dInfo->numberOfPoints(ids[1]));
    UInt_t nPpoints = 
      pTrackT->numberOfPossiblePoints() + 
      100*(pTrackT->numberOfPossiblePoints(ids[0]) + 10*pTrackT->numberOfPossiblePoints(ids[1]));
    StThreeVectorD g3 = pTrackT->geometry()->momentum();
#ifdef __USE_GLOBAL__
    StThreeVectorD g3Gl = gTrackT->geometry()->momentum();
#endif
    Double_t pT = g3.perp();
    Double_t pMom = g3.mag();
    if (pMom < pCut) continue;
    TrackT *track = AddTrackT();
    Double_t InvpT = 0;
    Double_t TanL = 999999;
    if (TMath::Abs(pT) > 1.e-7) {
      InvpT = pTrackT->geometry()->charge()/pT;
      TanL = g3.z()/pT;
    }
    track->SetInvpT(InvpT);
    track->SetPhi(TMath::ATan2(g3.y(),g3.x()));
    track->SetTanL(TanL);
    static const Double_t EC = 2.9979251E-4;
    Double_t Rho = - EC*InvpT*field;
    track->SetRho(Rho);
    Double_t I70 = 0;
    Double_t TrackLength70 = 0;
    StSPtrVecTrackPidTraits &traits = pTrackT->pidTraits();
    UInt_t size = traits.size();
    StDedxPidTraits *pid;
    for (UInt_t i = 0; i < size; i++) {
      if (! traits[i]) continue;
      if ( traits[i]->IsZombie()) continue;
      pid = dynamic_cast<StDedxPidTraits*>(traits[i]);
      if (! pid || pid->method() != kTruncatedMeanId) continue;
      I70 = pid->mean();
      TrackLength70 = pid->length();
    }
    track->SetdEdx(I70,TrackLength70);
#ifdef __USE_GLOBAL__
    Double_t pTGl = g3Gl.perp();
    //    Double_t pMomGl = g3Gl.mag();
    Double_t InvpTGl = 0;
    Double_t TanLGl = 999999;
    if (TMath::Abs(pTGl) > 1.e-7) {
      InvpTGl = gTrackT->geometry()->charge()/pTGl;
      TanLGl = g3Gl.z()/pTGl;
    }
    track->SetInvpTGl(InvpTGl);
    track->SetPhiGl(TMath::ATan2(g3Gl.y(),g3Gl.x()));
    track->SetTanLGl(TanLGl);
    Double_t RhoGl = - EC*InvpT*field;
    track->SetRhoGl(RhoGl);
    track->SetNPpoint(nPpoints);
#endif

    track->SetN(0);
    track->SetNpoint(npoints);
    track->SetNPpoint(nPpoints);
#if 0
    const Double_t XyzDirRho[6] = {fVertex[0], fVertex[1], fVertex[2], track->GetTanL(), track->GetPhi(), track->GetRho()};
    const Double_t XyzDirRhoGl[6] = {fVertex[0], fVertex[1], fVertex[2], track->GetTanL(), track->GetPhi(), track->GetRho()};
#endif
    THashList *fRotList = RotMatrices();
    if (! fRotList) continue;
    TIter next(fRotList);
    TGeoHMatrix *comb = 0;
    while ((comb = (TGeoHMatrix *) next())) {
      TString combName(comb->GetName());
      if (! combName.BeginsWith("R")) continue;
      Int_t Id;
      sscanf(comb->GetName()+1,"%04i",&Id);
      UInt_t Ladder = Id%100;
      UInt_t Layer  = Id/1000; if (Layer > 7) Layer = 7;
      UInt_t Wafer  = (Id - 1000*Layer)/100;
      UInt_t Barrel = (Layer - 1)/2 + 1;
      if (_debug) {
	cout << comb->GetName() << "\tLayer/Ladder/Wafer = " << Layer << "/" << Ladder << "/" << Wafer << endl;
	comb->Print();
      }
      static Double_t dz[2] = {3.00, 2.10};
      static Double_t dx[2] = {3.00, 3.65};
      Int_t k = 0; // svt
      if (Layer > 6) k = 1;
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
#endif
      Double_t *rot = comb->GetRotationMatrix();
      Double_t *tra = comb->GetTranslation();
      const StThreeVectorD normal(rot[2],      rot[5],      rot[8]);
      const StThreeVectorD middle(tra);
      StPhysicalHelixD helixI = pTrackT->geometry()->helix();
      Double_t sh = helixI.pathLength(middle, normal); if (_debug) cout << "StHelix sh " << sh << endl;
      if (sh <= 0 || sh > 1e3) continue;
      StThreeVectorD xyzG = helixI.at(sh); if (_debug) cout << "StHelix xyzG\t" << xyzG << endl;
      Double_t xyzGPred[3] = {xyzG.x(),xyzG.y(),xyzG.z()};
      Double_t uvPred[3];
      comb->MasterToLocal(xyzGPred,uvPred);
      TRVector xyzL(3,uvPred); if (_debug) cout << "StHelix xyzL\t" << xyzL << endl;
      Double_t dirGPred[3] = {helixI.cx(sh),helixI.cy(sh),helixI.cz(sh)};
      Double_t dxyzL[3];
      comb->MasterToLocalVect(dirGPred,dxyzL);
      Double_t tuvPred[2] = {dxyzL[0]/dxyzL[2], dxyzL[1]/dxyzL[2]};
      if (_debug) cout << "StHelix tU/tV =  " << tuvPred[0] << "\t" << tuvPred[1] << endl; 
#ifdef __USE_GLOBAL__
      
      StPhysicalHelixD helixG = gTrackT->geometry()->helix();
      sh = helixG.pathLength(middle, normal); if (_debug) cout << "StHelix sh " << sh << endl;
      if (sh < 0 || sh > 1e3) continue;
      xyzG = helixG.at(sh); if (_debug) cout << "StHelix xyzG\t" << xyzG << endl;
      Double_t xyzGPredGl[3] = {xyzG.x(),xyzG.y(),xyzG.z()};
      Double_t uvPredGl[3];
      comb->MasterToLocal(xyzGPredGl,uvPredGl);
      TRVector xyzLGl(3,uvPredGl); if (_debug) cout << "StHelix xyzL\t" << xyzL << endl;
      Double_t dirGPredGl[3] = {helixG.cx(sh),helixG.cy(sh),helixG.cz(sh)};
      comb->MasterToLocalVect(dirGPredGl,dxyzL);
      Double_t tuvPredGl[2] = {dxyzL[0]/dxyzL[2], dxyzL[1]/dxyzL[2]};
      if (_debug) cout << "StHelix tU/tV =  " << tuvPredGl[0] << "\t" << tuvPredGl[1] << endl; 
#endif      
      
      if (TMath::Abs(uvPred[0]) > dx[k] + 1.0) continue;
      if (TMath::Abs(uvPred[1]) > dz[k] + 1.0) continue;
      StPtrVecHit &hvec = pTrackT->detectorInfo()->hits();
      UInt_t NoHits = hvec.size();
      StHit *hit = 0;
      for (UInt_t j = 0; j < NoHits; j++) {
	StHit *hitc = hvec[j];
	if (! hitc) continue;
	if (hitc->detector() == kSvtId) {
	  StSvtHit *htSvt = (StSvtHit *) hitc;
	  if (htSvt->barrel() == Barrel &&
	      htSvt->ladder() == Ladder &&
	      htSvt->wafer()  == Wafer) {hit = hitc; break;}
	}
	if (hitc->detector() == kSsdId) {
	  StSsdHit *htSsd = (StSsdHit *) hitc;
	  if (htSsd->ladder() == Ladder &&
	      htSsd->wafer()  == Wafer) {hit = hitc; break;}
	}
      }
      HitT *ht = AddHitT();
      UInt_t Hybrid = 1;
      if (uvPred[0] >= 0) Hybrid = 2;
      ht->SetId(Barrel,Layer,Ladder,Wafer,Hybrid);
      ht->SetisTrack(t+1);
      ht->SetUVPred (uvPred[0],uvPred[1]);
      ht->SettUVPred(tuvPred[0],tuvPred[1]);
      ht->SetXyzG(xyzGPred);
      ht->SetDirG(dirGPred);
#ifdef __USE_GLOBAL__
      ht->SetUVPredGl (uvPredGl[0],uvPredGl[1]);
      ht->SettUVPredGl(tuvPredGl[0],tuvPredGl[1]);
      ht->SetXyzGl(xyzGPredGl);
      ht->SetDirGl(dirGPredGl);
#endif
      if (hit) {
	SetHitT(ht, hit, comb, track);
	ht->SetisFitted(t+1);
      }
      StSPtrVecSvtHit *hitsvt = 0;
      StSPtrVecSsdHit *hitssd = 0;
      Int_t NoHitPerTrack = 0;
      if (Layer < 7) { // svt
	if (! SvtHitCollection) continue;
	StSvtBarrelHitCollection* barrelCollection = SvtHitCollection->barrel(Barrel-1);
	if (! barrelCollection) continue;
	StSvtLadderHitCollection *ladderCollection = barrelCollection->ladder(Ladder-1);
	if (! ladderCollection) continue;
	if (svtRDOs && svtRDOs->svtRDOstrippedStatus(Barrel,Ladder,Wafer)) continue;
	StSvtWaferHitCollection* waferCollection = ladderCollection->wafer(Wafer-1);
	if (! waferCollection) continue;
	hitsvt = &waferCollection->hits();
      } else         { // ssd
	if (! SsdHitCollection) continue;
	StSsdLadderHitCollection* ladderCollection = SsdHitCollection->ladder(Ladder-1);
	if (! ladderCollection) continue;
	
	StSsdWaferHitCollection* waferCollection = ladderCollection->wafer(Wafer-1);
	if (! waferCollection) continue;
	hitssd = &waferCollection->hits();
      }
      if (! hitsvt && ! hitssd) continue;
      if (hitsvt) NoHits = hitsvt->size();
      if (hitssd) NoHits = hitssd->size();
      ht->SetNofHits(NoHits);
      if (! NoHits) continue;

      for (UInt_t l = 0; l < NoHits; l++) {
	hit = 0;
	if (hitsvt) hit = (*hitsvt)[l];
	if (hitssd) hit = (*hitssd)[l];
	if (hit) {
	  //if (hit->flag()>=4) continue;
	  //if (hit->flag()< 0) continue;
	  //	  cout << "hitFlag=" << hit->flag() << endl;
	  HitT *h = AddHitT();
	  h->SetHitFlag(UInt_t(hit->flag()));
	  h->SetUVPred (uvPred[0],uvPred[1]);
	  h->SettUVPred(tuvPred[0],tuvPred[1]);
	  h->SetXyzG(xyzGPred);
	  h->SetDirG(dirGPred);
#ifdef __USE_GLOBAL__

	  h->SetUVPredGl (uvPredGl[0],uvPredGl[1]);
	  h->SettUVPredGl(tuvPredGl[0],tuvPredGl[1]);
	  h->SetXyzGl(xyzGPredGl);
	  h->SetDirGl(dirGPredGl);
#endif
	  SetHitT(h, hit, comb, track);
	  NoHitPerTrack++;
	  h->SetHitPerTrack(NoHitPerTrack);
	  //	    SetHitT(h, hit, comb, track, &TPDeriv);
	}
      }
    }
  }
  fIsValid = kTRUE;
  iok = 0;
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
  struct RDO_t {
    const Char_t *name;
    Int_t ladder, barrel;
    Int_t rdo;
  };
  const Int_t NRDOS = 36;
  const RDO_t RDOS[NRDOS] = {
    {"L01B1", 1,1, 1},{"L02B1", 2,1, 2},{"L03B1", 3,1, 4},{"L04B1", 4,1, 5},{"L05B1", 5,1, 7},
    {"L06B1", 6,1, 8},{"L07B1", 7,1,10},{"L08B1", 8,1,11},{"L01B2", 1,2, 3},{"L02B2", 2,2, 3},
    {"L03B2", 3,2, 3},{"L04B2", 4,2, 6},{"L05B2", 5,2, 6},{"L06B2", 6,2, 6},{"L07B2", 7,2, 9},
    {"L08B2", 8,2, 9},{"L09B2", 9,2, 9},{"L10B2",10,2,12},{"L11B2",11,2,12},{"L12B2",12,2,12},
    {"L01B3", 1,3, 1},{"L02B3", 2,3, 1},{"L03B3", 3,3, 2},{"L04B3", 4,3, 2},{"L05B3", 5,3, 4}, 
    {"L06B3", 6,3, 4},{"L07B3", 7,3, 5},{"L08B3", 8,3, 5},{"L09B3", 9,3, 7},{"L10B3",10,3, 7}, 
    {"L11B3",11,3, 8},{"L12B3",12,3, 8},{"L13B3",13,3,10},{"L14B3",14,3,10},{"L15B3",15,3,11},
    {"L16B3",16,3,11} 
  };
  UInt_t B = 0, L = 0, l = 0, W = 0, H = 0;
  Int_t rdo = 0;
  h->SetRDO(rdo);
  if (hit->detector() == kSvtId) {
    StSvtHit *ht = (StSvtHit *) hit;
    B = ht->barrel();
    L = ht->layer();
    l = ht->ladder();
    W = ht->wafer();
    H = ht->hybrid();
    h->SetId(B,L,l,W,H);
    h->SetuvD(ht->localPosition(0), ht->localPosition(1));			
    h->SetAnode(ht->anode());
    h->SetTimeB(ht->timebucket());
    for (Int_t r = 0; r < NRDOS; r++) {
      if (h->Ladder() == RDOS[r].ladder && h->Barrel() == RDOS[r].barrel) {
	rdo = RDOS[r].rdo;
	break;
      }
    }
    if (rdo) h->SetRDO(rdo);
    St_svtHybridDriftVelocityC *d = St_svtHybridDriftVelocityC::instance();
    if (d && d->p(B,l,W,H)) {
      h->SetLM(d->CalcU(B,l,W,H,ht->timebucket(),ht->anode()),
	       d->CalcV(H,ht->anode()));
      h->SetuHat(d->uHat(B,l,W,H,ht->timebucket()));
    }
  }
  h->SetUsedInFit(hit->usedInFit());
  if (hit->detector() == kSsdId) {
    StSsdHit *ht = (StSsdHit *) hit;
    B = 4;
    L = 7;
    l = ht->ladder();
    W = ht->wafer();
    h->SetId(B,L,l,W,H);
    h->SetLM(-ht->localPosition(0), ht->localPosition(1));			
    h->SetuHat(-ht->localPosition(0));
    h->SetAnode(0);
    h->SetTimeB(0);
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
  struct Geom_t {
    Int_t Barrel;
    Int_t Layer;
    Int_t NoLadders;
    Int_t NoWafers;
  };
  const Int_t NoLayers = 7;
  //Barrel, Layer Nladder Nwafer
  static const Geom_t SvtSsdConfig[NoLayers] = 
  {    {1,     1,      8,   4}, // even
       {1,     2,      8,   4}, // odd
       {2,     3,     12,   6}, // event
       {2,     4,     12,   6}, // odd
       {3,     5,     16,   7}, // even
       {3,     6,     16,   7}, // odd
       {4,     7,     20,  16}  // Ssd
  };
  static const Int_t ssdSector[20] = {// 100*sector + ladder
      101, 102,
      203, 204, 205, 206, 207, 208, 209,
      310, 311, 312, 
      413, 414, 415, 416, 417, 418, 419,
      120
      };
  barrel = B; layer = L; ladder = l; wafer = W; hybrid = H;
  if (barrel == 0) Id = 7000       + 100*wafer + ladder;
  else             Id = 1000*layer + 100*wafer + ladder;
  sector = -1;
  if (layer < 7) {
    sector = 0;
    if (ladder > SvtSsdConfig[layer-1].NoLadders/2) sector = 1;
  } else         {sector = ssdSector[ladder-1]/100 + 1; barrel = 4;}
}
#if 0
//________________________________________________________________________________
void HitT::SetId(StHit *shit) {
  if (shit) {
    if (shit->detector() == kSvtId) {
      StSvtHit *hit = (StSvtHit *) shit;
      B = hit->barrel();
      L = hit->layer();
      l = hit->ladder();
      W = hit->wafer();
      H = hit->hybrid();
    }
    if (shit->detector() == kSsdId) {
      StSsdHit *hit = (StSsdHit *) shit;
      B = 4;
      L = 7;
      l = hit->ladder();
      W = hit->wafer();
    }
  }
  SetId(B,L,l,W,H);
}
#endif
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
