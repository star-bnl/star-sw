// Author : Yuri V. Fisyak 10/01/2004
// $Id: StTpcAlignerMaker.cxx,v 1.23 2014/09/10 13:54:58 fisyak Exp $
// $Log: StTpcAlignerMaker.cxx,v $
// Revision 1.23  2014/09/10 13:54:58  fisyak
// Freeze
//
// Revision 1.21  2014/04/30 22:09:23  fisyak
// Freeze
//
// Revision 1.20  2014/04/24 21:48:17  fisyak
// Freeze
//
// Revision 1.19  2011/12/16 20:35:23  fisyak
// Freeze
//
// Revision 1.18  2011/08/24 23:52:20  hejdar
// bug fix with segment status
//
// Revision 1.17  2011/08/24 22:19:18  hejdar
// bug fix with segment status
//
// Revision 1.16  2011/08/23 20:29:06  fisyak
// Add rejection for bad segments
//
// Revision 1.15  2011/08/23 19:47:48  fisyak
// Add check for detectorInfo
//
// Revision 1.13  2011/08/22 13:37:25  fisyak
// Freeze Super Sector version
//
// Revision 1.12  2011/08/09 17:31:23  fisyak
// Freeze before fixing coordinamte swap for super sector alignemnt
//
// Revision 1.11  2011/06/29 15:04:44  fisyak
// Freeze
//
// Revision 1.10  2011/06/08 21:52:47  fisyak
// Freeze version with ToF
//
// Revision 1.9  2011/05/04 18:44:33  fisyak
// Freeze
//
// Revision 1.8  2011/04/29 14:02:22  fisyak
// Separate documentation from codes
//
// Revision 1.7  2011/04/25 20:56:07  fisyak
// Freeze Pass N
//
// Revision 1.6  2011/04/22 16:03:53  fisyak
// Replace NTuple by TTree, use errors from THelixFit
//
// Revision 1.5  2011/04/11 19:51:04  fisyak
// Freeze
//
// Revision 1.4  2009/07/06 22:57:21  fisyak
// switch from obsolete THelixTrack::Fit to THelixFitter::Fit
//
// Revision 1.3  2006/11/27 17:41:53  fisyak
// Replace TCL.h => TCernLib.h
//
// Revision 1.2  2004/11/16 23:47:31  fisyak
// Add histograms for residuals
//
// Revision 1.1.1.1  2004/10/28 00:26:46  fisyak
//
//
//#define __TIME_CORRECTION__
#include <assert.h>
#include <vector>
#include <Stiostream.h>		 
#include "StTpcAlignerMaker.h"
#include "StDetectorDbMaker/StiTpcInnerHitErrorCalculator.h"
#include "StDetectorDbMaker/StiTpcOuterHitErrorCalculator.h"
#include "StDetectorDbMaker/St_tpcStatusC.h"
	       // StDb
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StCoordinates.hh" 
#include "StTpcDb/StTpcDb.h"
	       // StarClassLibrary
#include "SystemOfUnits.h"
#include "StPhysicalHelixD.hh"
	       // StEvent 
#include "StEventTypes.h"
#include "StEvent/StBTofCollection.h"
#include "StEvent/StBTofRawHit.h"
#include "StEvent/StBTofHeader.h"
#include "StEvent/StDcaGeometry.h"
	       // StarRoot
#include "THelixTrack.h"
#include "TRVector.h"
#include "TRMatrix.h"
	       // ROOT
#include "TChain.h"
#include "TFile.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
#endif
#include "TIterator.h"
#include "TArrayI.h"
#include "TArrayD.h"
#include "TH3.h"
#include "StBFChain.h"
#include "StMessMgr.h" 
#define __DEBUG__
#ifdef __DEBUG__
#define DEBUG_LEVEL if (Debug()%10 > 1)
#define PrPP(A,B)  DEBUG_LEVEL {LOG_INFO << "StTpcAlignerMaker::" << (#A) << "\t" << (#B) << " = \t" << (B) << endm;}
#else
#define PrPP(A,B)
#endif
//________________________________________________________________________________
ClassImp(SectorSegment);
ClassImp(StTpcW2SMatch);
ClassImp(HelixPar_t);
ClassImp(Hit_t);
ClassImp(StTpcInOutMatch);
ClassImp(StTpcAlignerMaker);
//________________________________________________________________________________
static TTree* TpcInOutTree = 0;
static TTree* TpcW2STree = 0;
static Int_t _debug = 0;
//________________________________________________________________________________
void  SectorSegment::Print(Option_t */* option */) const {
  cout << "Segment: " << GetName() << " for sector " << fSector << endl;
  if (_debug) {
    TIter next(&fList);
    Int_t i = 0;
    StTpcHit *tpcHit = 0;
    while ((tpcHit = (StTpcHit *) next())) {
      cout << i++ << "\t" << *tpcHit << endl;
    }
    fHelix.Print();
  }
  cout << "rowMin " << fRowMin << " at X = " << fXmin
       << "\trowMax " << fRowMax << " at X = " << fXmax << endl;
  if (HelixSmin.sector > 0) cout << " HelixSmin " << HelixSmin << endl;
  if (HelixSmax.sector > 0) cout << " HelixSmax " << HelixSmax << endl;
}
//________________________________________________________________________________
ostream&  operator<<(ostream& os, const HelixPar_t v) {
  os << "sector " << v.sector << " Rho = " << v.Rho << " +/- " << v.dRho
     << " pxyz: " << v.nx << " " << v.ny << " " << v.nz
     << " xyz: "  << v.x  << " " << v.y  << " " << v.z;
  if (_debug) {
    os << endl;
    TRSymMatrix C(5,v.fCov);
    os << C << endl;
  }
  os << "\tPoints = " << v.Npoints << "\tused = " << v.Nused << "\tchi2/Ndf = " << v.Chi2 << "/" << v.Ndf;
  return os;
}
//_____________________________________________________________________________
HelixPar_t &HelixPar_t::operator=(const THelixFitter &helix) {
  Chi2 = helix.Chi2();
  Ndf  = helix.Ndf();
  Npoints = helix.Size();
  Nused   = helix.Used();
  helix.Get(xyz(),pxyz(),Rho);
  return *this;
}
//_____________________________________________________________________________
Int_t StTpcAlignerMaker::Init(){
  StBFChain *chain = dynamic_cast<StBFChain*>(GetChain());
  TFile *f = 0;
  if (chain) f = chain->GetTFile();
  if (! f) {
    gMessMgr->Warning() << "StTpcAlignerMaker::Init root file has not been found" << endm;
    assert(f);
  }
  gMessMgr->Warning() << "StTpcAlignerMaker::Init found " << f->GetName() << " Create TpcInOutTree" << endm;
  f->cd();
  Int_t comp   = 1;       // by default file is compressed
  f->SetCompressionLevel(comp);
  Int_t split  = 9;       // by default, split Event in sub branches
  //   Int_t split  = -2;       // by default, split Event in sub branches << old style
  Int_t bufsize = 64000;
  if (split)  bufsize /= 4;
#if 1
   Int_t branchStyle = 1; //new style by default
   if (split < 0) {branchStyle = 0; split = -1-split;}
   TTree::SetBranchStyle(branchStyle);
#endif
  TpcInOutTree = new TTree("TpcInOutTree","the TPC residuals between Inner and Outer sub sectors");
  TpcInOutTree->SetAutoSave(1000000000); // autosave when 1 Gbyte written
  TpcInOutTree->SetCacheSize(10000000);  // set a 10 MBytes cache (useless when writing local files)
#if  1 /* bug in TStreamerInfo*, fixed 09/05/14, ROOT_VERSION_CODE < ROOT_VERSION(5,34,20) */
  StTpcInOutMatch::Class()->IgnoreTObjectStreamer();
  HelixPar_t::Class()->IgnoreTObjectStreamer();
  StTpcW2SMatch::Class()->IgnoreTObjectStreamer();
#endif
  fTpcInOutMatch = new StTpcInOutMatch();
  TBranch *branch = TpcInOutTree->Branch("StTpcInOutMatch","StTpcInOutMatch",&fTpcInOutMatch, bufsize,split);
#if 1
      branch->SetAutoDelete(kFALSE);
      if(split >= 0 && branchStyle) TpcInOutTree->BranchRef();
#endif
  fTpcW2SMatch = new StTpcW2SMatch();
  TpcW2STree = new TTree("TpcW2STree","the TPC residuals for prediction from sector W in sector S");
      TpcW2STree->SetAutoSave(1000000000); // autosave when 1 Gbyte written
      TpcW2STree->SetCacheSize(10000000);  // set a 10 MBytes cache (useless when writing local files)
  branch = TpcW2STree->Branch("StTpcW2SMatch","StTpcW2SMatch",&fTpcW2SMatch, bufsize,split);
  branch->SetAutoDelete(kFALSE);
#if 1
      if(split >= 0 && branchStyle) TpcW2STree->BranchRef();
#endif
  return StMaker::Init();
}
//_____________________________________________________________________________
Double_t StTpcAlignerMaker::PhiFromBTofTray(Int_t tray) {
  Double_t Phi = 0;
  if (tray > 60) { // east
    Phi = 6*(tray + 0.5 - 61) - 255;
  } else {
    Phi = 435 - 6*(tray + 0.5 -  1);
  }
  if (Phi > 180) Phi -= 360;
  if (Phi <-180) Phi += 360;
  return TMath::DegToRad()*Phi;
}
//_____________________________________________________________________________
Int_t StTpcAlignerMaker::TpcSectorFromBTofTray(Int_t tray) {
  Double_t angle = TMath::RadToDeg()*PhiFromBTofTray(tray);
  if (angle < 0) angle += 360;
  Int_t sec = (Int_t) ((angle + 15.)/30.);
  if (tray <= 60) {sec = 15 - sec; if (sec > 12) sec -= 12;}
  else            {sec +=       9; if (sec <=12) sec += 12;}
  return sec;
}
//_____________________________________________________________________________
Int_t StTpcAlignerMaker::Make(){
  enum {kRejected                    = BIT(19),  // take out the track from analysis
	kComingFromOutSide           = BIT(20),  // track is coming towards beam pipe
	kComingFromInSideTofMatched  = BIT(21),  // track is coming from beam pipe
        kNoToFDependece              = BIT(18)}; // Laser membrane tracks 
  assert(gStTpcDb);
  StTpcCoordinateTransform transform(gStTpcDb);
  Double_t bField = 0;
  StEvent* pEvent = dynamic_cast<StEvent*> (GetInputDS("StEvent"));
  if (!pEvent) {
    gMessMgr->Info() << "StdEdxY2Maker: no StEvent " << endm;
    return kStOK;        // if no event, we're done
  }
  if (pEvent->runInfo()) bField = pEvent->runInfo()->magneticField();//*kilogauss;
  Int_t TriggerId = 0;
  static Int_t goodIds[8] = {9200,9201,               // laser
			     310811, 310812, 310813,  // cosmic
			     420811, 420812, 420813}; // cosmic 2014
  const StTriggerIdCollection* trig = pEvent->triggerIdCollection();
  if (trig) {
    const StTriggerId *nominal = trig->nominal();
    if (nominal) {
      for (Int_t i = 0; i < 8; i++) {
	if (nominal->isTrigger(goodIds[i])) {TriggerId = goodIds[i]; break;}
      } 
    } else {TriggerId = goodIds[0];}
  }
  Bool_t LaserT  = TriggerId == goodIds[0] || TriggerId == goodIds[1];
  Bool_t CosmicT = TriggerId != 0 && ! LaserT;
  Bool_t EventT  = ! LaserT && ! CosmicT;
  StSPtrVecTrackNode& trackNode = pEvent->trackNodes();
  UInt_t nTracks = trackNode.size();
  if (! nTracks) return kStOK;
  //  if (TriggerId > 310810 && nTracks > 5) return kStOK;
  // Check ToF to understand direction where muons a comint from
  StBTofCollection *btofcol = pEvent->btofCollection();
  Double_t tofMin = 1e9; 
  Double_t tofMax =   0;
  if (btofcol) {
    StSPtrVecBTofHit &tofHits = btofcol->tofHits();
    UInt_t nHits = tofHits.size();
    for(UInt_t i=0; i < nHits; i++) {
      StBTofHit *aHit = tofHits[i]; // aHit->tray(); 
      if(!aHit) continue;
      if (!aHit->associatedTrack()) continue;
      if (Debug()%10 > 5) cout << "TofHit : " << i << *aHit << " Tpc sector " << TpcSectorFromBTofTray(aHit->tray()) << endl;
      Double_t time = aHit->leadingEdgeTime();
      if (time > tofMax) {tofMax = time;}
      if (time < tofMin) {tofMin = time;}
    }
  }
  StTrackNode *node=0;
  for (UInt_t i=0; i < nTracks; i++) {
    node = trackNode[i]; 
    if (!node) continue;
    StGlobalTrack  *gTrack = static_cast<StGlobalTrack *>(node->track(global));
    if (! gTrack ) continue;
    if (gTrack->flag() < 0)  {gTrack->SetBit(kRejected); continue;}
    StPrimaryTrack *pTrack = static_cast<StPrimaryTrack *>(node->track(primary));
    const StThreeVectorF &pxyz = gTrack->geometry()->momentum();
    Double_t pMomentum = pxyz.mag();
    if (TMath::Abs(bField) < 0.01) pMomentum = 1000;
    if (pMomentum < 1.0) {gTrack->SetBit(kRejected); continue;}
    if (! gTrack->detectorInfo()) continue;
    StPtrVecHit hvec   = gTrack->detectorInfo()->hits();
    UInt_t nTpcHits = hvec.size();
    if (nTpcHits <  10) {gTrack->SetBit(kRejected); continue;}
    if (EventT) {      continue;     }                          // event
    if (LaserT) {                                               // laser
      // mark all tracks as coming from outside
      if (pMomentum < 50) {gTrack->SetBit(kRejected); continue;}
      if (! pTrack)       gTrack->SetBit(kComingFromOutSide);
      else                gTrack->SetBit(kNoToFDependece);
      continue;
    }                                         
    if (CosmicT && btofcol && btofcol->hitsPresent() && tofMin <= tofMax) { // cosmic
      StSPtrVecBTofHit &tofHits = btofcol->tofHits();
      UInt_t nHits = tofHits.size();
      gTrack->SetBit(kRejected);
      for(UInt_t i=0; i < nHits; i++) {
	StBTofHit *aHit = tofHits[i]; // aHit->tray(); 
	if(!aHit) continue;
	if (!aHit->associatedTrack()) continue;
	if (aHit->associatedTrack() != gTrack) continue;
	Double_t time = aHit->leadingEdgeTime();
	if      (tofMax == tofMin || time == tofMax) {gTrack->ResetBit(kRejected); gTrack->SetBit(kComingFromOutSide);}
	else if (tofMax >  tofMin && time == tofMin) {gTrack->ResetBit(kRejected); gTrack->SetBit(kComingFromInSideTofMatched);}
	if (Debug()%10 > 5) cout << "TofHit : " << i << *aHit << " Tpc sector " << TpcSectorFromBTofTray(aHit->tray()) << endl;
	break;
      }
    }
  }
  static StThreeVectorD XyzI, XyzO;
  static StThreeVectorD DirI, DirO;
  static Double_t stepMX = 1.e3;
  Double_t StiErr[21];
  Double_t step;
  static StTpcLocalSectorCoordinate              local;
  static Double_t err2xy[3] = {0,0,0}, err2z = 0;
#define __IN2OUT__
#ifdef __IN2OUT__
  // Outer to Inner sector alignment
  Hit_t tpcHits[100];
  Double_t RIO[100];
  static Double_t RefSurfice[4] = {-123, 0, 1, 0};
  static Int_t idx[100];
  for (UInt_t i=0; i < nTracks; i++) {
    node = trackNode[i]; 
    if (!node) continue;
    StGlobalTrack  *gTrack = static_cast<StGlobalTrack *>(node->track(global));
    if (! gTrack ) continue;
    if (gTrack->flag() < 0) continue;
    if (! gTrack->detectorInfo()) continue;
    if (gTrack->TestBit(kRejected)) continue;
    if (TriggerId && btofcol &&
	! gTrack->TestBit(kComingFromOutSide) &&
	! gTrack->TestBit(kComingFromInSideTofMatched) &&
	! gTrack->TestBit(kNoToFDependece)) continue;
    const StDcaGeometry* dca = gTrack->dcaGeometry();
    if (! dca) continue;
    if (Debug()%10 > 5) {
      cout << "Track:" << i << *dca << endl;
    }
    const StThreeVectorF &pxyz = gTrack->geometry()->momentum();
    Double_t pMomentum = pxyz.mag();
    if (TMath::Abs(bField) < 0.01) pMomentum = 1000;
    StThreeVectorD origin    = dca->origin();
    StThreeVectorD dir       = dca->momentum().unit();
    //    Double_t       curvature = dca->curvature();
    Double_t tanl  = dir.z()/dir.perp();
    Double_t coslI = sqrt(1.+tanl*tanl);
    static const Double_t M2muon = 0.1056584*0.1056584;
    Double_t e = TMath::Sqrt(M2muon + pMomentum*pMomentum);
    Double_t beta = pMomentum/e;
    fTpcInOutMatch->Clear();
    fTpcInOutMatch->TriggerId = TriggerId;
    fTpcInOutMatch->field  = bField;
    fTpcInOutMatch->charge = gTrack->geometry()->charge();
    fTpcInOutMatch->pX     = pxyz.x();
    fTpcInOutMatch->pY     = pxyz.y();
    fTpcInOutMatch->pZ     = pxyz.z();
    StPtrVecHit hvec   = gTrack->detectorInfo()->hits();
    Int_t count = 0;
    if (hvec.size()){
      //      Int_t Id = gTrack->key();
      fTpcInOutMatch->NoFitPoints = gTrack->fitTraits().numberOfFitPoints(kTpcId);
      if (fTpcInOutMatch->NoFitPoints < 10) continue;
      Int_t NhitInSector[24]; memset(NhitInSector,0,sizeof(NhitInSector));
      // Find sector with maximum no. of hits
      //      UInt_t sectorWithMaxRowNo = 0;
      UInt_t MaxRowNo = 0;
      for (UInt_t j=0; j<hvec.size(); j++) {// hit loop
	if (! hvec[j]) continue;
	if (hvec[j]->detector() != kTpcId) continue;
	StTpcHit *tpcHit = (StTpcHit *) hvec[j];
	Int_t sector = tpcHit->sector();
	if (tpcHit->padrow() > MaxRowNo) {
	  MaxRowNo = tpcHit->padrow(); 
	  //	  sectorWithMaxRowNo = sector;
	}
	NhitInSector[sector-1]++;
      }	
      Int_t sectorWithMaxNoHits = -1;
      Int_t N = 0;
      for (Int_t j = 0; j < 24; j++) {
	if (NhitInSector[j] > N) {
	  sectorWithMaxNoHits = j+1;
	  N = NhitInSector[j];
	}
      }
      if (sectorWithMaxNoHits < 0 || N < 10) continue;
      N = 0;
      UInt_t Nhits = hvec.size();
      for (UInt_t j = 0; j < Nhits; j++) {// hit loop
	if (hvec[j]->detector() != kTpcId) continue;
	StTpcHit *tpcHit = static_cast<StTpcHit *> (hvec[j]);
	if (! tpcHit) continue;
	Int_t sector = tpcHit->sector();
	Int_t row    = tpcHit->padrow();
	if (St_tpcStatusC::instance()->status(sector,row)) continue;
	if (! tpcHit->usedInFit() || tpcHit->flag()) continue;
	if (sector != sectorWithMaxNoHits) continue;
	StGlobalCoordinate global(tpcHit->position());
	// Distance from Dca to the hit 
	StThreeVectorD dist = StThreeVectorD(tpcHit->position()) - origin;
	Double_t d = dist.perp();
	Double_t sn = 0;
	if (d > 0) sn = TMath::Abs(dir.x()*dist.y() - dir.y()*dist.x())/d;
	if (sn> 0.99) sn =  0.99;
	if (sn<0.2) {
	  d *= (1.+sn*sn/6);
	} else {
	  d *= TMath::ASin(sn)/sn;
	}
	d *= coslI;
	Double_t time = d/(TMath::Ccgs()*beta*1e-6); // mksec  
	if (gTrack->TestBit(kComingFromOutSide)) time = - time;
	//	transform(global,local,sector,row);
	StThreeVectorD xyzL;
   	StTpcDb::instance()->SupS2Glob(sector).MasterToLocal(global.position().xyz(), xyzL.xyz());
	StThreeVectorD xyz;
	StTpcDb::instance()->Flip().MasterToLocal(xyzL.xyz(), xyz.xyz());
	local = StTpcLocalSectorCoordinate(xyz,sector,row);
	tpcHits[N].hit = tpcHit;
	tpcHits[N].x = local.position().x();
	tpcHits[N].y = local.position().y();
#ifdef __TIME_CORRECTION__
	Float_t driftvel = 1e-6*gStTpcDb->DriftVelocity(sector+1); // cm/mkmsec
	tpcHits[N].z = local.position().z() - driftvel*time;
#else
	tpcHits[N].z = local.position().z();
#endif
	tpcHits[N].err2xy = tpcHit->positionError().perp2();
	tpcHits[N].err2z  = tpcHit->positionError().z()*tpcHit->positionError().z();
	RIO[N]      = tpcHits[N].y;
	tpcHits[N].row    = row;
	if (_debug) {
	  if (Debug()%10 > 5)    
	    cout << Form("row = %2i N =%2i %8.3f %8.3f %8.3f",row,N,
			 tpcHits[N].x,tpcHits[N].y,tpcHits[N].z) << endl;
	}
	N++;
      }
      //      TArrayI idxT(N); Int_t *idx = idxT.GetArray();
      TMath::Sort(N,RIO,idx,0);
      Int_t I123 = -1;
      Int_t i,k;
      for (i = 0; i < N; i++) if (RIO[idx[i]] > 123) {I123 = i; break;}
      Int_t NoIOHits[2] = {I123, N - I123};
      if (NoIOHits[0] < 5 || NoIOHits[1] < 5) continue;
      THelixFitter vHelices[2];
      HelixPar_t *HlxPars[2] = {&fTpcInOutMatch->In, &fTpcInOutMatch->Out};
      for (Int_t io = 0; io < 2; io++) {// Inner / Outer Loop
	HlxPars[io]->sector = sectorWithMaxNoHits;
	Int_t i1 = NoIOHits[0]-1;
	Int_t ii = -1;
	if (io) {i1 = NoIOHits[0]; ii = 1;}
	for (i = i1, k = 0; i >= 0 && i < N; i += ii, k++) {
	  Double_t *xyz = &tpcHits[idx[i]].x;
	  vHelices[io].Add(xyz[0],xyz[1],xyz[2]);
	  if (tpcHits[idx[i]].err2xy == 0 || tpcHits[idx[i]].err2z == 0) {
	    if (! io) StiTpcInnerHitErrorCalculator::instance()->calculateError(200-xyz[2], 0., 0., err2xy[0], err2z);
	    else      StiTpcOuterHitErrorCalculator::instance()->calculateError(200-xyz[2], 0., 0., err2xy[0], err2z);
	  } else {
	    err2xy[0] = tpcHits[idx[i]].err2xy;
	    err2z     = tpcHits[idx[i]].err2z;
	  }
	  vHelices[io].AddErr(err2xy,err2z);
	}
	vHelices[io].Fit();
	*HlxPars[io] = vHelices[io];
	vHelices[io].MakeErrs();
	vHelices[io].Backward();
	step = vHelices[io].Step(stepMX, RefSurfice, 4, HlxPars[io]->xyz(), HlxPars[io]->pxyz());
	if (step >= stepMX) goto FAILED;;
	if (Debug()%10 > 5) {
	  if (! io) cout << "In:\t";
	  else      cout << "Out:\t";
	  const THelixFitter &p = *&vHelices[io];
	  p.Print("");
	  PrPP(Make, *HlxPars[io]);
	}
	vHelices[io].Move(step);
	if (step < 0) {HlxPars[io]->nx *= -1; HlxPars[io]->ny *= -1; HlxPars[io]->nz *= -1;}
	HlxPars[io]->Rho = vHelices[io].GetRho();
	HlxPars[io]->dRho = vHelices[io].GetDRho();
	vHelices[io].StiEmx(StiErr);
	TRSymMatrix StiMtx(6,StiErr);// PrPP(Make,StiMtx);
	TRMatrix &S2R = GetSti2R(HlxPars[io]->nx, HlxPars[io]->ny, HlxPars[io]->nz);
	TRSymMatrix Cov2(S2R,TRArray::kATxSxA,StiMtx);//  PrPP(Make,Cov2);
	memcpy(HlxPars[io]->fCov, Cov2.GetArray(), 15*sizeof(Double_t));
      } // end loop over helices
      if (TpcInOutTree)	TpcInOutTree->Fill();
    FAILED:
      count++;
    }
  }
#endif /* __IN2OUT__ */
  // Sector to Sector Alignment
  TList SegmentList; SegmentList.SetOwner(kTRUE);
  for (UInt_t i=0; i < nTracks; i++) {
    node = trackNode[i]; 
    if (!node) continue;
    StGlobalTrack  *gTrack = static_cast<StGlobalTrack *>(node->track(global));
    if (! gTrack ) continue;
    if (gTrack->flag() < 0) continue;
    if (! gTrack->detectorInfo()) continue;
    if (gTrack->TestBit(kRejected)) continue;
    if (TriggerId && btofcol &&
	! gTrack->TestBit(kComingFromOutSide) &&
	! gTrack->TestBit(kComingFromInSideTofMatched) &&
	! gTrack->TestBit(kNoToFDependece)) continue;
    const StThreeVectorF &pxyz = gTrack->geometry()->momentum();
    Double_t pMomentum = pxyz.mag();
    if (TMath::Abs(bField) < 0.01) pMomentum = 1000;
    if (pMomentum < 1.0) continue;
    StPtrVecHit hvec   = gTrack->detectorInfo()->hits();
    StTpcHit *tpcHit = 0;
    if (hvec.size()){
      //      Int_t Id = gTrack->key();
      if (gTrack->fitTraits().numberOfFitPoints(kTpcId) < 10) continue;
      TList HitList;
      PrPP(track,i);
      for (UInt_t j=0; j<hvec.size(); j++) {// hit loop
	if (! hvec[j]) continue;
	if (hvec[j]->detector() != kTpcId) continue;
	tpcHit = (StTpcHit *) hvec[j];
	if (_debug) {
	  PrPP(hit,*tpcHit);
	}
	HitList.Add(tpcHit);
      }	
      HitList.Sort();
      TIter next(&HitList);
      Int_t oldSect = -1;
      SectorSegment *ssegm = 0;
      while ((tpcHit = (StTpcHit *) next())) {
	Int_t sector = tpcHit->sector();
	if (!ssegm || sector != oldSect) {
	  ssegm = new SectorSegment(sector);
	  SegmentList.Add(ssegm);
	  oldSect = sector;
	}
	ssegm->List()->Add(tpcHit); 
      }
    }
  }
  if (SegmentList.GetSize() < 2) return kStOK;
  // Fit Segments
  SegmentList.Sort();
  TIter nextSegment(&SegmentList);
  SectorSegment *ssegm = 0;
  while ((ssegm = (SectorSegment *) nextSegment())) {
    TIter nextHit(ssegm->List());
    StTpcHit *tpcHit = 0;
    Int_t nh = 0;
    //    ssegm->Helix() = THelixFitter();
    while ((tpcHit = (StTpcHit *) nextHit())) {
      Int_t sector = tpcHit->sector();
      Int_t row    = tpcHit->padrow();
      StGlobalCoordinate global(tpcHit->position());
      //	transform(global,local,sector,row);
      StThreeVectorD xyz;
      StTpcDb::instance()->SupS2Glob(sector).MasterToLocal(global.position().xyz(), xyz.xyz());
      local = StTpcLocalSectorCoordinate(xyz,sector,row);
      ssegm->Helix().Add(xyz[0],xyz[1],xyz[2]);
      if (tpcHit->positionError().perp2() == 0 || tpcHit->positionError().z() == 0) {
	if (row <= 13) StiTpcInnerHitErrorCalculator::instance()->calculateError(200-xyz[2], 0., 0., err2xy[0], err2z);
	else           StiTpcOuterHitErrorCalculator::instance()->calculateError(200-xyz[2], 0., 0., err2xy[0], err2z);
      } else {
	err2xy[0] = tpcHit->positionError().perp2();
	err2z     = tpcHit->positionError().z()*tpcHit->positionError().z();
      }
      ssegm->Helix().AddErr(err2xy,err2z);
      if (Debug()%10 > 5) {
	cout << nh << "\t" << *tpcHit 
	     << " loc " << xyz 
	     << " err xy/z " << TMath::Sqrt(err2xy[0]) << " " << TMath::Sqrt(err2z) << endl;
      }
      if (row > ssegm->fRowMax) {ssegm->fRowMax = row; ssegm->fXmax = xyz.x();}
      if (row < ssegm->fRowMin) {ssegm->fRowMin = row; ssegm->fXmin = xyz.x();}
      nh++;
    }
    if (nh < 5) continue;
    ssegm->Helix().Fit();
    ssegm->Helix().MakeErrs();
    ssegm->SetStatus(0);
    //    ssegm->Helix().Backward();
    if (Debug()%10 > 0) ssegm->Print();
    Double_t RefXmax[2][4] = {
      {-ssegm->fXmax, 1, 0, 0},
      {-ssegm->fXmin, 1, 0, 0}
    };
    HelixPar_t *HlxPars[2] = {&ssegm->HelixSmax, &ssegm->HelixSmin};
    for (Int_t io = 0; io < 2; io++) {
      THelixFitter helix =  ssegm->Helix();
      //      if (io) helix.Backward();
      *HlxPars[io] = helix;
      step = helix.Step(stepMX, RefXmax[io], 4, HlxPars[io]->xyz(), HlxPars[io]->pxyz(),1);
      if (TMath::Abs(step) >= stepMX) continue;
      helix.Move(step);
      if (step < 0) {HlxPars[io]->nx *= -1; HlxPars[io]->ny *= -1; HlxPars[io]->nz *= -1;}
      HlxPars[io]->Rho = helix.GetRho();
      HlxPars[io]->dRho = helix.GetDRho();
      helix.StiEmx(StiErr);
      TRSymMatrix StiMtx(6,StiErr); // PrPP(Make,StiMtx);
      TRMatrix &S2R = GetSti2R(HlxPars[io]->nx, HlxPars[io]->ny, HlxPars[io]->nz);
      TRSymMatrix Cov2(S2R,TRArray::kATxSxA,StiMtx); PrPP(Make,Cov2);
      memcpy(HlxPars[io]->fCov, Cov2.GetArray(), 15*sizeof(Double_t));
      HlxPars[io]->sector = ssegm->Sector();
      if (Debug()%10 > 5) {
	if (io) cout << "In:\t";
	else    cout << "Out:\t";
	PrPP(Make,*HlxPars[io]);
      }
    }
    //      if (StTpcW2SMatch)	StTpcW2SMatch->Fill();
    if (Debug()%10 > 0) ssegm->Print();
  }
  Int_t NoSegm = SegmentList.GetSize();
  Double_t surf[4];
  if (NoSegm > 1) {
    for (Int_t k = 0; k < NoSegm; k++) {
      if (! SegmentList.At(k)) continue;
      SectorSegment *ssegW = (SectorSegment *)  SegmentList.At(k);
      if (ssegW->Status()) continue;
      Int_t sectorW = ssegW->Sector();
      Int_t RowsW[2] = {ssegW->fRowMin, ssegW->fRowMax};
      for (Int_t l = 0; l < NoSegm; l++) {
	if (l == k) continue;
	if (! SegmentList.At(l)) continue;
	SectorSegment *ssegS = (SectorSegment *)  SegmentList.At(l);
	if (ssegS->Status()) continue;
	Int_t sectorS = ssegS->Sector();
	if (sectorW == sectorS) continue;
	DEBUG_LEVEL {
	  cout << "ssegW\t"; ssegW->Print();
	  cout << "ssegS\t"; ssegS->Print();
	}
	Int_t RowsS[2] = {ssegS->fRowMin, ssegS->fRowMax};
	DEBUG_LEVEL {
	  cout << "sectorW " << sectorW << "\tRowsW [" << RowsW[0] << "," << RowsW[1] << "]" 
	       << "\tsectorS " << sectorS << "\tRowsS [" << RowsS[0] << "," << RowsS[1] << "]" << endl;
	  if (_debug) {
	    cout << "W: " << sectorW; StTpcDb::instance()->SupS2Tpc(sectorW).Print();
	    cout << "S: " << sectorS; StTpcDb::instance()->SupS2Tpc(sectorS).Print();
	    cout << "SI: " << sectorS; StTpcDb::instance()->SupS2Tpc(sectorS).Inverse().Print();
	  }
	}
	TGeoHMatrix RW2S = StTpcDb::instance()->SupS2Tpc(sectorS).Inverse() * StTpcDb::instance()->SupS2Tpc(sectorW); 
	DEBUG_LEVEL {
	  cout << "RW2S\t"; RW2S.Print();
	}
	//	TGeoHMatrix RW2SI = RW2S.Inverse();
	Double_t *trans = RW2S.GetTranslation(); //RW2SI.GetTranslation();
	Double_t *rot   = RW2S.GetRotationMatrix(); //RW2SI.GetRotationMatrix();
	surf[1] = rot[0];
	surf[2] = rot[1];
	surf[3] = rot[2];
	HelixPar_t *HlxParS[2] = {&ssegS->HelixSmax, &ssegS->HelixSmin};
	Double_t stepMin[2] = {stepMX, stepMX};
	HelixPar_t HlxParW[2];
	HelixPar_t HlxParU[2];
	for (Int_t ioS = 0; ioS < 2; ioS++) {// two predictions
	  if (! HlxParS[ioS]->sector) continue;
	  surf[0] = - HlxParS[ioS]->x + trans[0];
	  THelixFitter helix =  ssegW->Helix();
	  HlxParW[ioS] = helix;
	  HlxParW[ioS].sector = ssegW->Sector();
	  stepMin[ioS] = helix.Step(stepMX, surf, 4, HlxParW[ioS].xyz(), HlxParW[ioS].pxyz(),1); 
	  if (TMath::Abs(stepMin[ioS]) >= stepMX) continue;
	  helix.StiEmx(StiErr);
	  TRSymMatrix StiMtx(6,StiErr);// PrPP(Make,StiMtx);
	  TRMatrix &S2R = GetSti2R(HlxParW[ioS].nx, HlxParW[ioS].ny, HlxParW[ioS].nz);
	  TRSymMatrix Cov2(S2R,TRArray::kATxSxA,StiMtx);//  PrPP(Make,Cov2);
	  memcpy(HlxParW[ioS].fCov, Cov2.GetArray(), 15*sizeof(Double_t));
	  
	  HlxParW[ioS].Rho = helix.GetRho();
	  HlxParW[ioS].dRho = helix.GetDRho();
	  PrPP(Make,HlxParW[ioS]);
	  const Char_t * names[6] = {"x","y","z","nx","ny","nz"};
	  Double_t *s = HlxParS[ioS]->xyz();
	  HlxParU[ioS] = HlxParW[ioS];
	  Double_t *w = HlxParU[ioS].xyz();
	  RW2S.LocalToMaster(HlxParW[ioS].xyz(),&w[0]);
	  RW2S.LocalToMasterVect(HlxParW[ioS].pxyz(),&w[3]);
	  Double_t norm = w[3]*HlxParS[ioS]->nx + w[4]*HlxParS[ioS]->ny + w[5]*HlxParS[ioS]->nz;
	  if (norm < 0) {
	    TCL::vscale(HlxParW[ioS].pxyz(),-1.,HlxParW[ioS].pxyz(),3);
	    //	    TCL::vscale(HlxParU[ioS].pxyz(),-1.,HlxParU[ioS].pxyz(),3);
	    RW2S.LocalToMasterVect(HlxParW[ioS].pxyz(),&w[3]);
	  }
	  DEBUG_LEVEL {
	    cout << "Matching segment " << ioS << " from sectorW " << sectorW << " to sectorS " << sectorS
		 << " at step = " << stepMin[ioS] << endl;
	    cout << "HlxParS[" << ioS << "]\t" << *HlxParS[ioS] << endl;
	    cout << "HlxParW[" << ioS << "]\t" << HlxParW[ioS] << endl;
	    cout << "HlxParU[" << ioS << "]\t" << HlxParU[ioS] << endl;
	  }
	  Int_t reject = 0;
	  for (Int_t i = 0; i < 6; i++) {
	    DEBUG_LEVEL {
	      cout << names[i] << "\t" << s[i] << "\t" << w[i] << "\tDelta " << s[i] - w[i] << endl;
	    }
	    if (i < 3 && TMath::Abs(s[i] - w[i]) > 10) reject++;
	  }
	  DEBUG_LEVEL {
	    if (reject) cout << "rejected" << endl;
	  }
	  if (reject) stepMin[ioS]  = stepMX;
	}
	Int_t bestSegm = -1;
	if   (TMath::Abs(stepMin[0]) < stepMX && TMath::Abs(stepMin[0]) <  TMath::Abs(stepMin[1])) bestSegm = 0;
	else 
	  if (TMath::Abs(stepMin[1]) < stepMX && TMath::Abs(stepMin[0]) >= TMath::Abs(stepMin[1])) bestSegm = 1;
	if (bestSegm < 0) continue;
	fTpcW2SMatch->Clear();
	fTpcW2SMatch->TriggerId = TriggerId;
	fTpcW2SMatch->RW2S   =  RW2S;
	fTpcW2SMatch->HelixW =  HlxParW[bestSegm];
	fTpcW2SMatch->HelixU =  HlxParU[bestSegm];
	fTpcW2SMatch->HelixS = *HlxParS[bestSegm];
	TpcW2STree->Fill();
      }
    }
  }
#if 0
  if (TpcInOutTree) TpcInOutTree->AutoSave("SaveSelf");
  if (TpcW2STree)   TpcW2STree->AutoSave("SaveSelf");
#endif
  return kStOK;
}
//________________________________________________________________________________
TRMatrix &StTpcAlignerMaker::GetSti2R(Double_t nx, Double_t ny, Double_t nz) {
  static TRMatrix S2R;
  Double_t dxy  = TMath::Sqrt(nx*nx + ny*ny);
  Double_t tanL = nz/dxy;
  Double_t l2   = TMath::Sqrt(1+tanL*tanL);
  Double_t l1l = tanL*l2;
  Double_t Sti2R[30] = {
    /*          y, z      nx,     ny,  nz,*/
    /* x    */  0, 0,      0,      0,   0,  
    /* y    */  1, 0,      0,      0,   0,  
    /* z    */  0, 1,      0,      0,   0,  
    /* eta  */  0, 0,    -ny,     nx,   0,
    /* curv */  0, 0,      0,      0,   0,  
    /* tanL */  0, 0, nx*l1l, ny*l1l, -l2};
  S2R = TRMatrix(6,5,Sti2R);
  return *&S2R;
}
