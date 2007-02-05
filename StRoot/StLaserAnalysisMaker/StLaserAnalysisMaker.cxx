//*-- Author :Y.Fisyak 01/26/07
// 
// $Id: StLaserAnalysisMaker.cxx,v 1.9 2007/02/05 15:30:06 fisyak Exp $
// $Log: StLaserAnalysisMaker.cxx,v $
// Revision 1.9  2007/02/05 15:30:06  fisyak
// Freeze a version for laser drift velocity calations
//
#include "StBFChain.h"
#include "StEventTypes.h"
#include "StLaserAnalysisMaker.h"
#include "LaserBeams.h"
#include "LaserEvent.h"
#include "StTpcDb/StTpcDb.h"
ClassImp(StLaserAnalysisMaker);
static  LaserEvent             *event = 0;  //! Laser Event object
static  const Int_t NS = 12;
static  const Int_t NB =  6;
static  const Int_t NM =  7;
static LaserB *Lasers[12][6][7];
//_____________________________________________________________________________
Int_t StLaserAnalysisMaker::Init(){
  event = new LaserEvent();
  StBFChain *chain = dynamic_cast<StBFChain*>(GetChain());
  TFile *f = 0;
  if (chain) {
    f = chain->GetTFile();
    if (f)     {
      f->cd();
      m_laser = new TTree("laser","Tpc laser track tree");
      m_laser->SetAutoSave(100000000); //Save every 100 MB
      Int_t bufsize= 64000;
      Int_t split = 99;
      if (split)  bufsize /= 4;
      m_laser->Branch("event", "LaserEvent",&event, bufsize, split);
    }
  }
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StLaserAnalysisMaker::InitRun(Int_t run){
  // average Z for membrane = -3.6 cm
  memset(Lasers, 0, NS*NB*NM*sizeof(LaserB *));
  const TGeoHMatrix &Tpc2Global = gStTpcDb->Tpc2GlobalMatrix();
  for (Int_t i = 0; i < NoBeams; i++) {
    Int_t s = LaserBeams[i].Sector/2 - 1;
    if (s < 0 || s >= NS) continue;
    Int_t b = LaserBeams[i].Bundle - 1;
    if (b < 0 || b >= NB) continue;
    Int_t m = LaserBeams[i].Mirror - 1;
    if (m < 0 || m >= NM) continue;
    Lasers[s][b][m] = new LaserB(LaserBeams[i]);
    Tpc2Global.LocalToMaster(Lasers[s][b][m]->XyzL.xyz(),Lasers[s][b][m]->XyzG.xyz());
    Tpc2Global.LocalToMasterVect(Lasers[s][b][m]->DirL.xyz(),Lasers[s][b][m]->DirG.xyz());
  }
#if 0
  struct RaftCorr_t {
    Int_t raft;
    Double_t phi;
    Double_t dphi;
  };
  RaftCorr_t RaftCorr[] = {
    {1,-0.0180513,8.19979e-05},
    {2,0.00309707,5.31293e-05},
    {3,-0.00618811,4.50876e-05},
    {4,0.00703574,8.15936e-05},
    {5,0,0},
    {6,0.0384866,0.000124806},
    {7,-0.0186755,5.16188e-05},
    {8,-0.0193609,9.0548e-05},
    {9,-0.00499205,0.000102081},
    {10,0.00408203,0.000100415},
    {11,0,0},
    {12,0,0},
    {13,0.000907182,9.67858e-05},
    {14,0.0208145,7.85322e-05}
  };
#endif
   for (Int_t s = 0; s < NS; s++) 
    for (Int_t b = 0; b < NB; b++) {
      //      Int_t raft = Lasers[s][0][0]->Raft;
#if 0
      Double_t phi0 = RaftCorr[raft-1].phi;
#else
      Double_t phi0 = 0;
#endif
      for (Int_t m = 0; m < 3; m++) phi0 += Lasers[s][b][m]->dPhi;
      LaserB *theLaser = Lasers[s][b][3];
      Double_t phiM = TMath::ATan2(theLaser->XyzL.y(),theLaser->XyzL.x());
      Double_t Phi = phiM - phi0;
      for (Int_t m = 0; m < NM; m++) {
	theLaser = Lasers[s][b][m];
	theLaser->Phi =  Phi;
	Phi += theLaser->dPhi;
	theLaser->DirL = 
	  StThreeVectorD(TMath::Cos(theLaser->Phi)*TMath::Cos(theLaser->Theta),
			 TMath::Sin(theLaser->Phi)*TMath::Cos(theLaser->Theta),
			 TMath::Sin(theLaser->Theta));
	Tpc2Global.LocalToMaster(theLaser->XyzL.xyz(),theLaser->XyzG.xyz());
	Tpc2Global.LocalToMasterVect(theLaser->DirL.xyz(),theLaser->DirG.xyz());
	theLaser->PhiG = theLaser->DirG.phi();
	theLaser->ThetaG = theLaser->DirG.theta();
      }
    }
  return kStOk;
}
//_____________________________________________________________________________
void StLaserAnalysisMaker::Clear(const Option_t *option){
  if (event) event->Clear(option);
  StMaker::Clear(option);
}
//_____________________________________________________________________________
Int_t StLaserAnalysisMaker::Make(){
  StEvent* pEvent = dynamic_cast<StEvent*> (GetInputDS("StEvent"));
    // Fill the event header.
  
  StEvtHddr *EvtHddr = (StEvtHddr*)GetDataSet("EvtHddr");
  if (! EvtHddr) return kStWarn;
  event->SetHeader(EvtHddr->GetEventNumber(), EvtHddr->GetRunNumber(), EvtHddr->GetDate(), EvtHddr->GetTime(),
		   gStTpcDb->Electronics()->tZero(), gStTpcDb->DriftVelocity(), gStTpcDb->Electronics()->samplingFrequency(), 
		   EvtHddr->GetInputTriggerMask());
  
  if (! pEvent) return kStWarn;
  UInt_t nvtx = pEvent->numberOfPrimaryVertices();
  for (UInt_t i = 0; i < nvtx; i++) {
    event->AddVertex(pEvent->primaryVertex(i));
  }
  StSPtrVecTrackNode& trackNode = pEvent->trackNodes();
  UInt_t nTracks = trackNode.size();
  StTrackNode *node=0;
  for (unsigned int i=0; i < nTracks; i++) {
    node = trackNode[i]; 
    if (!node) continue;
    StGlobalTrack  *gTrack = static_cast<StGlobalTrack *>(node->track(global));
    if (! gTrack) continue;
    StTrackFitTraits&  fitTraits =  gTrack->fitTraits();
    if (fitTraits.numberOfFitPoints(kTpcId) < 15) continue;
    StThreeVectorD g3 = gTrack->outerGeometry()->momentum();
    if (g3.mag() < 10) continue;
    StThreeVectorD unit = g3.unit();
    StPhysicalHelixD helixO = gTrack->outerGeometry()->helix();
    // find a match using last hit
    StPtrVecHit hvec = gTrack->detectorInfo()->hits();
    Double_t rMax = 0;
    Int_t jmax = -1;
    LaserB *theLaser = 0;
    //    StThreeVectorD *pred = 0;
    StTpcHit *tpcHit;
    Int_t sector, s = -1;
    Int_t bundle = -1;
    Double_t dZ, dZmin  = 9999;
    Int_t b, m;
    Double_t prod, prodMax = 0;
    Int_t mmax = -1;
    //    Double_t thePath;
    StThreeVectorD Pred;
    for (unsigned int j=0; j<hvec.size(); j++) {// hit loop
      if (hvec[j]->detector() != kTpcId) continue;
      tpcHit = static_cast<StTpcHit *> (hvec[j]);
      if (tpcHit->position().perp() > rMax) {
	rMax = tpcHit->position().perp();
	jmax = j;
      }
    }
    if (jmax < 0 || rMax < 120) goto ADDTRACK;
    tpcHit = static_cast<StTpcHit *> (hvec[jmax]);
    // sector
    sector = tpcHit->sector();
    if (2*(sector/2) != sector) goto ADDTRACK;
    s = sector/2 - 1;
    if (s < 0 || s >= NS) goto ADDTRACK;
    // bundle
    dZmin  = 9999;
    for (b = 0; b < NB; b++) {
      if (! Lasers[s][b][0]) continue;
      dZ = TMath::Abs(tpcHit->position().z() - Lasers[s][b][0]->XyzG.z());
      if (dZ < dZmin) {bundle = b; dZmin = dZ;}
    }
    if (bundle < 0 || dZmin > 10) goto ADDTRACK;
    // mirror
     prodMax = 0;
    for (m = 0; m < NM; m++) {
      if (! Lasers[s][bundle][m]) continue;
      StThreeVectorD DirG = *&Lasers[s][bundle][m]->DirG;
      prod = unit * DirG;
      if (TMath::Abs(prod) > prodMax) {
	prodMax = TMath::Abs(prod);
	mmax = m;
      }
    }
    if (mmax < 0 || prodMax < 0.5)goto ADDTRACK;
    theLaser = Lasers[s][bundle][mmax];
  ADDTRACK:
    event->AddTrack(sector,gTrack,theLaser);
  }  
  FitDV *fit = 0;
  for (Int_t k = 0; k < 2; k++) {
    if (k == 0) fit = &event->West;
    else        fit = &event->East;
    if (fit->N > 2) {
      Double_t *x = &fit->xM;
      for (Int_t i = 0; i < 5; i++) x[i] /= fit->N;
      Double32_t det = fit->x2M - fit->xM*fit->xM;
      if (TMath::Abs(det) > 1e-7) {
	Double_t d = 1./det;
	fit->slope = (fit->xyM - fit->xM*fit->yM)*d;
	fit->offset = (fit->x2M*fit->yM - fit->xM*fit->xyM)*d;
	fit->chisq = fit->y2M - 2*fit->slope*fit->xyM + fit->slope*fit->slope*fit->yM - 2*fit->offset*fit->yM + fit->offset*fit->offset;
	fit->dslope = TMath::Sqrt(d);
	fit->doffset = TMath::Sqrt(fit->x2M*d);
      }
    }
  }
  m_laser->Fill(); //Fill the Tree
  return kStOK;
}
//________________________________________________________________________________
Int_t StLaserAnalysisMaker::Finish() {
  for (Int_t s = 0; s < NS; s++) 
    for (Int_t b = 0; b < NB; b++) 
      for (Int_t m = 0; m < NM; m++) SafeDelete(Lasers[s][b][m]);
  return StMaker::Finish();
}
