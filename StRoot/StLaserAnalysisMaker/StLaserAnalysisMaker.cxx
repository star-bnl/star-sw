//*-- Author :Y.Fisyak 01/26/07
// 
// $Id: StLaserAnalysisMaker.cxx,v 1.10 2007/03/06 16:31:55 fisyak Exp $
// $Log: StLaserAnalysisMaker.cxx,v $
// Revision 1.10  2007/03/06 16:31:55  fisyak
// Before Selection of good runs
//
#include "StBFChain.h"
#include "StEventTypes.h"
#include "StLaserAnalysisMaker.h"
#include "LaserBeams.h"
#include "LaserEvent.h"
#include "StTpcDb/StTpcDb.h"
#include "StDetectorDbMaker/StDetectorDbClock.h"
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
#ifndef BillLove
  struct SectorCorr_t {
    Int_t sector2;
    Double_t phi;
    Double_t dphi;
    Double_t sigma;
    Double_t dsigma;
  };
  SectorCorr_t SectorCorr0[12] = {
    // Mirror 4
    {2,     -0.0186195,     1.4527e-05,     0.00184551,     1.02723e-05}, 
    {4,     -0.0196896,     2.33502e-05,    0.00184777,     1.65113e-05},
    {6,     -0.005116,      2.93135e-05,    0.00260989,     2.07281e-05},
    {8,     0.00437859,     2.77778e-05,    0.00329948,     1.96424e-05},
    {10,    0,      0,      0,      0},
    {12,    0.000916182,    2.43739e-05,    0.00234977,     1.72353e-05},
    {14,    -0.0180773,     2.52572e-05,    0.00275963,     1.78601e-05},
    {16,    0.00312329,     1.55691e-05,    0.00204276,     1.10091e-05},
    {18,    -0.00609319,    1.41944e-05,    0.00136081,     1.00371e-05},
    {20,    0.00649718,     2.39854e-05,    0.00278396,     1.69605e-05},
    {22,    0.0201383,      2.10855e-05,    0.00128673,     1.49099e-05},
    {24,    0.0374981,      4.71165e-05,    0.00324044,     3.33172e-05}
  };
#endif
  // average Z for membrane = -3.6 cm
  memset(Lasers, 0, NS*NB*NM*sizeof(LaserB *));
  const TGeoHMatrix &Tpc2Global = gStTpcDb->Tpc2GlobalMatrix();
  LaserB *theLaser = 0;
  for (Int_t i = 0; i < NoBeams; i++) {
    Int_t s = LaserBeams[i].Sector/2 - 1;
    if (s < 0 || s >= NS) continue;
    Int_t b = LaserBeams[i].Bundle - 1;
    if (b < 0 || b >= NB) continue;
    Int_t m = LaserBeams[i].Mirror - 1;
    if (m < 0 || m >= NM) continue;
    theLaser = new LaserB(LaserBeams[i]);
    Lasers[s][b][m] = theLaser;
    Tpc2Global.LocalToMaster(theLaser->XyzL.xyz(),theLaser->XyzG.xyz());
    Tpc2Global.LocalToMasterVect(theLaser->DirL.xyz(),theLaser->DirG.xyz());
    theLaser->IsValid = 0;
    if (theLaser->Sector ==  2 && theLaser->Bundle == 3) continue;
    if (theLaser->Sector ==  4 && theLaser->Bundle == 3) continue;
    if (theLaser->Sector == 10) continue;
    if (theLaser->Sector == 16 && theLaser->Bundle == 2) continue;
    if (theLaser->Sector == 18 && theLaser->Bundle == 3) continue;
    if (theLaser->Sector == 22 && (theLaser->Bundle == 2 || theLaser->Bundle == 5)) continue;
    theLaser->IsValid = 1;
  }
   for (Int_t s = 0; s < NS; s++) 
    for (Int_t b = 0; b < NB; b++) {
      Double_t phi0 = SectorCorr0[s].phi;
      for (Int_t m = 0; m < 3; m++) phi0 += Lasers[s][b][m]->dPhi;
      theLaser = Lasers[s][b][3];
      Double_t phiM = TMath::ATan2(theLaser->XyzL.y(),theLaser->XyzL.x());
      Double_t Phi = phiM - phi0;
      for (Int_t m = 0; m < NM; m++) {
	theLaser = Lasers[s][b][m];
	if (! theLaser) continue;
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
  if (event) event->Clear("C");
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
  event->GetHeader()->SetDriftDistance(gStTpcDb->Dimensions()->gatingGridZ());
  event->GetHeader()->SetInnerSectorzOffset(gStTpcDb->Dimensions()->zInnerOffset());
  event->GetHeader()->SetOuterSectorzOffset(gStTpcDb->Dimensions()->zOuterOffset());
  event->GetHeader()->SettriggerTimeOffset(gStTpcDb->triggerTimeOffset());
  StDetectorDbClock* dbclock = StDetectorDbClock::instance();
  double freq = dbclock->getCurrentFrequency()/1000000.0;
  event->GetHeader()->SetOnlClock(freq);
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
    Double_t distXY, distXYmin  = 999;
    Int_t mmax = -1;
    Double_t thePath;
    StThreeVectorD Pred, Diff;
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
    if (bundle < 0 || dZmin > 15) goto ADDTRACK;
    // mirror
    // minimum distance in XY plane from laser spot
    distXYmin  = 999;
    for (m = 0; m < NM; m++) {
      if (! Lasers[s][bundle][m]) continue;
      thePath = helixO.pathLength(Lasers[s][bundle][m]->XyzG.x(),Lasers[s][bundle][m]->XyzG.y());
      Pred = helixO.at(thePath);
      Diff = Lasers[s][bundle][m]->XyzG - Pred;
      distXY = Diff.perp();
      if (distXY < distXYmin) {
	distXYmin = distXY;
	mmax = m;
      }
    }
    if (mmax < 0 || distXYmin > 0.1) goto ADDTRACK;
    theLaser = Lasers[s][bundle][mmax];
  ADDTRACK:
    event->AddTrack(sector,gTrack,theLaser);
  }  
  for (Int_t k = 0; k < 11; k++) {
    FitDV *fit = (FitDV *) (*event->Fit())[k];
    Int_t N = fit->N;
    if (N >= 2) {
      Double_t *x = &fit->xM;
      for (Int_t i = 0; i < 5; i++) x[i] /= fit->N;
      Double_t xM = fit->xM;
      Double_t yM = fit->yM;
      Double_t x2M = fit->x2M;
      Double_t y2M = fit->y2M;
      Double_t xyM = fit->xyM;
      Double32_t det = x2M - xM*xM;
      if (det > 0) {
	Double_t d = 1./det;
	Double_t slope   = (xyM - xM*yM)*d;
	Double_t offset  = (x2M*yM - xM*xyM)*d;
	Double_t chisq   = slope*slope*x2M + 2*slope*offset*xM + offset*offset - 2*slope*xyM -2 *offset*yM + y2M; 
	Double_t dslope  = TMath::Sqrt(d);
	Double_t doffset = TMath::Sqrt(x2M*d);
        fit->slope   = slope;
	fit->offset  = offset;
	fit->chisq   = chisq;
	fit->dslope  = dslope;
	fit->doffset = doffset;
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
